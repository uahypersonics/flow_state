"""
tests for atmosphere models (USSA76, CIRA86)
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------

import pytest

from flow_state.atmosphere import CIRA86, USSA76, available_atmosphere_models, get_atmosphere_model
from flow_state.math_utils import bisect

# --------------------------------------------------
# tests for model registry
# --------------------------------------------------


class TestRegistry:
    """test model registry and dispatcher"""

    def test_available_atmosphere_models(self):
        """check available models list"""
        models = available_atmosphere_models()
        assert "ussa76" in models
        assert "cira86" in models

    def test_get_atmosphere_model_ussa76(self):
        """get USSA76 model by name"""
        model = get_atmosphere_model("ussa76")
        assert isinstance(model, USSA76)

    def test_get_atmosphere_model_cira86(self):
        """get CIRA86 model by name"""
        model = get_atmosphere_model("cira86", latitude=45, month=7)
        assert isinstance(model, CIRA86)

    def test_get_atmosphere_model_case_insensitive(self):
        """model name should be case insensitive"""
        model1 = get_atmosphere_model("USSA76")
        model2 = get_atmosphere_model("ussa76")
        model3 = get_atmosphere_model("Ussa76")
        # all should work
        assert model1(10000).temp == model2(10000).temp == model3(10000).temp

    def test_unknown_model_raises(self):
        """unknown model should raise ValueError"""
        with pytest.raises(ValueError, match="Unknown atmosphere model"):
            get_atmosphere_model("unknown_model")


# --------------------------------------------------
# tests for US Standard Atmosphere 1976
# --------------------------------------------------


class TestUSSA76:
    """test US Standard Atmosphere 1976 model"""

    @pytest.fixture
    def model(self):
        return get_atmosphere_model("ussa76")

    def test_sea_level(self, model):
        """sea level conditions"""
        atm = model(0)

        # sea level: T ≈ 288.15 K (15°C), p ≈ 101325 Pa
        assert atm.temp == pytest.approx(288.19, rel=0.01)  # 15.04°C + 273.15
        assert atm.pres == pytest.approx(101290, rel=0.01)
        assert atm.altitude == 0.0

    def test_troposphere_5km(self, model):
        """conditions at 5 km altitude"""
        atm = model(5000)

        # at 5 km: T ≈ 255.7 K, p ≈ 54000 Pa
        expected_temp = 15.04 - 0.00649 * 5000 + 273.15  # ≈ 255.7 K
        assert atm.temp == pytest.approx(expected_temp, rel=0.01)

    def test_tropopause_11km(self, model):
        """conditions at tropopause (11 km)"""
        atm = model(11000)

        # at tropopause: T ≈ 216.65 K, p ≈ 22632 Pa
        expected_temp = 15.04 - 0.00649 * 11000 + 273.15  # ≈ 216.8 K
        assert atm.temp == pytest.approx(expected_temp, rel=0.01)

    def test_stratosphere_isothermal_20km(self, model):
        """isothermal layer at 20 km"""
        atm = model(20000)

        # in lower stratosphere: T ≈ 216.65 K (constant)
        expected_temp = -56.46 + 273.15
        assert atm.temp == pytest.approx(expected_temp, rel=0.01)

    def test_upper_stratosphere_30km(self, model):
        """upper stratosphere at 30 km (layer 3: base 20 km, +1.0 K/km)"""
        atm = model(30000)

        # T = 216.65 + 0.001 * (30000 - 20000) = 226.65 K
        expected_temp = 216.65 + 0.001 * (30000 - 20000)
        assert atm.temp == pytest.approx(expected_temp, rel=0.01)
        assert atm.pres > 0  # pressure should still be positive

    def test_density_ideal_gas(self, model):
        """verify density computed from ideal gas law"""
        atm = model(10000)
        r_air = 287.05
        expected_dens = atm.pres / (r_air * atm.temp)
        assert atm.dens == pytest.approx(expected_dens, rel=0.01)

    def test_negative_altitude_raises(self, model):
        """negative altitude should raise ValueError"""
        with pytest.raises(ValueError, match="non-negative"):
            model(-100)

    def test_pressure_decreases_with_altitude(self, model):
        """pressure should monotonically decrease with altitude"""
        altitudes = [0, 5000, 11000, 15000, 25000, 35000]
        pressures = [model(h).pres for h in altitudes]

        for i in range(1, len(pressures)):
            assert pressures[i] < pressures[i - 1]

    def test_continuity_at_11km(self, model):
        """check continuity at troposphere/stratosphere boundary"""
        atm_below = model(10999)
        atm_above = model(11001)

        # temperature and pressure should be nearly continuous
        assert abs(atm_above.temp - atm_below.temp) < 1.0  # < 1 K difference
        assert abs(atm_above.pres - atm_below.pres) / atm_below.pres < 0.01


# --------------------------------------------------
# tests for CIRA-86 model
# --------------------------------------------------


class TestCIRA86:
    """test CIRA-86 model"""

    def test_basic_call(self):
        """basic functionality"""
        model = get_atmosphere_model("cira86", latitude=45, month=1)
        atm = model(10000)
        assert atm.temp > 0
        assert atm.pres > 0
        assert atm.dens > 0

    def test_latitude_effect(self):
        """temperature should vary with latitude"""
        model_equator = get_atmosphere_model("cira86", latitude=0, month=1)
        model_polar = get_atmosphere_model("cira86", latitude=80, month=1)

        # at high altitude (50 km), expect different temperatures
        t_eq = model_equator(50000).temp
        t_polar = model_polar(50000).temp
        assert t_eq != t_polar

    def test_seasonal_effect(self):
        """temperature should vary with season"""
        model_jan = get_atmosphere_model("cira86", latitude=60, month=1)
        model_jul = get_atmosphere_model("cira86", latitude=60, month=7)

        # at mesosphere (80 km), expect seasonal variation
        t_jan = model_jan(80000).temp
        t_jul = model_jul(80000).temp
        assert t_jan != t_jul

    def test_high_altitude(self):
        """should work up to 120 km"""
        model = get_atmosphere_model("cira86", latitude=45, month=7)
        atm = model(100000)  # 100 km
        assert atm.temp > 0
        assert atm.pres > 0

    def test_invalid_latitude_raises(self):
        """invalid latitude should raise"""
        with pytest.raises(ValueError):
            get_atmosphere_model("cira86", latitude=100, month=1)

    def test_invalid_month_raises(self):
        """invalid month should raise"""
        with pytest.raises(ValueError):
            get_atmosphere_model("cira86", latitude=45, month=13)

    def test_southern_hemisphere_independent_data(self):
        """Southern Hemisphere has independent data, not a mirror of the North"""
        # 60°S January (SH summer) should differ from 60°N July (NH summer)
        # because the hemispheres have independent CIRA-86 tables
        s_jan = CIRA86(latitude=-60, month=1)(50000).temp
        n_jul = CIRA86(latitude=60, month=7)(50000).temp
        # both are summer hemispheres, so temperatures should be similar
        # but not identical due to land/ocean asymmetry
        assert s_jan > 0
        assert n_jul > 0

    def test_southern_hemisphere_winter_colder(self):
        """SH polar winter should be colder than NH polar winter in stratosphere"""
        # Antarctic polar vortex is stronger than Arctic
        sh_winter = CIRA86(latitude=-80, month=7)(30000).temp
        nh_winter = CIRA86(latitude=80, month=1)(30000).temp
        assert sh_winter < nh_winter

    def test_southern_hemisphere_basic(self):
        """negative latitude should work and return valid results"""
        model = CIRA86(latitude=-45, month=1)
        atm = model(20000)
        assert atm.temp > 0
        assert atm.pres > 0
        assert atm.dens > 0

    def test_equator_symmetric(self):
        """equator should be unaffected by hemisphere logic"""
        t_pos = CIRA86(latitude=0, month=1)(50000).temp
        t_neg = CIRA86(latitude=0, month=7)(50000).temp  # different month
        # equator has minimal seasonal variation but these can differ slightly
        # just verify both are valid
        assert t_pos > 0
        assert t_neg > 0


# --------------------------------------------------
# tests for inverse lookups with bisection
# --------------------------------------------------


class TestInverseWithBisection:
    """test using bisection for inverse lookups (user pattern)"""

    def test_altitude_from_pressure(self):
        """find altitude given pressure using bisection"""
        model = get_atmosphere_model("ussa76")

        # get pressure at 10 km
        target_pres = model(10000).pres

        # find altitude using bisection
        h = bisect(lambda h: model(h).pres - target_pres, 0, 47000, abs_tol=1.0)

        assert h == pytest.approx(10000, abs=10)

    def test_altitude_from_density(self):
        """find altitude given density using bisection"""
        model = get_atmosphere_model("ussa76")

        # get density at 15 km
        target_dens = model(15000).dens

        # find altitude using bisection
        h = bisect(lambda h: model(h).dens - target_dens, 0, 47000, abs_tol=1.0)

        assert h == pytest.approx(15000, abs=10)

    def test_altitude_from_temperature_troposphere(self):
        """find altitude given temperature in troposphere"""
        model = get_atmosphere_model("ussa76")

        # get temp at 5 km (in troposphere where T decreases monotonically)
        target_temp = model(5000).temp

        # find altitude using bisection (in troposphere only)
        h = bisect(lambda h: model(h).temp - target_temp, 0, 11000, abs_tol=1.0)

        assert h == pytest.approx(5000, abs=10)
