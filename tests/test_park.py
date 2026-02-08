"""
tests for ParkAir high-temperature gas model
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------

import pytest

from flow_state.gas import ParkAir, PerfectGas

# --------------------------------------------------
# tests for park air class
# --------------------------------------------------


class TestParkAir:
    """test ParkAir class"""

    def test_air_defaults(self) -> None:
        """air preset has correct values"""
        air = ParkAir.air()
        assert air.name == "park_air"
        assert air.r_gas(300, 101325) == pytest.approx(287.05, rel=1e-6)

    def test_gamma_low_temp_matches_perfect_gas(self) -> None:
        """at low temperature, gamma approaches perfect gas value"""
        park = ParkAir.air()

        # at 300 K, vibrational modes are frozen, gamma ~ 1.4
        gamma_300 = park.gamma(300, 101325)
        assert gamma_300 == pytest.approx(1.4, rel=0.01)

    def test_gamma_decreases_with_temperature(self) -> None:
        """gamma decreases as temperature increases (vibrational excitation)"""
        park = ParkAir.air()

        gamma_300 = park.gamma(300, 101325)
        gamma_1000 = park.gamma(1000, 101325)
        gamma_2000 = park.gamma(2000, 101325)

        assert gamma_1000 < gamma_300
        assert gamma_2000 < gamma_1000

    def test_gamma_high_temp_reasonable(self) -> None:
        """at high temperature, gamma is in reasonable range"""
        park = ParkAir.air()

        # at 2000 K, gamma should be around 1.3 (vibration partially excited)
        gamma_2000 = park.gamma(2000, 101325)
        assert 1.25 < gamma_2000 < 1.35

    def test_cp_cv_consistent_with_gamma(self) -> None:
        """cp/cv equals gamma"""
        park = ParkAir.air()

        for temp in [300, 1000, 2000]:
            cp = park.cp(temp, 101325)
            cv = park.cv(temp, 101325)
            gamma = park.gamma(temp, 101325)
            assert cp / cv == pytest.approx(gamma, rel=1e-10)

    def test_mayer_relation(self) -> None:
        """cp - cv = R holds (Mayer's relation for ideal gas)"""
        park = ParkAir.air()

        for temp in [300, 1000, 2000]:
            cp = park.cp(temp, 101325)
            cv = park.cv(temp, 101325)
            assert cp - cv == pytest.approx(park.r_gas(temp, 101325), rel=1e-10)

    def test_sound_speed_increases_with_temp(self) -> None:
        """sound speed generally increases with temperature"""
        park = ParkAir.air()

        a_300 = park.sound_speed(300)
        a_1000 = park.sound_speed(1000)

        # sound speed ~ sqrt(gamma * R * T)
        # even though gamma decreases, T increase dominates
        assert a_1000 > a_300

    def test_sound_speed_vs_perfect_gas(self) -> None:
        """at low temp, sound speed matches perfect gas closely"""
        park = ParkAir.air()
        perfect = PerfectGas.air()

        a_park = park.sound_speed(300)
        a_perfect = perfect.sound_speed(300)

        assert a_park == pytest.approx(a_perfect, rel=0.01)

    def test_density_calculation(self) -> None:
        """density from ideal gas law"""
        park = ParkAir.air()
        pres = 101325.0
        temp = 300.0

        dens = park.density(pres, temp)
        expected = pres / (park.r_gas(temp, pres) * temp)
        assert dens == pytest.approx(expected, rel=1e-10)

    def test_pressure_calculation(self) -> None:
        """pressure from ideal gas law"""
        park = ParkAir.air()
        dens = 1.225
        temp = 300.0

        pres = park.pressure(dens, temp)
        expected = dens * park.r_gas(temp, 101325) * temp
        assert pres == pytest.approx(expected, rel=1e-10)

    def test_temperature_calculation(self) -> None:
        """temperature from ideal gas law"""
        park = ParkAir.air()
        pres = 101325.0
        dens = 1.225

        temp = park.temperature(pres, dens)
        expected = pres / (dens * park.r_gas(300, pres))
        assert temp == pytest.approx(expected, rel=1e-10)

    def test_equation_of_state_roundtrip(self) -> None:
        """p, rho, T roundtrip consistency"""
        park = ParkAir.air()
        pres = 101325.0
        temp = 1500.0

        dens = park.density(pres, temp)
        pres_back = park.pressure(dens, temp)
        temp_back = park.temperature(pres, dens)

        assert pres_back == pytest.approx(pres, rel=1e-10)
        assert temp_back == pytest.approx(temp, rel=1e-10)

    def test_negative_temp_raises(self) -> None:
        """negative temperature raises ValueError"""
        park = ParkAir.air()

        with pytest.raises(ValueError):
            park.gamma(-100, 101325)

        with pytest.raises(ValueError):
            park.sound_speed(-100)

    def test_pressure_independent(self) -> None:
        """thermodynamic properties are pressure-independent (thermally perfect)"""
        park = ParkAir.air()
        temp = 1500.0

        gamma_1atm = park.gamma(temp, 101325)
        gamma_10atm = park.gamma(temp, 1013250)

        assert gamma_1atm == gamma_10atm
