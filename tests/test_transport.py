"""
tests for transport models (viscosity laws)
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------

import pytest

from flow_state.transport import (
    Keyes,
    PowerLaw,
    Sutherland,
    SutherlandBlended,
    SutherlandLowTemp,
)

# --------------------------------------------------
# tests for sutherland viscosity model
# --------------------------------------------------


class TestSutherland:
    """test Sutherland viscosity model"""

    def test_air_defaults(self) -> None:
        """air preset has reasonable values"""
        air = Sutherland.air()
        assert air.mu_ref == pytest.approx(1.716e-5, rel=0.01)
        assert air.T_ref == pytest.approx(273.15, rel=0.01)
        assert air.S == pytest.approx(110.4, rel=0.01)
        assert "air" in air.name.lower()

    def test_nitrogen_defaults(self) -> None:
        """nitrogen preset has reasonable values"""
        n2 = Sutherland.nitrogen()
        assert n2.mu_ref == pytest.approx(1.663e-5, rel=0.01)
        assert n2.T_ref == pytest.approx(273.15, rel=0.01)
        assert n2.S == pytest.approx(106.7, rel=0.05)  # looser tolerance, values vary in literature
        assert "nitrogen" in n2.name.lower()

    def test_custom_model(self) -> None:
        """custom model creation"""
        custom = Sutherland.custom(
            mu_ref=1.8e-5,
            T_ref=300.0,
            S=120.0,
            name="test_gas",
        )
        assert custom.mu_ref == 1.8e-5
        assert custom.T_ref == 300.0
        assert custom.S == 120.0
        assert custom.name == "test_gas"

    def test_mu_at_reference_temperature(self) -> None:
        """mu(T_ref) returns mu_ref"""
        air = Sutherland.air()
        mu = air.visc_dyn(temp=273.15)
        # at reference temperature, should get reference viscosity
        assert mu == pytest.approx(air.mu_ref, rel=1e-6)

    def test_mu_at_300K_air(self) -> None:
        """viscosity at 300 K for air ≈ 1.85e-5 Pa·s"""
        air = Sutherland.air()
        mu = air.visc_dyn(temp=300.0)
        # order of magnitude check
        assert 1e-6 < mu < 1e-4
        # more specific check (literature: ~1.85e-5 Pa·s)
        assert mu == pytest.approx(1.85e-5, rel=0.05)

    def test_mu_at_200K_air(self) -> None:
        """viscosity at 200 K for air ≈ 1.33e-5 Pa·s"""
        air = Sutherland.air()
        mu = air.visc_dyn(temp=200.0)
        # should be positive and reasonable
        assert 1e-6 < mu < 1e-4
        # should be less than mu at 300 K
        mu_300 = air.visc_dyn(temp=300.0)
        assert mu < mu_300

    def test_mu_increases_with_temperature(self) -> None:
        """viscosity increases with temperature"""
        air = Sutherland.air()
        temps = [200, 300, 400, 500, 600]
        mus = [air.visc_dyn(T) for T in temps]
        # each mu should be greater than the previous
        for i in range(1, len(mus)):
            assert mus[i] > mus[i - 1]

    def test_mu_negative_temperature_raises(self) -> None:
        """negative temperature raises ValueError"""
        air = Sutherland.air()
        with pytest.raises(ValueError, match="positive"):
            air.visc_dyn(temp=-100)

    def test_mu_zero_temperature_raises(self) -> None:
        """zero temperature raises ValueError"""
        air = Sutherland.air()
        with pytest.raises(ValueError, match="positive"):
            air.visc_dyn(temp=0)

    def test_nu_calculation(self) -> None:
        """kinematic viscosity calculation"""
        air = Sutherland.air()
        T = 300.0
        rho = 1.177  # approximate air density at 300 K, 1 atm

        mu = air.visc_dyn(T)
        nu = air.visc_kin(T, rho)

        # nu = mu / rho
        assert nu == pytest.approx(mu / rho, rel=1e-6)

    def test_nu_negative_density_raises(self) -> None:
        """negative density raises ValueError"""
        air = Sutherland.air()
        with pytest.raises(ValueError, match="positive"):
            air.visc_kin(temp=300, dens=-1.0)


# --------------------------------------------------
# tests for low-temperature corrected sutherland
# --------------------------------------------------


class TestSutherlandLowTemp:
    """test low-temperature corrected Sutherland model"""

    def test_air_preset(self) -> None:
        """air preset creation"""
        model = SutherlandLowTemp.air()
        assert "air" in model.name.lower()

    def test_high_temp_matches_standard(self) -> None:
        """above T2, matches standard Sutherland"""
        low_temp = SutherlandLowTemp.air()
        Sutherland.air()

        # at 300 K (well above T2=110.4 K)
        T = 300.0
        mu_low = low_temp.visc_dyn(T)
        # standard Sutherland formula: c1 * T^1.5 / (T + S)
        mu_std = low_temp.c1 * T**1.5 / (T + low_temp.S)

        assert mu_low == pytest.approx(mu_std, rel=1e-6)

    def test_linear_region(self) -> None:
        """between T1 and T2, viscosity is linear"""
        model = SutherlandLowTemp.air()

        T1, T2 = 50.0, 80.0  # both in linear region
        mu1 = model.visc_dyn(T1)
        mu2 = model.visc_dyn(T2)

        # should be linear: mu = c2 * T
        assert mu1 == pytest.approx(model.c2 * T1, rel=1e-6)
        assert mu2 == pytest.approx(model.c2 * T2, rel=1e-6)

    def test_frozen_below_T1(self) -> None:
        """below T1, viscosity is constant"""
        model = SutherlandLowTemp.air()

        T_low = 20.0  # below T1=40 K
        mu = model.visc_dyn(T_low)

        # should be frozen at T1 value
        expected = model.c2 * model.T1
        assert mu == pytest.approx(expected, rel=1e-6)

    def test_continuity_at_T2(self) -> None:
        """check continuity at T2 boundary"""
        model = SutherlandLowTemp.air()

        mu_below = model.visc_dyn(model.T2 - 0.01)
        mu_above = model.visc_dyn(model.T2 + 0.01)

        # should be nearly continuous
        assert abs(mu_above - mu_below) / mu_below < 0.01


# --------------------------------------------------
# tests for blended sutherland model
# --------------------------------------------------


class TestSutherlandBlended:
    """test blended Sutherland model"""

    def test_air_preset(self) -> None:
        """air preset creation"""
        model = SutherlandBlended.air()
        assert "air" in model.name.lower()

    def test_high_temp_standard_sutherland(self) -> None:
        """above 130 K, matches standard Sutherland"""
        model = SutherlandBlended.air()

        T = 300.0
        mu = model.visc_dyn(T)
        expected = model.c1 * T**1.5 / (T + model.S)

        assert mu == pytest.approx(expected, rel=1e-6)

    def test_low_temp_linear(self) -> None:
        """below 100 K, is linear"""
        model = SutherlandBlended.air()

        T = 80.0
        mu = model.visc_dyn(T)
        expected = model.c2 * T

        assert mu == pytest.approx(expected, rel=1e-6)

    def test_blend_region_smooth(self) -> None:
        """blend region (100-130 K) is smooth"""
        model = SutherlandBlended.air()

        temps = [100, 105, 110, 115, 120, 125, 130]
        mus = [model.visc_dyn(T) for T in temps]

        # should be monotonically increasing
        for i in range(1, len(mus)):
            assert mus[i] > mus[i - 1]


# --------------------------------------------------
# tests for keyes viscosity law
# --------------------------------------------------


class TestKeyes:
    """test Keyes viscosity law"""

    def test_air_preset(self) -> None:
        """air preset has correct coefficients"""
        model = Keyes.air()
        assert model.a0 == pytest.approx(1.488e-6, rel=0.01)
        assert model.a1 == pytest.approx(122.1, rel=0.01)
        assert model.a2 == pytest.approx(5.0, rel=0.01)

    def test_nitrogen_preset(self) -> None:
        """nitrogen preset has correct coefficients"""
        model = Keyes.nitrogen()
        assert model.a0 == pytest.approx(1.418e-6, rel=0.01)
        assert model.a1 == pytest.approx(116.4, rel=0.01)
        assert model.a2 == pytest.approx(5.0, rel=0.01)

    def test_mu_increases_with_temperature(self) -> None:
        """viscosity increases with temperature"""
        model = Keyes.air()

        temps = [200, 400, 600, 800, 1000]
        mus = [model.visc_dyn(T) for T in temps]

        for i in range(1, len(mus)):
            assert mus[i] > mus[i - 1]

    def test_reasonable_values_at_300K(self) -> None:
        """reasonable viscosity at 300 K"""
        model = Keyes.air()
        mu = model.visc_dyn(300.0)

        # should be same order as Sutherland (~1.8e-5)
        assert 1e-5 < mu < 3e-5

    def test_high_temperature(self) -> None:
        """at high temperature (hypersonic regime)"""
        model = Keyes.air()
        mu = model.visc_dyn(2000.0)

        # should still be reasonable
        assert 1e-5 < mu < 1e-4


# --------------------------------------------------
# tests for power-law viscosity model
# --------------------------------------------------


class TestPowerLaw:
    """test power-law viscosity model"""

    def test_air_preset(self) -> None:
        """air preset with default exponent"""
        model = PowerLaw.air()
        assert model.m == pytest.approx(0.76, rel=0.01)

    def test_custom_exponent(self) -> None:
        """air preset with custom exponent"""
        model = PowerLaw.air(m=0.7)
        assert model.m == pytest.approx(0.7, rel=0.01)

    def test_mu_at_reference(self) -> None:
        """at T_ref, get mu_ref"""
        model = PowerLaw.air()
        mu = model.visc_dyn(model.T_ref)
        assert mu == pytest.approx(model.mu_ref, rel=1e-6)

    def test_power_law_scaling(self) -> None:
        """power-law scaling"""
        model = PowerLaw.air(m=0.75)

        T1 = 200.0
        T2 = 400.0

        mu1 = model.visc_dyn(T1)
        mu2 = model.visc_dyn(T2)

        # mu2/mu1 should equal (T2/T1)^m
        expected_ratio = (T2 / T1) ** model.m
        assert (mu2 / mu1) == pytest.approx(expected_ratio, rel=1e-6)
