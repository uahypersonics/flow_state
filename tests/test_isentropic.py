"""
tests for isentropic relations module
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------

import pytest

from flow_state.isentropic import (
    density_ratio,
    pressure_ratio,
    stag_to_stat,
    stat_to_stag,
    temperature_ratio,
)

# --------------------------------------------------
# tests for stagnation/static conversions
# --------------------------------------------------


class TestStagnationStaticConversion:
    """test stagnation-to-static and static-to-stagnation conversions"""

    def test_stag_to_stat_subsonic(self):
        """isentropic conversion at M=0.5"""
        M = 0.5
        pres_stag = 101325.0
        temp_stag = 300.0
        gamma = 1.4

        static = stag_to_stat(M, pres_stag, temp_stag, gamma)

        # manual calculation
        factor = 1.0 + 0.5 * (gamma - 1.0) * M * M
        expected_temp = temp_stag / factor
        expected_pres = pres_stag * factor ** (-gamma / (gamma - 1.0))

        assert static.temp == pytest.approx(expected_temp, rel=1e-10)
        assert static.pres == pytest.approx(expected_pres, rel=1e-10)

    def test_stag_to_stat_sonic(self):
        """isentropic conversion at M=1.0 (critical conditions)"""
        M = 1.0
        pres_stag = 101325.0
        temp_stag = 300.0
        gamma = 1.4

        static = stag_to_stat(M, pres_stag, temp_stag, gamma)

        # at M=1: T/T_stag = 2/(gamma+1) = 0.8333...
        # at M=1: p/p_stag = (2/(gamma+1))^(gamma/(gamma-1)) = 0.5283...
        expected_temp_ratio = 2.0 / (gamma + 1.0)
        expected_pres_ratio = expected_temp_ratio ** (gamma / (gamma - 1.0))

        assert static.temp == pytest.approx(temp_stag * expected_temp_ratio, rel=1e-10)
        assert static.pres == pytest.approx(pres_stag * expected_pres_ratio, rel=1e-10)

    def test_stag_to_stat_supersonic(self):
        """isentropic conversion at M=2.0"""
        M = 2.0
        pres_stag = 101325.0
        temp_stag = 300.0
        gamma = 1.4

        static = stag_to_stat(M, pres_stag, temp_stag, gamma)

        # at M=2, gamma=1.4: T/T_stag = 1/1.8 = 0.5556
        1.0 + 0.5 * 0.4 * 4.0  # = 1.8
        expected_temp = 300.0 / 1.8

        assert static.temp == pytest.approx(expected_temp, rel=1e-10)

    def test_stat_to_stag_roundtrip(self):
        """stat_to_stag inverts stag_to_stat"""
        M = 2.5
        pres_stag_orig = 500000.0
        temp_stag_orig = 400.0
        gamma = 1.4
        r_gas = 287.05

        # convert stagnation -> static
        static = stag_to_stat(M, pres_stag_orig, temp_stag_orig, gamma, r_gas)

        # convert static -> stagnation
        stag = stat_to_stag(M, static.pres, static.temp, gamma, r_gas)

        assert stag.pres == pytest.approx(pres_stag_orig, rel=1e-10)
        assert stag.temp == pytest.approx(temp_stag_orig, rel=1e-10)

    def test_stat_to_stag_subsonic(self):
        """static-to-stagnation at M=0.8"""
        M = 0.8
        pres = 80000.0
        temp = 250.0
        gamma = 1.4

        stag = stat_to_stag(M, pres, temp, gamma)

        # manual calculation
        factor = 1.0 + 0.5 * (gamma - 1.0) * M * M
        expected_temp_stag = temp * factor
        expected_pres_stag = pres * factor ** (gamma / (gamma - 1.0))

        assert stag.temp == pytest.approx(expected_temp_stag, rel=1e-10)
        assert stag.pres == pytest.approx(expected_pres_stag, rel=1e-10)

    def test_zero_mach(self):
        """at M=0, stagnation equals static"""
        M = 0.0
        pres_stag = 101325.0
        temp_stag = 288.15

        static = stag_to_stat(M, pres_stag, temp_stag)

        assert static.pres == pytest.approx(pres_stag, rel=1e-10)
        assert static.temp == pytest.approx(temp_stag, rel=1e-10)

    def test_density_computed_correctly(self):
        """density computed from ideal gas law"""
        M = 1.5
        pres_stag = 200000.0
        temp_stag = 350.0
        r_gas = 287.05

        static = stag_to_stat(M, pres_stag, temp_stag, r_gas=r_gas)
        expected_dens = static.pres / (r_gas * static.temp)

        assert static.dens == pytest.approx(expected_dens, rel=1e-10)


# --------------------------------------------------
# tests for isentropic ratio functions
# --------------------------------------------------


class TestIsentropicRatios:
    """test individual ratio functions"""

    def test_pressure_ratio_m2(self):
        """pressure ratio at M=2, gamma=1.4"""
        ratio = pressure_ratio(2.0, 1.4)
        # p/p_stag = (1 + 0.2*4)^(-3.5) = 1.8^(-3.5)
        expected = 1.8 ** (-3.5)
        assert ratio == pytest.approx(expected, rel=1e-10)

    def test_temperature_ratio_m2(self):
        """temperature ratio at M=2, gamma=1.4"""
        ratio = temperature_ratio(2.0, 1.4)
        # T/T_stag = 1/1.8
        expected = 1.0 / 1.8
        assert ratio == pytest.approx(expected, rel=1e-10)

    def test_density_ratio_m2(self):
        """density ratio at M=2, gamma=1.4"""
        ratio = density_ratio(2.0, 1.4)
        # rho/rho_stag = 1.8^(-2.5)
        expected = 1.8 ** (-2.5)
        assert ratio == pytest.approx(expected, rel=1e-10)

    def test_ratios_consistent(self):
        """p_ratio = T_ratio^(gamma/(gamma-1)) * rho_ratio is consistent"""
        M = 3.0
        gamma = 1.4

        p_ratio = pressure_ratio(M, gamma)
        T_ratio = temperature_ratio(M, gamma)
        rho_ratio = density_ratio(M, gamma)

        # from ideal gas: p/p_stag = (rho/rho_stag) * (T/T_stag)
        assert p_ratio == pytest.approx(rho_ratio * T_ratio, rel=1e-10)
