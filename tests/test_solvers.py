"""
tests for re1-based solvers (bisection search for flow conditions)
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------

import pytest

from flow_state.atmosphere import get_atmosphere_model
from flow_state.gas import PerfectGas
from flow_state.isentropic import stag_to_stat
from flow_state.solvers import (
    _compute_re1,
    from_mach_re1_atmosphere,
    from_re1_pres_mach,
    from_re1_pres_stag_temp_stag,
    from_re1_temp_mach,
)
from flow_state.transport import Sutherland

# --------------------------------------------------
# fixtures for gas and transport models
# --------------------------------------------------


@pytest.fixture
def air():
    return PerfectGas.air()


@pytest.fixture
def sutherland():
    return Sutherland.air()


# --------------------------------------------------
# tests for from_re1_temp_mach (solves for pressure)
# --------------------------------------------------


class TestFromRe1TempMach:
    """tests for from_re1_temp_mach - solves for pressure given re1, temp, mach"""

    def test_roundtrip(self, air, sutherland):
        """solve for pressure, verify re1 matches original"""
        temp = 300.0
        mach = 2.0
        pres_original = 50000.0

        re1 = _compute_re1(pres_original, temp, mach, air, sutherland)

        state = from_re1_temp_mach(re1, temp, mach, air, sutherland)

        assert state.pres == pytest.approx(pres_original, rel=1e-6)
        assert state.re1 == pytest.approx(re1, rel=1e-6)

    def test_high_re1(self, air, sutherland):
        """high unit reynolds number case"""
        re1_target = 1e8
        temp = 200.0
        mach = 3.0

        state = from_re1_temp_mach(re1_target, temp, mach, air, sutherland)

        assert state.re1 == pytest.approx(re1_target, rel=1e-6)

    def test_low_re1(self, air, sutherland):
        """low unit reynolds number (high altitude conditions)"""
        re1_target = 1e5
        temp = 220.0
        mach = 2.0

        state = from_re1_temp_mach(re1_target, temp, mach, air, sutherland)

        assert state.re1 == pytest.approx(re1_target, rel=1e-6)

    def test_subsonic(self, air, sutherland):
        """subsonic mach number case"""
        re1_target = 5e6
        temp = 288.0
        mach = 0.8

        state = from_re1_temp_mach(re1_target, temp, mach, air, sutherland)

        assert state.re1 == pytest.approx(re1_target, rel=1e-6)

    def test_provenance(self, air, sutherland):
        """verify provenance records correct builder and inputs"""
        state = from_re1_temp_mach(1e7, 300.0, 2.0, air, sutherland)

        assert state.provenance["builder"] == "from_re1_temp_mach"
        assert state.provenance["inputs"]["re1"] == 1e7
        assert state.provenance["inputs"]["temp"] == 300.0
        assert state.provenance["inputs"]["mach"] == 2.0


# --------------------------------------------------
# tests for from_re1_pres_mach (solves for temperature)
# --------------------------------------------------


class TestFromRe1PresMach:
    """tests for from_re1_pres_mach - solves for temperature given re1, pres, mach"""

    def test_roundtrip(self, air, sutherland):
        """solve for temperature, verify re1 matches original"""
        pres = 101325.0
        mach = 2.0
        temp_original = 250.0

        re1 = _compute_re1(pres, temp_original, mach, air, sutherland)

        state = from_re1_pres_mach(re1, pres, mach, air, sutherland)

        assert state.temp == pytest.approx(temp_original, rel=1e-4)

    def test_cold_conditions(self, air, sutherland):
        """cold temperature target"""
        re1_target = 2e7
        pres = 50000.0
        mach = 3.0

        state = from_re1_pres_mach(re1_target, pres, mach, air, sutherland,
                                   temp_bounds=(100.0, 500.0))

        assert state.re1 == pytest.approx(re1_target, rel=1e-4)

    def test_warm_conditions(self, air, sutherland):
        """warm temperature - use realistic re1 that's achievable"""
        pres = 101325.0
        mach = 1.5
        temp_original = 400.0

        re1 = _compute_re1(pres, temp_original, mach, air, sutherland)

        # note: re1 vs T is non-monotonic, so there may be multiple solutions
        # narrow the bounds to find the specific solution we want
        state = from_re1_pres_mach(re1, pres, mach, air, sutherland,
                                   temp_bounds=(300.0, 500.0))

        assert state.temp == pytest.approx(temp_original, rel=1e-4)


# --------------------------------------------------
# tests for from_re1_pres_stag_temp_stag (solves for mach)
# --------------------------------------------------


class TestFromRe1PresStag:
    """tests for from_re1_pres_stag_temp_stag - solves for mach

    note: re1 vs mach is non-monotonic - at low mach, re1 increases with M
    at high mach, the static pressure/temperature drop dominates and re1
    can decrease - solutions may not exist for all (re1, p_stag, T_stag) combos
    """

    def test_roundtrip(self, air, sutherland):
        """solve for mach, verify re1 matches original"""
        pres_stag = 5000000.0  # 5 MPa - high enough for solution
        temp_stag = 600.0
        mach_original = 3.0

        static = stag_to_stat(mach_original, pres_stag, temp_stag,
                              gamma=air.gamma(temp_stag, pres_stag),
                              r_gas=air.r_gas(temp_stag, pres_stag))
        re1 = _compute_re1(static.pres, static.temp, mach_original, air, sutherland)

        state = from_re1_pres_stag_temp_stag(re1, pres_stag, temp_stag, air, sutherland,
                                             mach_bounds=(0.5, 8.0))

        assert state.mach == pytest.approx(mach_original, rel=1e-3)

    def test_supersonic(self, air, sutherland):
        """supersonic mach number with realistic conditions"""
        pres_stag = 10e6  # 10 MPa
        temp_stag = 800.0
        mach_original = 4.0

        static = stag_to_stat(mach_original, pres_stag, temp_stag,
                              gamma=air.gamma(temp_stag, pres_stag),
                              r_gas=air.r_gas(temp_stag, pres_stag))
        re1_target = _compute_re1(static.pres, static.temp, mach_original, air, sutherland)

        state = from_re1_pres_stag_temp_stag(re1_target, pres_stag, temp_stag, air, sutherland,
                                             mach_bounds=(1.0, 10.0))

        assert state.mach == pytest.approx(mach_original, rel=1e-3)

    def test_hypersonic(self, air, sutherland):
        """hypersonic mach number"""
        pres_stag = 2e6
        temp_stag = 800.0
        re1_target = 1e6

        state = from_re1_pres_stag_temp_stag(re1_target, pres_stag, temp_stag, air, sutherland,
                                             mach_bounds=(0.1, 15.0))

        assert state.re1 == pytest.approx(re1_target, rel=1e-4)


# --------------------------------------------------
# tests for from_mach_re1_atmosphere (solves for altitude)
# --------------------------------------------------


class TestFromMachRe1Atmosphere:
    """tests for from_mach_re1_atmosphere - solves for altitude given mach and re1"""

    @pytest.fixture
    def atm_model(self):
        return get_atmosphere_model("ussa76")

    def test_roundtrip(self, air, sutherland, atm_model):
        """solve for altitude, verify re1 matches original"""
        mach = 2.0
        altitude_original = 15000.0

        atm = atm_model(altitude_original)
        re1 = _compute_re1(atm.pres, atm.temp, mach, air, sutherland)

        state = from_mach_re1_atmosphere(re1, mach, air, sutherland)

        # verify re1 matches
        assert state.re1 == pytest.approx(re1, rel=1e-3)

    def test_sea_level_re1(self, air, sutherland, atm_model):
        """high re1 corresponds to low altitude"""
        mach = 0.8
        re1_target = 1e7

        state = from_mach_re1_atmosphere(re1_target, mach, air, sutherland)

        assert state.re1 == pytest.approx(re1_target, rel=1e-3)

    def test_high_altitude_re1(self, air, sutherland, atm_model):
        """low re1 corresponds to high altitude"""
        mach = 3.0
        re1_target = 1e5

        state = from_mach_re1_atmosphere(re1_target, mach, air, sutherland)

        assert state.re1 == pytest.approx(re1_target, rel=1e-3)

    def test_stratosphere(self, air, sutherland, atm_model):
        """find altitude in stratosphere"""
        mach = 2.5
        altitude_target = 25000.0

        atm = atm_model(altitude_target)
        re1 = _compute_re1(atm.pres, atm.temp, mach, air, sutherland)

        state = from_mach_re1_atmosphere(re1, mach, air, sutherland)

        # check the state produces the same re1
        assert state.re1 == pytest.approx(re1, rel=1e-3)


# --------------------------------------------------
# edge cases and error handling
# --------------------------------------------------


class TestBisectionEdgeCases:
    """test edge cases and error handling for bisection solvers"""

    def test_no_solution_raises(self, air, sutherland):
        """impossible re1 target should raise error"""
        # re1 that's way too high for the pressure bounds
        with pytest.raises(ValueError, match="No sign change"):
            from_re1_temp_mach(
                re1=1e20,  # impossibly high
                temp=300.0,
                mach=2.0,
                gas=air,
                transport=sutherland,
                pres_bounds=(100, 1e6),
            )

    def test_custom_bounds(self, air, sutherland):
        """custom pressure bounds for bisection search"""
        re1_target = 5e6
        temp = 300.0
        mach = 2.0

        state = from_re1_temp_mach(
            re1_target, temp, mach, air, sutherland,
            pres_bounds=(1000, 500000)
        )

        assert state.re1 == pytest.approx(re1_target, rel=1e-6)
