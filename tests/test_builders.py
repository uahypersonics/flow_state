"""
tests for flow state builders (from_mach_pres_temp, from_mach_altitude_atmosphere, etc)
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------

import math

import pytest

from flow_state.gas import PerfectGas
from flow_state.isentropic import stag_to_stat
from flow_state.solvers import (
    from_mach_altitude_atmosphere,
    from_mach_pres_stag_temp_stag,
    from_mach_pres_temp,
    solve,
)
from flow_state.transport import Sutherland

# --------------------------------------------------
# tests for solve() with static conditions
# --------------------------------------------------


class TestSolveStaticConditions:
    """tests for solve() with static conditions (pres, temp)"""

    def test_basic_computation(self) -> None:
        """basic flow state from pres and temp without mach"""
        air = PerfectGas.air()
        state = solve(pres=101325.0, temp=300.0, gas=air)

        # Check basic properties
        assert state.pres == 101325.0
        assert state.temp == 300.0
        assert state.gas_model == "air"

        # Check computed properties
        expected_dens = 101325.0 / (287.05 * 300.0)
        assert state.dens == pytest.approx(expected_dens, rel=1e-6)

        expected_a = math.sqrt(1.4 * 287.05 * 300.0)
        assert state.a == pytest.approx(expected_a, rel=1e-6)

    def test_with_mach_computes_velocity(self) -> None:
        """providing mach computes velocity"""
        air = PerfectGas.air()
        state = solve(pres=101325.0, temp=300.0, gas=air, mach=2.0)

        assert state.mach == 2.0
        expected_uvel = 2.0 * state.a
        assert state.uvel == pytest.approx(expected_uvel, rel=1e-6)

    def test_with_velocity_computes_mach(self) -> None:
        """providing velocity computes mach"""
        air = PerfectGas.air()
        state = solve(pres=101325.0, temp=300.0, gas=air, uvel=600.0)

        expected_mach = 600.0 / state.a
        assert state.mach == pytest.approx(expected_mach, rel=1e-6)
        assert state.uvel == pytest.approx(600.0, rel=1e-6)

    def test_with_transport_model(self) -> None:
        """transport model adds viscosity properties"""
        air = PerfectGas.air()
        sutherland = Sutherland.air()
        state = solve(pres=101325.0, temp=300.0, gas=air, transport=sutherland)

        assert state.transport_model == sutherland.name
        assert state.visc_dyn is not None
        assert state.visc_kin is not None
        assert state.visc_dyn > 0
        assert state.visc_kin > 0
        # check visc_dyn is consistent with transport model
        assert state.visc_dyn == pytest.approx(sutherland.visc_dyn(300.0), rel=1e-6)

    def test_notes_included(self) -> None:
        """notes field is preserved"""
        air = PerfectGas.air()
        state = solve(pres=101325.0, temp=300.0, gas=air, notes="Test case")
        assert state.notes == "Test case"


# --------------------------------------------------
# tests for from_mach_pres_temp
# --------------------------------------------------


class TestFromMPT:
    """tests for from_mach_pres_temp builder"""

    def test_basic_computation(self) -> None:
        """basic flow state from M, pres, and temp"""
        air = PerfectGas.air()
        state = from_mach_pres_temp(mach=2.0, pres=101325.0, temp=300.0, gas=air)

        # Check inputs
        assert state.mach == 2.0
        assert state.pres == 101325.0
        assert state.temp == 300.0
        assert state.gas_model == "air"

        # Check computed velocity
        expected_a = math.sqrt(1.4 * 287.05 * 300.0)
        expected_uvel = 2.0 * expected_a
        assert state.a == pytest.approx(expected_a, rel=1e-6)
        assert state.uvel == pytest.approx(expected_uvel, rel=1e-6)

    def test_velocity_mach_consistency(self) -> None:
        """verify uvel = M * a consistently"""
        air = PerfectGas.air()
        for mach in [0.5, 1.0, 2.0, 5.0]:
            state = from_mach_pres_temp(mach=mach, pres=101325.0, temp=300.0, gas=air)
            assert state.uvel == pytest.approx(mach * state.a, rel=1e-9)

    def test_with_transport_computes_reynolds(self) -> None:
        """unit reynolds number computed with transport model"""
        air = PerfectGas.air()
        sutherland = Sutherland.air()
        state = from_mach_pres_temp(mach=2.0, pres=101325.0, temp=300.0, gas=air, transport=sutherland)

        assert state.re1 is not None
        # re1 = dens * uvel / visc_dyn
        expected_Re = state.dens * state.uvel / state.visc_dyn
        assert state.re1 == pytest.approx(expected_Re, rel=1e-6)

    def test_provenance_recorded(self) -> None:
        """provenance records builder and inputs"""
        air = PerfectGas.air()
        state = from_mach_pres_temp(mach=2.0, pres=101325.0, temp=300.0, gas=air)

        assert state.provenance["builder"] == "from_mach_pres_temp"
        assert state.provenance["inputs"]["mach"] == 2.0
        assert state.provenance["inputs"]["pres"] == 101325.0
        assert state.provenance["inputs"]["temp"] == 300.0

    def test_nitrogen_gas(self) -> None:
        """nitrogen gas model"""
        n2 = PerfectGas.nitrogen()
        sutherland = Sutherland.nitrogen()
        state = from_mach_pres_temp(mach=2.0, pres=101325.0, temp=300.0, gas=n2, transport=sutherland)

        assert state.gas_model == "nitrogen"
        assert state.r_gas == 296.8
        # speed of sound should be slightly higher than air (higher R)
        expected_a = math.sqrt(1.4 * 296.8 * 300.0)
        assert state.a == pytest.approx(expected_a, rel=1e-6)


# --------------------------------------------------
# tests for from_mach_pres_stag_temp_stag
# --------------------------------------------------


class TestFromMachPressStagTempStag:
    """tests for from_mach_pres_stag_temp_stag builder"""

    def test_basic_computation(self) -> None:
        """build state from stagnation conditions"""
        air = PerfectGas.air()
        mach = 2.0
        pres_stag = 101325.0
        temp_stag = 300.0

        state = from_mach_pres_stag_temp_stag(
            mach=mach, pres_stag=pres_stag, temp_stag=temp_stag, gas=air
        )

        # Verify Mach is preserved
        assert state.mach == mach

        # Verify static conditions are correct via isentropic relations
        static = stag_to_stat(mach, pres_stag, temp_stag, gamma=air.gamma(temp_stag, pres_stag), r_gas=air.r_gas(temp_stag, pres_stag))
        assert state.pres == pytest.approx(static.pres, rel=1e-10)
        assert state.temp == pytest.approx(static.temp, rel=1e-10)
        assert state.dens == pytest.approx(static.dens, rel=1e-10)

    def test_velocity_computed(self) -> None:
        """velocity computed from M and a"""
        air = PerfectGas.air()
        state = from_mach_pres_stag_temp_stag(
            mach=2.0, pres_stag=101325.0, temp_stag=300.0, gas=air
        )

        expected_uvel = 2.0 * state.a
        assert state.uvel == pytest.approx(expected_uvel, rel=1e-10)

    def test_subsonic(self) -> None:
        """subsonic case (M < 1)"""
        air = PerfectGas.air()
        state = from_mach_pres_stag_temp_stag(
            mach=0.5, pres_stag=101325.0, temp_stag=300.0, gas=air
        )

        # at mach=0.5, static conditions should be close to stagnation
        assert state.temp < 300.0  # static T < stagnation T
        assert state.temp > 280.0  # but not too much lower at mach=0.5
        assert state.pres < 101325.0

    def test_hypersonic(self) -> None:
        """hypersonic case (M = 6)"""
        air = PerfectGas.air()
        state = from_mach_pres_stag_temp_stag(
            mach=6.0, pres_stag=500000.0, temp_stag=1000.0, gas=air
        )

        # at M=6, static T should be much lower than stagnation
        # T/T_stag = 1/(1 + 0.2*36) = 1/8.2 ≈ 0.122
        expected_temp_ratio = 1.0 / (1.0 + 0.2 * 36.0)
        assert state.temp == pytest.approx(1000.0 * expected_temp_ratio, rel=1e-6)

    def test_with_transport(self) -> None:
        """transport model adds viscosity"""
        air = PerfectGas.air()
        sutherland = Sutherland.air()
        state = from_mach_pres_stag_temp_stag(
            mach=2.0, pres_stag=101325.0, temp_stag=300.0, gas=air, transport=sutherland
        )

        assert state.visc_dyn is not None
        assert state.visc_kin is not None
        assert state.re1 is not None
        # viscosity should be evaluated at static temperature
        assert state.visc_dyn == pytest.approx(sutherland.visc_dyn(state.temp), rel=1e-6)

    def test_provenance_recorded(self) -> None:
        """provenance records stagnation inputs"""
        air = PerfectGas.air()
        state = from_mach_pres_stag_temp_stag(
            mach=2.0, pres_stag=101325.0, temp_stag=300.0, gas=air
        )

        assert state.provenance["builder"] == "from_mach_pres_stag_temp_stag"
        assert state.provenance["inputs"]["mach"] == 2.0
        assert state.provenance["inputs"]["pres_stag"] == 101325.0
        assert state.provenance["inputs"]["temp_stag"] == 300.0

    def test_zero_mach(self) -> None:
        """at M=0, static equals stagnation"""
        air = PerfectGas.air()
        state = from_mach_pres_stag_temp_stag(
            mach=0.0, pres_stag=101325.0, temp_stag=300.0, gas=air
        )

        assert state.pres == pytest.approx(101325.0, rel=1e-10)
        assert state.temp == pytest.approx(300.0, rel=1e-10)

    def test_with_unit_conversion(self) -> None:
        """unit conversion works correctly via solve()"""
        air = PerfectGas.air()

        # Without units (SI)
        state_si = from_mach_pres_stag_temp_stag(
            mach=6.0, pres_stag=965266.0, temp_stag=420.0, gas=air
        )

        # With units via solve() - psi and K
        state_units = solve(
            mach=6.0, pres_stag=(140.0, "psi"),
            temp_stag=420.0, gas=air, transport=None
        )

        # Results should be very close
        assert state_units.pres == pytest.approx(state_si.pres, rel=1e-3)
        assert state_units.temp == pytest.approx(state_si.temp, rel=1e-3)


# --------------------------------------------------
# tests for from_mach_altitude_atmosphere
# --------------------------------------------------


class TestFromMachAltitudeAtmosphere:
    """tests for from_mach_altitude_atmosphere builder"""

    def test_sea_level(self) -> None:
        """sea level conditions"""
        air = PerfectGas.air()
        state = from_mach_altitude_atmosphere(altitude=0, mach=0.8, gas=air)

        # sea level: T ≈ 288 K, p ≈ 101325 Pa
        assert state.temp == pytest.approx(288.19, rel=0.01)
        assert state.pres == pytest.approx(101290, rel=0.01)
        assert state.mach == 0.8

    def test_10km_altitude(self) -> None:
        """10 km altitude"""
        air = PerfectGas.air()
        state = from_mach_altitude_atmosphere(altitude=10000, mach=2.0, gas=air)

        # at 10 km: T ≈ 223 K
        expected_temp = 15.04 - 0.00649 * 10000 + 273.15
        assert state.temp == pytest.approx(expected_temp, rel=0.01)
        assert state.mach == 2.0

    def test_velocity_computed(self) -> None:
        """velocity computed correctly"""
        air = PerfectGas.air()
        state = from_mach_altitude_atmosphere(altitude=5000, mach=1.5, gas=air)

        expected_uvel = 1.5 * state.a
        assert state.uvel == pytest.approx(expected_uvel, rel=1e-10)

    def test_with_transport(self) -> None:
        """transport model adds viscosity"""
        air = PerfectGas.air()
        sutherland = Sutherland.air()
        state = from_mach_altitude_atmosphere(
            altitude=15000, mach=2.5, gas=air, transport=sutherland
        )

        assert state.visc_dyn is not None
        assert state.re1 is not None

    def test_provenance_recorded(self) -> None:
        """provenance records altitude input"""
        air = PerfectGas.air()
        state = from_mach_altitude_atmosphere(altitude=10000, mach=0.8, gas=air)

        assert state.provenance["builder"] == "from_mach_altitude_atmosphere"
        assert state.provenance["inputs"]["altitude"] == 10000
        assert state.provenance["inputs"]["mach"] == 0.8

    def test_negative_altitude_raises(self) -> None:
        """negative altitude should raise ValueError"""
        air = PerfectGas.air()
        with pytest.raises(ValueError):
            from_mach_altitude_atmosphere(altitude=-100, mach=0.8, gas=air)

    def test_high_altitude_stratosphere(self) -> None:
        """upper stratosphere (30 km)"""
        air = PerfectGas.air()
        state = from_mach_altitude_atmosphere(altitude=30000, mach=3.0, gas=air)

        # temperature should be warming again in upper stratosphere
        assert state.temp > 216.0  # above isothermal layer temp
        assert state.mach == 3.0

