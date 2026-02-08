"""
tests for PerfectGas model
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------

import math

import pytest

from flow_state.gas import PerfectGas

# --------------------------------------------------
# tests for perfect gas class
# --------------------------------------------------


class TestPerfectGas:
    """test PerfectGas class"""

    def test_air_defaults(self) -> None:
        """air preset has correct values"""
        air = PerfectGas.air()
        assert air.gamma(300, 101325) == 1.4
        assert air.r_gas(300, 101325) == 287.05
        assert air.name == "air"

    def test_nitrogen_defaults(self) -> None:
        """nitrogen preset has correct values"""
        n2 = PerfectGas.nitrogen()
        assert n2.gamma(300, 101325) == 1.4
        assert n2.r_gas(300, 101325) == 296.8
        assert n2.name == "nitrogen"

    def test_custom_gas(self) -> None:
        """custom gas creation"""
        custom = PerfectGas.custom(gamma=1.3, r_gas=300.0, name="test_gas")
        assert custom.gamma(300, 101325) == 1.3
        assert custom.r_gas(300, 101325) == 300.0
        assert custom.name == "test_gas"

    def test_cp_cv_properties(self) -> None:
        """cp and cv are correctly computed"""
        air = PerfectGas.air()
        # cp = gamma * R / (gamma - 1)
        expected_cp = 1.4 * 287.05 / 0.4
        assert air.cp(300, 101325) == pytest.approx(expected_cp, rel=1e-6)
        # cv = R / (gamma - 1)
        expected_cv = 287.05 / 0.4
        assert air.cv(300, 101325) == pytest.approx(expected_cv, rel=1e-6)

    def test_sound_speed_at_known_temperature(self) -> None:
        """speed of sound at T = 300 K, air: a ≈ 347.2 m/s"""
        air = PerfectGas.air()
        a = air.sound_speed(temp=300.0)
        expected = math.sqrt(1.4 * 287.05 * 300.0)
        assert a == pytest.approx(expected, rel=1e-6)
        # sanity check: should be around 347 m/s
        assert 340 < a < 360

    def test_sound_speed_at_standard_temperature(self) -> None:
        """speed of sound at T = 288.15 K (ISA sea level), expected ≈ 340.3 m/s"""
        air = PerfectGas.air()
        a = air.sound_speed(temp=288.15)
        # typical ISA sea level speed of sound
        assert 338 < a < 342

    def test_sound_speed_negative_temperature_raises(self) -> None:
        """negative temperature raises ValueError"""
        air = PerfectGas.air()
        with pytest.raises(ValueError, match="positive"):
            air.sound_speed(temp=-100)

    def test_density_ideal_gas_law(self) -> None:
        """density via ideal gas law at p = 101325 Pa, T = 288.15 K ≈ 1.225 kg/m³"""
        air = PerfectGas.air()
        rho = air.density(pres=101325.0, temp=288.15)
        # ISA sea level density is approximately 1.225 kg/m³
        assert rho == pytest.approx(1.225, rel=0.01)

    def test_pressure_from_density_and_temperature(self) -> None:
        """pressure from rho and T"""
        air = PerfectGas.air()
        p = air.pressure(dens=1.225, temp=288.15)
        # should be close to 101325 Pa
        assert p == pytest.approx(101325, rel=0.01)

    def test_temperature_from_pressure_and_density(self) -> None:
        """temperature from p and rho"""
        air = PerfectGas.air()
        T = air.temperature(pres=101325.0, dens=1.225)
        # should be close to 288.15 K
        assert T == pytest.approx(288.15, rel=0.01)

    def test_density_negative_pressure_raises(self) -> None:
        """negative pressure raises ValueError"""
        air = PerfectGas.air()
        with pytest.raises(ValueError, match="non-negative"):
            air.density(pres=-100, temp=300)

    def test_density_zero_temperature_raises(self) -> None:
        """zero temperature raises ValueError"""
        air = PerfectGas.air()
        with pytest.raises(ValueError, match="positive"):
            air.density(pres=101325, temp=0)
