"""
tests for unit conversion utilities
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------

import pytest

from flow_state.units import (
    ATM_TO_PA,
    FEET_TO_M,
    PSI_TO_PA,
    convert_length,
    convert_pressure,
    convert_temperature,
)

# --------------------------------------------------
# tests for pressure conversions
# --------------------------------------------------


class TestPressureConversions:
    """test pressure unit conversions"""

    def test_psi_to_pa(self) -> None:
        """psi to Pa conversion"""
        # 1 psi = 6894.76 Pa (approximately)
        assert convert_pressure(1.0, "psi") == pytest.approx(6894.757, rel=1e-4)
        assert convert_pressure(14.696, "psi") == pytest.approx(101325, rel=1e-3)  # ~1 atm

    def test_atm_to_pa(self) -> None:
        """atm to Pa conversion"""
        assert convert_pressure(1.0, "atm") == 101325.0

    def test_bar_to_pa(self) -> None:
        """bar to Pa conversion"""
        assert convert_pressure(1.0, "bar") == 100000.0

    def test_convert_pressure_generic(self) -> None:
        """generic pressure converter"""
        assert convert_pressure(140, "psi") == pytest.approx(140 * PSI_TO_PA)
        assert convert_pressure(1, "atm") == ATM_TO_PA
        assert convert_pressure(101325, "pa") == 101325.0

    def test_convert_pressure_case_insensitive(self) -> None:
        """unit strings are case insensitive"""
        assert convert_pressure(1, "PSI") == convert_pressure(1, "psi")
        assert convert_pressure(1, "ATM") == convert_pressure(1, "atm")

    def test_convert_pressure_unknown_unit(self) -> None:
        """unknown units raise ValueError"""
        with pytest.raises(ValueError, match="Unknown pressure unit"):
            convert_pressure(100, "invalid")


# --------------------------------------------------
# tests for temperature conversions
# --------------------------------------------------


class TestTemperatureConversions:
    """test temperature unit conversions"""

    def test_celsius_to_kelvin(self) -> None:
        """Celsius to Kelvin conversion"""
        assert convert_temperature(0, "c") == 273.15
        assert convert_temperature(100, "c") == 373.15
        assert convert_temperature(-273.15, "c") == pytest.approx(0, abs=1e-10)

    def test_fahrenheit_to_kelvin(self) -> None:
        """Fahrenheit to Kelvin conversion"""
        assert convert_temperature(32, "f") == pytest.approx(273.15, rel=1e-6)
        assert convert_temperature(212, "f") == pytest.approx(373.15, rel=1e-6)

    def test_rankine_to_kelvin(self) -> None:
        """Rankine to Kelvin conversion"""
        # 0 R = 0 K (absolute zero)
        assert convert_temperature(0, "r") == 0
        # 491.67 R = 273.15 K (freezing point)
        assert convert_temperature(491.67, "r") == pytest.approx(273.15, rel=1e-4)

    def test_convert_temperature_generic(self) -> None:
        """generic temperature converter"""
        assert convert_temperature(300, "k") == 300
        assert convert_temperature(300, "kelvin") == 300
        assert convert_temperature(0, "celsius") == 273.15
        assert convert_temperature(32, "fahrenheit") == pytest.approx(273.15, rel=1e-6)

    def test_convert_temperature_case_insensitive(self) -> None:
        """unit strings are case insensitive"""
        assert convert_temperature(100, "C") == convert_temperature(100, "c")
        assert convert_temperature(100, "F") == convert_temperature(100, "f")

    def test_convert_temperature_unknown_unit(self) -> None:
        """unknown units raise ValueError"""
        with pytest.raises(ValueError, match="Unknown temperature unit"):
            convert_temperature(100, "invalid")


# --------------------------------------------------
# tests for length conversions
# --------------------------------------------------


class TestLengthConversions:
    """test length unit conversions"""

    def test_feet_to_m(self) -> None:
        """feet to meters conversion"""
        assert convert_length(1, "ft") == FEET_TO_M
        assert convert_length(1000, "ft") == 304.8

    def test_convert_length_generic(self) -> None:
        """generic length converter"""
        assert convert_length(1, "m") == 1.0
        assert convert_length(1, "feet") == FEET_TO_M
        assert convert_length(1, "km") == 1000.0

    def test_convert_length_unknown_unit(self) -> None:
        """unknown units raise ValueError"""
        with pytest.raises(ValueError, match="Unknown length unit"):
            convert_length(100, "invalid")
