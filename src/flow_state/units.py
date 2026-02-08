"""Unit conversion utilities.

All functions convert to SI units (Pa, K, m, etc.).
"""

# --------------------------------------------------
# pressure converters
# --------------------------------------------------

# define pressure conversion factors to pascal
PSI_TO_PA = 6894.757293168
ATM_TO_PA = 101325.0
BAR_TO_PA = 100000.0
TORR_TO_PA = 133.322368

# define pressure conversion dictionary for supported units
_PRES_CONVERTERS: dict[str, float] = {
    "pa": 1.0,
    "psi": PSI_TO_PA,
    "atm": ATM_TO_PA,
    "bar": BAR_TO_PA,
    "torr": TORR_TO_PA,
}

# define pressure converter function
def convert_pressure(value: float, unit: str) -> float:
    """
    Convert pressure to pascal

    Args:
        value: Pressure value
        unit: Unit string (pa, psi, atm, bar, mbar, torr)

    Returns:
        Pressure in pascal
    """
    # get the unit in lowercase and stripped of whitespace for matching
    unit_lower = unit.lower().strip()

    # check if the unit is supported and convert, if not raise an error
    if unit_lower not in _PRES_CONVERTERS:
        raise ValueError(f"Unknown pressure unit: {unit}. "
                        f"Supported: {list(_PRES_CONVERTERS.keys())}")
    return value * _PRES_CONVERTERS[unit_lower]

# --------------------------------------------------
# temperature converters
# --------------------------------------------------

# define temperature conversion factors/offsets to kelvin
CELSIUS_TO_K = 273.15
RANKINE_TO_K = 5.0 / 9.0

# define temperature converter functions for supported units
_TEMP_CONVERTERS: dict[str, callable] = {
    "k": lambda t: t,
    "kelvin": lambda t: t,
    "c": lambda t: t + CELSIUS_TO_K,
    "celsius": lambda t: t + CELSIUS_TO_K,
    "f": lambda t: (t - 32.0) * RANKINE_TO_K + CELSIUS_TO_K,
    "fahrenheit": lambda t: (t - 32.0) * RANKINE_TO_K + CELSIUS_TO_K,
    "r": lambda t: t * RANKINE_TO_K,
    "rankine": lambda t: t * RANKINE_TO_K,
}

def convert_temperature(value: float, unit: str) -> float:
    """
    Convert temperature to Kelvin.

    Args:
        value: Temperature value
        unit: Unit string (k, kelvin, c, celsius, f, fahrenheit, r, rankine)

    Returns:
        Temperature in Kelvin
    """
    unit_lower = unit.lower().strip()
    if unit_lower not in _TEMP_CONVERTERS:
        raise ValueError(f"Unknown temperature unit: {unit}. "
                        f"Supported: {list(_TEMP_CONVERTERS.keys())}")
    return _TEMP_CONVERTERS[unit_lower](value)


# --------------------------------------------------
# length converters
# --------------------------------------------------

# define length conversion factors to meters
FEET_TO_M = 0.3048
INCH_TO_M = 0.0254
MILE_TO_M = 1609.344
KM_TO_M = 1000.0

# define length conversion dictionary for supported units
_LENGTH_CONVERTERS: dict[str, float] = {
    "m": 1.0,
    "meters": 1.0,
    "ft": FEET_TO_M,
    "feet": FEET_TO_M,
    "in": INCH_TO_M,
    "inch": INCH_TO_M,
    "inches": INCH_TO_M,
    "mi": MILE_TO_M,
    "mile": MILE_TO_M,
    "miles": MILE_TO_M,
    "km": KM_TO_M,
}

# length converter function
def convert_length(value: float, unit: str) -> float:
    """
    Convert length to meters.

    Args:
        value: Length value
        unit: Unit string (m, meters, ft, feet, in, inch, mi, mile, km)

    Returns:
        Length in meters
    """
    unit_lower = unit.lower().strip()
    if unit_lower not in _LENGTH_CONVERTERS:
        raise ValueError(f"Unknown length unit: {unit}. "
                        f"Supported: {list(_LENGTH_CONVERTERS.keys())}")
    return value * _LENGTH_CONVERTERS[unit_lower]

# --------------------------------------------------
# velocity converters
# --------------------------------------------------

# velocity conversion factors to m/s
FT_PER_S_TO_M_PER_S = FEET_TO_M
KTS_TO_M_PER_S = 0.514444
MPH_TO_M_PER_S = 0.44704
KPH_TO_M_PER_S = 1.0 / 3.6

# define velocity conversion dictionary for supported units
_VELOCITY_CONVERTERS: dict[str, float] = {
    "m/s": 1.0,
    "ft/s": FT_PER_S_TO_M_PER_S,
    "kts": KTS_TO_M_PER_S,
    "knots": KTS_TO_M_PER_S,
    "mph": MPH_TO_M_PER_S,
    "kph": KPH_TO_M_PER_S,
    "km/h": KPH_TO_M_PER_S,
}

# velocity converter function
def convert_velocity(value: float, unit: str) -> float:
    """
    Convert velocity to m/s.

    Args:
        value: Velocity value
        unit: Unit string (m/s, ft/s, kts, knots, mph, kph, km/h)

    Returns:
        Velocity in m/s
    """
    unit_lower = unit.lower().strip()
    if unit_lower not in _VELOCITY_CONVERTERS:
        raise ValueError(f"Unknown velocity unit: {unit}. "
                        f"Supported: {list(_VELOCITY_CONVERTERS.keys())}")
    return value * _VELOCITY_CONVERTERS[unit_lower]
