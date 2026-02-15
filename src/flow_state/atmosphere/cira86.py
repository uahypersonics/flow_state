"""
CIRA-86 (COSPAR International Reference Atmosphere 1986) model.

Provides atmospheric properties as a function of altitude, latitude, and month.
Extends higher than USSA76 (~120 km) and includes seasonal/latitudinal variations.

Reference:
    COSPAR International Reference Atmosphere: 1986
    Advances in Space Research, Vol. 10, No. 12, 1990
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------
from __future__ import annotations

from flow_state.atmosphere.core import AtmosphereState

# --------------------------------------------------
# CIRA-86 temperature data tables
# T(altitude, latitude, month) for both hemispheres
# Latitudes: -80° to 80° in 20° increments
# Months: January (1) and July (7) as representative extremes
# --------------------------------------------------

# Altitude breakpoints [km]
_ALT_KM = [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120]

# Temperature profiles [K] for different latitudes and seasons
# Format: _TEMP_DATA[month][latitude] = [T at each altitude in _ALT_KM]
#
# Northern Hemisphere data from CIRA-86 reference tables.
# Southern Hemisphere data from CIRA-86 reference tables — NOT a simple
# mirror of the North due to:
#   - Antarctic ice sheet (colder surface at high southern latitudes)
#   - Stronger/colder Southern Hemisphere polar vortex in winter
#   - Asymmetric land/ocean distribution

_TEMP_DATA = {
    # --------------------------------------------------
    # January (NH winter / SH summer)
    # --------------------------------------------------
    1: {
        # Southern Hemisphere (summer)
        -80: [255, 215, 210, 225, 255, 288, 280, 255, 228, 218, 212, 278, 425],
        -60: [275, 218, 212, 226, 256, 285, 275, 246, 220, 210, 208, 268, 405],
        -40: [290, 222, 215, 228, 255, 280, 266, 232, 208, 200, 202, 258, 385],
        -20: [298, 227, 218, 230, 254, 274, 257, 222, 198, 193, 198, 248, 368],
        # Equator (symmetric)
        0:   [300, 230, 220, 230, 250, 270, 255, 220, 195, 190, 195, 240, 360],
        # Northern Hemisphere (winter)
        20:  [295, 225, 218, 228, 252, 272, 252, 215, 190, 185, 195, 250, 380],
        40:  [275, 220, 215, 235, 260, 275, 245, 205, 180, 175, 200, 280, 420],
        60:  [255, 215, 210, 245, 270, 270, 235, 195, 170, 165, 210, 320, 480],
        80:  [245, 210, 205, 250, 275, 265, 225, 185, 165, 160, 220, 350, 520],
    },
    # --------------------------------------------------
    # July (NH summer / SH winter)
    # --------------------------------------------------
    7: {
        # Southern Hemisphere (winter — stronger polar vortex than NH)
        -80: [230, 205, 198, 238, 265, 255, 212, 175, 155, 150, 218, 345, 515],
        -60: [248, 210, 205, 238, 263, 260, 225, 185, 162, 157, 210, 318, 478],
        -40: [268, 218, 212, 232, 257, 270, 240, 200, 177, 172, 198, 278, 418],
        -20: [290, 224, 217, 228, 252, 271, 250, 214, 189, 184, 195, 250, 380],
        # Equator (symmetric)
        0:   [300, 230, 220, 230, 250, 270, 255, 220, 195, 190, 195, 240, 360],
        # Northern Hemisphere (summer)
        20:  [300, 228, 218, 230, 255, 275, 258, 222, 198, 192, 198, 245, 365],
        40:  [295, 225, 215, 228, 255, 280, 265, 230, 205, 198, 200, 255, 380],
        60:  [285, 220, 212, 225, 255, 285, 275, 245, 218, 208, 205, 265, 400],
        80:  [280, 218, 210, 222, 252, 288, 280, 255, 228, 215, 210, 275, 420],
    },
}

# Latitude breakpoints for interpolation
_LATS = [-80, -60, -40, -20, 0, 20, 40, 60, 80]


# --------------------------------------------------
# helper: linear interpolation
# --------------------------------------------------
def _interp(x: float, x_arr: list[float], y_arr: list[float]) -> float:
    """Linear interpolation with clamping at boundaries."""
    if x <= x_arr[0]:
        return y_arr[0]
    if x >= x_arr[-1]:
        return y_arr[-1]

    for i in range(len(x_arr) - 1):
        if x_arr[i] <= x <= x_arr[i + 1]:
            t = (x - x_arr[i]) / (x_arr[i + 1] - x_arr[i])
            return y_arr[i] + t * (y_arr[i + 1] - y_arr[i])

    return y_arr[-1]


# --------------------------------------------------
# helper: bilinear interpolation for latitude and month
# --------------------------------------------------
def _interp_temp(alt_km: float, lat: float, month: int) -> float:
    """
    Interpolate temperature from CIRA-86 tables.

    Args:
        alt_km: Altitude in kilometers
        lat: Latitude in degrees (-80 to 80)
        month: Month (1-12)

    Returns:
        Temperature in Kelvin

    Notes:
        Both hemispheres have independent temperature tables from the
        CIRA-86 reference. Intermediate months are interpolated between
        January and July profiles.
    """
    # Clamp latitude to table range
    lat = max(-80.0, min(80.0, lat))

    # Determine which seasonal profiles to use
    # Interpolate between January and July based on month
    if month <= 1 or month >= 12:
        month_weight = 0.0  # Pure January
    elif month == 7:
        month_weight = 1.0  # Pure July
    elif month < 7:
        month_weight = (month - 1) / 6.0  # Jan -> Jul
    else:
        month_weight = (13 - month) / 6.0  # Jul -> Jan

    # Get temperature profiles for bounding latitudes
    lat_idx_lo = 0
    for i, lat_val in enumerate(_LATS):
        if lat_val <= lat:
            lat_idx_lo = i

    lat_idx_hi = min(lat_idx_lo + 1, len(_LATS) - 1)
    lat_lo = _LATS[lat_idx_lo]
    lat_hi = _LATS[lat_idx_hi]

    if lat_hi == lat_lo:
        lat_weight = 0.0
    else:
        lat_weight = (lat - lat_lo) / (lat_hi - lat_lo)

    # Interpolate temperature at altitude for each corner
    T_jan_lo = _interp(alt_km, _ALT_KM, _TEMP_DATA[1][lat_lo])
    T_jan_hi = _interp(alt_km, _ALT_KM, _TEMP_DATA[1][lat_hi])
    T_jul_lo = _interp(alt_km, _ALT_KM, _TEMP_DATA[7][lat_lo])
    T_jul_hi = _interp(alt_km, _ALT_KM, _TEMP_DATA[7][lat_hi])

    # Bilinear interpolation: latitude then month
    T_jan = T_jan_lo + lat_weight * (T_jan_hi - T_jan_lo)
    T_jul = T_jul_lo + lat_weight * (T_jul_hi - T_jul_lo)
    T = T_jan + month_weight * (T_jul - T_jan)

    return T


# --------------------------------------------------
# helper: pressure from hydrostatic integration
# --------------------------------------------------
def _compute_pressure(alt_m: float, lat: float, month: int, r_air: float, g0: float) -> float:
    """
    Compute pressure by numerical integration of hydrostatic equation.

    Uses: dp/dh = -ρg = -p*g/(R*T)
    Integrated in small steps from sea level.
    """
    p = 101325.0  # Sea level pressure [Pa]
    h = 0.0
    dh = 100.0  # Step size [m]

    while h < alt_m:
        h_step = min(dh, alt_m - h)
        T = _interp_temp(h / 1000.0, lat, month)
        # dp/dh = -p * g / (R * T)
        dp_dh = -p * g0 / (r_air * T)
        p += dp_dh * h_step
        h += h_step

        if p <= 0:
            p = 1e-10
            break

    return max(p, 1e-10)


# --------------------------------------------------
# CIRA86: COSPAR International Reference Atmosphere 1986
# --------------------------------------------------
class CIRA86:
    """
    CIRA-86 (COSPAR International Reference Atmosphere 1986) model.

    Unlike USSA76, this model accounts for:
    - Latitude variation (0-90°)
    - Seasonal/monthly variation
    - Extended altitude range (0-120 km)

    Example:
        >>> from flow_state.atmosphere import CIRA86
        >>> model = CIRA86(latitude=45, month=7)  # 45°N in July
        >>> state = model(20000)  # 20 km
        >>> print(f"T = {state.temp:.2f} K, p = {state.pres:.0f} Pa")

    Notes:
        - Latitude symmetry assumed (Northern/Southern hemisphere same)
        - Temperature data interpolated from tabulated values
        - Pressure computed via hydrostatic integration
        - Valid range: 0 to ~120 km
    """

    # --------------------------------------------------
    # define attributes
    # --------------------------------------------------
    R_AIR = 287.05    # Specific gas constant for air [J/(kg·K)]
    G0 = 9.80665      # Standard gravity [m/s²]

    # --------------------------------------------------
    # initialize model with latitude and month
    # --------------------------------------------------
    def __init__(self, latitude: float = 45.0, month: int = 1) -> None:
        """
        Initialize CIRA86 model with latitude and month.

        Args:
            latitude: Latitude in degrees (-90 to 90). Defaults to 45.
            month: Month of year (1-12). Defaults to 1 (January).

        Raises:
            ValueError: If latitude or month is out of range.
        """
        if not -90 <= latitude <= 90:
            raise ValueError(f"Latitude must be between -90 and 90, got {latitude}")
        if not 1 <= month <= 12:
            raise ValueError(f"Month must be between 1 and 12, got {month}")

        self.latitude = latitude
        self.month = month

    # --------------------------------------------------
    # create callable interface for atmosphere model
    # --------------------------------------------------
    def __call__(self, altitude: float) -> AtmosphereState:
        """
        Compute atmospheric properties at a given geometric altitude.

        Args:
            altitude: Geometric altitude [m]. Valid range: 0 to ~120,000 m.

        Returns:
            AtmosphereState with altitude, temperature, pressure, and density.

        Raises:
            ValueError: If altitude is negative or exceeds 120 km.
        """
        if altitude < 0:
            raise ValueError(f"Altitude must be non-negative, got {altitude} m")
        if altitude > 120000:
            raise ValueError(f"Altitude must be <= 120 km for CIRA86, got {altitude} m")

        alt_km = altitude / 1000.0

        # Get temperature from interpolated tables
        temp = _interp_temp(alt_km, self.latitude, self.month)

        # Compute pressure via hydrostatic integration
        pres = _compute_pressure(altitude, self.latitude, self.month, self.R_AIR, self.G0)

        # Density from ideal gas law
        dens = pres / (self.R_AIR * temp)

        return AtmosphereState(altitude=altitude, temp=temp, pres=pres, dens=dens)

    # --------------------------------------------------
    # set return string representation for debugging
    # --------------------------------------------------
    def __repr__(self) -> str:
        """Return string representation for debugging (e.g. in REPL or logs)."""
        return f"CIRA86(latitude={self.latitude}, month={self.month})"
