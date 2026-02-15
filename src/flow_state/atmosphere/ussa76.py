"""
US Standard Atmosphere 1976 model.

Provides atmospheric properties (temperature, pressure, density) as a function
of geopotential altitude for seven layers from 0 to 86 km.

Reference:
    US Standard Atmosphere, 1976 (NOAA/NASA/USAF)
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------
from __future__ import annotations

import math

from flow_state.atmosphere.core import AtmosphereState

# --------------------------------------------------
# USSA76 layer definitions
# each layer: (base geopotential altitude [m], base temperature [K], lapse rate [K/m])
# --------------------------------------------------
_LAYERS = [
    (0,     288.150, -0.0065),   # Troposphere
    (11000, 216.650,  0.0),      # Tropopause
    (20000, 216.650,  0.0010),   # Stratosphere 1
    (32000, 228.650,  0.0028),   # Stratosphere 2
    (47000, 270.650,  0.0),      # Stratopause
    (51000, 270.650, -0.0028),   # Mesosphere 1
    (71000, 214.650, -0.0020),   # Mesosphere 2
]

# --------------------------------------------------
# standard constants
# --------------------------------------------------
_G0 = 9.80665       # standard gravity [m/s²]
_M = 0.0289644      # molar mass of dry air [kg/mol]
_R_STAR = 8.31447   # universal gas constant [J/(mol·K)]
_R_AIR = 287.05     # specific gas constant for air [J/(kg·K)]

# --------------------------------------------------
# pre-compute base pressures for each layer
# --------------------------------------------------
_BASE_PRESSURES: list[float] = [101325.0]  # sea level pressure [Pa]

for _i in range(1, len(_LAYERS)):
    _h_b, _T_b, _lapse = _LAYERS[_i - 1]
    _h_top = _LAYERS[_i][0]
    _p_b = _BASE_PRESSURES[_i - 1]

    if _lapse == 0.0:
        # isothermal layer
        _p_top = _p_b * math.exp(-_G0 * _M * (_h_top - _h_b) / (_R_STAR * _T_b))
    else:
        # gradient layer
        _T_top = _T_b + _lapse * (_h_top - _h_b)
        _p_top = _p_b * (_T_top / _T_b) ** (-_G0 * _M / (_R_STAR * _lapse))

    _BASE_PRESSURES.append(_p_top)


class USSA76:
    """
    US Standard Atmosphere 1976 model.

    Implements atmospheric properties as a function of geopotential altitude
    for seven layers from 0 to 86 km:

    - Troposphere:      0–11 km   (lapse rate −6.5 K/km)
    - Tropopause:      11–20 km   (isothermal 216.65 K)
    - Stratosphere 1:  20–32 km   (lapse rate +1.0 K/km)
    - Stratosphere 2:  32–47 km   (lapse rate +2.8 K/km)
    - Stratopause:     47–51 km   (isothermal 270.65 K)
    - Mesosphere 1:    51–71 km   (lapse rate −2.8 K/km)
    - Mesosphere 2:    71–86 km   (lapse rate −2.0 K/km)

    Example:
        >>> from flow_state.atmosphere import USSA76
        >>> model = USSA76()
        >>> state = model(10000)  # 10 km
        >>> print(f"T = {state.temp:.2f} K, p = {state.pres:.0f} Pa")

    Notes:
        - Sea level conditions: T = 288.15 K (15°C), p = 101325 Pa
        - Tropopause (~11 km): T = 216.65 K (−56.5°C), p ≈ 22632 Pa
        - Mesopause (~86 km): T ≈ 186.87 K, p ≈ 0.37 Pa
    """

    # --------------------------------------------------
    # define attributes
    # --------------------------------------------------
    R_AIR = _R_AIR

    # --------------------------------------------------
    # create callable interface for atmosphere model
    # --------------------------------------------------
    def __call__(self, altitude: float) -> AtmosphereState:
        """
        Compute atmospheric properties at a given geopotential altitude.

        Args:
            altitude: Geopotential altitude [m]. Valid range: 0 to 86,000 m.

        Returns:
            AtmosphereState with altitude, temperature, pressure, and density.

        Raises:
            ValueError: If altitude is negative or exceeds 86 km.
        """
        if altitude < 0:
            raise ValueError(f"Altitude must be non-negative, got {altitude} m")
        if altitude > 86000:
            raise ValueError(f"Altitude must be <= 86 km for USSA76, got {altitude} m")

        # find the layer containing this altitude
        layer_idx = 0
        for i in range(len(_LAYERS) - 1, -1, -1):
            if altitude >= _LAYERS[i][0]:
                layer_idx = i
                break

        h_b, T_b, lapse = _LAYERS[layer_idx]
        p_b = _BASE_PRESSURES[layer_idx]
        dh = altitude - h_b

        # temperature
        temp = T_b + lapse * dh

        # pressure
        if lapse == 0.0:
            pres = p_b * math.exp(-_G0 * _M * dh / (_R_STAR * T_b))
        else:
            pres = p_b * (temp / T_b) ** (-_G0 * _M / (_R_STAR * lapse))

        # density from ideal gas law
        dens = pres / (_R_AIR * temp)

        return AtmosphereState(altitude=altitude, temp=temp, pres=pres, dens=dens)
