"""
US Standard Atmosphere 1976 model.

Provides atmospheric properties (temperature, pressure, density) as a function
of geometric altitude. Covers troposphere, lower stratosphere, and upper
stratosphere (0 to ~47 km).

Reference:
    https://www.grc.nasa.gov/www/k-12/airplane/atmosmet.html
    US Standard Atmosphere, 1976 (NOAA/NASA/USAF)
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------
from __future__ import annotations

import math

from flow_state.atmosphere.core import AtmosphereState


class USSA76:
    """
    US Standard Atmosphere 1976 model.

    Implements atmospheric properties as a function of geometric altitude
    for three layers:
    - Troposphere: 0 to 11,000 m (temperature decreases linearly)
    - Lower Stratosphere: 11,000 to 25,000 m (isothermal at ~216.65 K)
    - Upper Stratosphere: 25,000 to 47,000 m (temperature increases linearly)

    Example:
        >>> from flow_state.atmosphere import USSA76
        >>> model = USSA76()
        >>> state = model(10000)  # 10 km
        >>> print(f"T = {state.temp:.2f} K, p = {state.pres:.0f} Pa")

    Notes:
        - Sea level conditions: T = 288.15 K (15°C), p = 101325 Pa
        - Tropopause (~11 km): T = 216.65 K (-56.5°C), p ≈ 22632 Pa
        - Model accuracy decreases above ~47 km
    """

    # --------------------------------------------------
    # define attributes
    # --------------------------------------------------
    R_AIR = 287.05  # Specific gas constant for air [J/(kg·K)]

    # --------------------------------------------------
    # create callable interface for atmosphere model
    # --------------------------------------------------
    def __call__(self, altitude: float) -> AtmosphereState:
        """
        Compute atmospheric properties at a given geometric altitude.

        Args:
            altitude: Geometric altitude [m]. Valid range: 0 to ~47,000 m.

        Returns:
            AtmosphereState with altitude, temperature, pressure, and density.

        Raises:
            ValueError: If altitude is negative.
        """
        if altitude < 0:
            raise ValueError(f"Altitude must be non-negative, got {altitude} m")

        if altitude <= 11000:
            # --------------------------------------------------
            # Troposphere: 0 to 11,000 m
            # Temperature decreases at ~6.49 K/km (lapse rate)
            # --------------------------------------------------
            temp_celsius = 15.04 - 0.00649 * altitude
            temp = temp_celsius + 273.15  # Convert to Kelvin

            # Pressure (barometric formula)
            pres = 101290.0 * (temp / 288.08) ** 5.256

        elif altitude <= 25000:
            # --------------------------------------------------
            # Lower Stratosphere: 11,000 to 25,000 m
            # Isothermal layer at approximately -56.46°C
            # --------------------------------------------------
            temp_celsius = -56.46
            temp = temp_celsius + 273.15  # ≈ 216.69 K

            # Pressure (exponential decay in isothermal layer)
            pres = 22650.0 * math.exp(1.73 - 0.000157 * altitude)

        else:
            # --------------------------------------------------
            # Upper Stratosphere: > 25,000 m
            # Temperature increases at ~2.99 K/km
            # --------------------------------------------------
            temp_celsius = -131.21 + 0.00299 * altitude
            temp = temp_celsius + 273.15

            # Pressure
            pres = 2488.0 * (temp / 216.6) ** (-11.388)

        # Density from ideal gas law
        dens = pres / (self.R_AIR * temp)

        return AtmosphereState(altitude=altitude, temp=temp, pres=pres, dens=dens)
