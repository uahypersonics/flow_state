"""Park high-temperature gas model.

Implements temperature-dependent thermodynamic properties using harmonic
oscillator model for vibrational excitation. Based on concepts from:

    Park, C. (1990). Nonequilibrium Hypersonic Aerothermodynamics.
    Wiley, New York.

This model captures the decrease in gamma with temperature as vibrational
modes become excited, which is the dominant real-gas effect for air at
temperatures between ~800 K and ~2500 K (before dissociation).
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------
from __future__ import annotations

import math
from dataclasses import dataclass

# --------------------------------------------------
# constants
# --------------------------------------------------
# Characteristic vibrational temperatures [K]
# From statistical mechanics / spectroscopic data
THETA_VIB_N2 = 3395.0  # nitrogen
THETA_VIB_O2 = 2239.0  # oxygen

# Specific gas constants [J/(kg K)]
R_N2 = 296.8   # R_universal / M_N2
R_O2 = 259.8   # R_universal / M_O2
R_AIR = 287.05  # R_universal / M_air


# --------------------------------------------------
# helper functions
# --------------------------------------------------
def _cv_vib_harmonic(temp: float, theta_vib: float, r_gas: float) -> float:
    """
    Vibrational contribution to cv using harmonic oscillator model.

    Args:
        temp: Temperature [K]
        theta_vib: Characteristic vibrational temperature [K]
        r_gas: Specific gas constant [J/(kg K)]

    Returns:
        Vibrational specific heat contribution [J/(kg K)]

    Notes:
        cv_vib = r_gas * (theta_vib/temp)^2 * exp(theta_vib/temp) / (exp(theta_vib/temp) - 1)^2

        This goes to 0 as temp -> 0 (frozen vibration)
        and approaches r_gas as temp -> infinity (fully excited)
    """
    if temp <= 0:
        raise ValueError(f"Temperature must be positive, got {temp} K")

    # Avoid overflow for very low temperatures
    x = theta_vib / temp
    if x > 50:
        return 0.0

    exp_x = math.exp(x)
    return r_gas * x * x * exp_x / ((exp_x - 1) ** 2)


# --------------------------------------------------
# ParkAir: high-temperature air model
# --------------------------------------------------
@dataclass(frozen=True)
class ParkAir:
    """
    High-temperature air model with vibrational excitation.

    Uses harmonic oscillator model to compute temperature-dependent
    specific heats. Treats air as a mixture of N2 (79%) and O2 (21%)
    by mole fraction.

    This model is valid for temperatures where vibrational excitation
    is significant but dissociation has not yet occurred (~500-2500 K).
    Above ~2500 K, dissociation effects become important and this model
    will underpredict gamma.

    Attributes:
        name: Model identifier

    Notes:
        - At low temp (~300 K): gamma -> 1.4 (matches perfect gas)
        - At high temp (~2000 K): gamma -> ~1.3 (vibrational excitation)
        - Pressure argument in methods is unused (thermally perfect gas)
    """

    # --------------------------------------------------
    # attributes
    # --------------------------------------------------
    name: str = "park_air"

    # Mole fractions (fixed for air)
    _y_n2: float = 0.79
    _y_o2: float = 0.21

    # --------------------------------------------------
    # class method for standard air
    #
    # cls is the class itself, passed automatically by @classmethod
    # --------------------------------------------------
    @classmethod
    def air(cls) -> ParkAir:
        """
        Standard high-temperature air model.

        Returns:
            ParkAir configured for standard air composition.

        Notes:
            - 79% N2, 21% O2 by mole
            - Valid for ~500-2500 K (vibrational regime, pre-dissociation)
        """
        return cls(name="park_air")

    # --------------------------------------------------
    # methods: thermodynamic properties
    #
    # these methods take temp and pres for API compatibility with
    # equilibrium gas models. For ParkAir, pressure is unused
    # (thermally perfect gas assumption) and r_gas is constant.
    # --------------------------------------------------

    # specific gas constant
    def r_gas(self, temp: float, pres: float) -> float:
        """
        Specific gas constant.

        Args:
            temp: Temperature [K] (ignored for Park model)
            pres: Pressure [Pa] (ignored for Park model)

        Returns:
            Specific gas constant [J/(kg K)]

        Notes:
            Constant for Park model (no dissociation, fixed composition).
        """
        return R_AIR

    # specific heat at constant volume
    def cv(self, temp: float, pres: float) -> float:
        """
        Specific heat at constant volume.

        Args:
            temp: Temperature [K]
            pres: Pressure [Pa] (unused, for API compatibility)

        Returns:
            Specific heat at constant volume [J/(kg K)]

        Notes:
            cv = cv_trans + cv_rot + cv_vib
            cv_trans = 3/2 * r_gas (translational, 3 DOF)
            cv_rot = r_gas (rotational, 2 DOF for diatomic)
            cv_vib = harmonic oscillator contribution (temp-dependent)
        """
        if temp <= 0:
            raise ValueError(f"Temperature must be positive, got {temp} K")

        # Translational + rotational (fully excited for diatomic)
        # cv_trans_rot = 5/2 * R for diatomic gas
        cv_trans_rot = 2.5 * R_AIR

        # Vibrational contribution (weighted by mole fraction)
        # Note: using mass-weighted average for mixture cv
        cv_vib_n2 = _cv_vib_harmonic(temp, THETA_VIB_N2, R_N2)
        cv_vib_o2 = _cv_vib_harmonic(temp, THETA_VIB_O2, R_O2)

        # Convert to air basis using mole fractions and molecular weights
        # For simplicity, use mole-fraction weighted average (approximate)
        cv_vib = self._y_n2 * cv_vib_n2 * (R_AIR / R_N2) + \
                 self._y_o2 * cv_vib_o2 * (R_AIR / R_O2)

        return cv_trans_rot + cv_vib

    # specific heat at constant pressure
    def cp(self, temp: float, pres: float) -> float:
        """
        Specific heat at constant pressure.

        Args:
            temp: Temperature [K]
            pres: Pressure [Pa] (unused, for API compatibility)

        Returns:
            Specific heat at constant pressure [J/(kg K)]

        Notes:
            cp = cv + r_gas (Mayer's relation, valid for ideal gas)
        """
        return self.cv(temp, pres) + R_AIR

    # specific heat ratio
    def gamma(self, temp: float, pres: float) -> float:
        """
        Specific heat ratio.

        Args:
            temp: Temperature [K]
            pres: Pressure [Pa] (unused, for API compatibility)

        Returns:
            Specific heat ratio cp/cv [-]

        Notes:
            gamma decreases from ~1.4 at low temp to ~1.3 at high temp
            as vibrational modes become excited.
        """
        cv_val = self.cv(temp, pres)
        cp_val = cv_val + R_AIR
        return cp_val / cv_val

    # speed of sound
    def sound_speed(self, temp: float) -> float:
        """
        Compute speed of sound.

        Args:
            temp: Temperature [K]

        Returns:
            Speed of sound [m/s]

        Notes:
            a = sqrt(gamma * r_gas * temp)
            gamma is temperature-dependent for this model.
        """
        if temp <= 0:
            raise ValueError(f"Temperature must be positive, got {temp} K")
        gamma_val = self.gamma(temp, 0.0)  # pres unused
        return math.sqrt(gamma_val * R_AIR * temp)

    # --------------------------------------------------
    # methods: equation of state (ideal gas law)
    # --------------------------------------------------

    # density from equation of state
    def density(self, pres: float, temp: float) -> float:
        """
        Compute density from ideal gas law.

        Args:
            pres: Pressure [Pa]
            temp: Temperature [K]

        Returns:
            Density [kg/m^3]

        Notes:
            dens = pres / (r_gas * temp)
        """
        if temp <= 0:
            raise ValueError(f"Temperature must be positive, got {temp} K")
        if pres < 0:
            raise ValueError(f"Pressure must be non-negative, got {pres} Pa")
        return pres / (R_AIR * temp)

    # pressure from equation of state
    def pressure(self, dens: float, temp: float) -> float:
        """
        Compute pressure from ideal gas law.

        Args:
            dens: Density [kg/m^3]
            temp: Temperature [K]

        Returns:
            Pressure [Pa]

        Notes:
            pres = dens * r_gas * temp
        """
        if temp <= 0:
            raise ValueError(f"Temperature must be positive, got {temp} K")
        if dens < 0:
            raise ValueError(f"Density must be non-negative, got {dens} kg/m^3")
        return dens * R_AIR * temp

    # temperature from equation of state
    def temperature(self, pres: float, dens: float) -> float:
        """
        Compute temperature from ideal gas law.

        Args:
            pres: Pressure [Pa]
            dens: Density [kg/m^3]

        Returns:
            Temperature [K]

        Notes:
            temp = pres / (dens * r_gas)
        """
        if dens <= 0:
            raise ValueError(f"Density must be positive, got {dens} kg/m^3")
        if pres < 0:
            raise ValueError(f"Pressure must be non-negative, got {pres} Pa")
        return pres / (dens * R_AIR)
