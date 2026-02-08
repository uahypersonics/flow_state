"""Calorically perfect gas model."""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------
from __future__ import annotations

import math
from dataclasses import dataclass


# --------------------------------------------------
# PerfectGas: calorically perfect gas model
# --------------------------------------------------
@dataclass(frozen=True)
class PerfectGas:
    """
    Calorically perfect gas model.

    Assumes constant specific heat ratio (gamma) and specific gas constant (r_gas).
    Valid for moderate temperatures where dissociation and vibrational excitation
    are negligible.

    Attributes:
        _gamma: Specific heat ratio cp/cv [-] (constant, use gamma() method)
        _r_gas: Specific gas constant [J/(kg K)] (constant, use r_gas() method)
        name: Model identifier

    Notes:
        - For air at standard conditions, gamma = 1.4 and r_gas = 287.05 J/(kg K)
        - Valid for static temp < ~800 K (vibrational excitation negligible)
        - gamma(), cp(), cv(), r_gas() methods take temp and pres for API
          compatibility with temperature/pressure-dependent gas models
    """

    # --------------------------------------------------
    # attributes
    # --------------------------------------------------
    _gamma: float
    _r_gas: float
    name: str = "custom"

    # --------------------------------------------------
    # class methods for standard gases
    #
    # cls is the class itself, passed automatically by @classmethod
    #
    # currently implemented:
    # - air
    # - nitrogen
    # - custom
    # --------------------------------------------------
    @classmethod
    def air(cls) -> PerfectGas:
        """
        Standard air model.

        Returns:
            PerfectGas configured for air at standard conditions.

        Notes:
            - gamma = 1.4 (diatomic gas approximation)
            - r_gas = 287.05 J/(kg K) (based on M_air = 28.97 g/mol)
            - Valid for static temp < ~800 K (vibrational excitation negligible)
        """
        return cls(_gamma=1.4, _r_gas=287.05, name="air")

    @classmethod
    def nitrogen(cls) -> PerfectGas:
        """
        Standard nitrogen model.

        Returns:
            PerfectGas configured for nitrogen at standard conditions.

        Notes:
            - gamma = 1.4 (diatomic gas)
            - r_gas = 296.8 J/(kg K) (based on M_N2 = 28.0134 g/mol)
        """
        return cls(_gamma=1.4, _r_gas=296.8, name="nitrogen")

    @classmethod
    def custom(cls, gamma: float, r_gas: float, name: str = "custom") -> PerfectGas:
        """
        Create a custom perfect gas model.

        Args:
            gamma: Specific heat ratio [-]
            r_gas: Specific gas constant [J/(kg K)]
            name: Optional identifier

        Returns:
            PerfectGas with specified properties.
        """
        return cls(_gamma=gamma, _r_gas=r_gas, name=name)

    # --------------------------------------------------
    # methods: thermodynamic properties
    #
    # these methods take temp and pres for API compatibility with
    # temperature/pressure-dependent gas models (e.g., Park, equilibrium).
    # For PerfectGas, the arguments are ignored and constant values are returned.
    # --------------------------------------------------

    # specific gas constant
    def r_gas(self, temp: float, pres: float) -> float:
        """
        Specific gas constant.

        Args:
            temp: Temperature [K] (ignored for perfect gas)
            pres: Pressure [Pa] (ignored for perfect gas)

        Returns:
            Specific gas constant [J/(kg K)]
        """
        return self._r_gas

    # specific heat ratio
    def gamma(self, temp: float, pres: float) -> float:
        """
        Specific heat ratio.

        Args:
            temp: Temperature [K] (ignored for perfect gas)
            pres: Pressure [Pa] (ignored for perfect gas)

        Returns:
            Specific heat ratio cp/cv [-]
        """
        return self._gamma

    # specific heat at constant pressure
    def cp(self, temp: float, pres: float) -> float:
        """
        Specific heat at constant pressure.

        Args:
            temp: Temperature [K] (ignored for perfect gas)
            pres: Pressure [Pa] (ignored for perfect gas)

        Returns:
            Specific heat at constant pressure [J/(kg K)]
        """
        return self._gamma * self._r_gas / (self._gamma - 1)

    # specific heat at constant volume
    def cv(self, temp: float, pres: float) -> float:
        """
        Specific heat at constant volume.

        Args:
            temp: Temperature [K] (ignored for perfect gas)
            pres: Pressure [Pa] (ignored for perfect gas)

        Returns:
            Specific heat at constant volume [J/(kg K)]
        """
        return self._r_gas / (self._gamma - 1)

    # --------------------------------------------------
    # methods: thermodynamic calculations
    # --------------------------------------------------

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
        """
        if temp <= 0:
            raise ValueError(f"Temperature must be positive, got {temp} K")
        return math.sqrt(self._gamma * self._r_gas * temp)

    # density from equation of state (perfect gas law)
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
        return pres / (self._r_gas * temp)

    # pressure from equation of state (perfect gas law)
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
        return dens * self._r_gas * temp

    # temperature from equation of state (perfect gas law)
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
        return pres / (dens * self._r_gas)
