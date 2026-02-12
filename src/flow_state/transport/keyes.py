"""Keyes viscosity model."""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------
from __future__ import annotations

import math
from dataclasses import dataclass


# --------------------------------------------------
# Keyes: high-temperature viscosity law
# --------------------------------------------------
@dataclass(frozen=True)
class Keyes:
    """
    Keyes viscosity law for high-temperature flows.

    The Keyes formula is:
        mu(T) = a0 * sqrt(T) / (1 + a1 * 10^(-a2/T) / T)

    This law is often used for hypersonic flows where temperatures can be very high.

    References:
        - Priebe & Martin (2011), AIAA 2011-3432
        - Roy & Blottner (2006), Progress in Aerospace Sciences, 42(7-8), 469-530

    Attributes:
        a0: Coefficient [kg/(m·s·K^0.5)]
        a1: Coefficient [K]
        a2: Coefficient [-]
        name: Model identifier
    """

    # --------------------------------------------------
    # attributes
    # --------------------------------------------------
    a0: float
    a1: float
    a2: float
    name: str = "keyes"

    # --------------------------------------------------
    # class methods for standard gases
    #
    # cls is the class itself, passed automatically by @classmethod
    #
    # currently implemented:
    # - air
    # - nitrogen
    # --------------------------------------------------
    @classmethod
    def air(cls) -> Keyes:
        """Keyes viscosity model for air."""
        return cls(a0=1.488e-6, a1=122.1, a2=5.0, name="keyes_air")

    @classmethod
    def nitrogen(cls) -> Keyes:
        """Keyes viscosity model for nitrogen."""
        return cls(a0=1.418e-6, a1=116.4, a2=5.0, name="keyes_nitrogen")

    # --------------------------------------------------
    # methods to compute viscosity
    # --------------------------------------------------

    def mu(self, temp: float) -> float:
        """Dynamic viscosity [Pa s] at temperature temp [K]."""
        if temp <= 0:
            raise ValueError(f"Temperature must be positive, got {temp} K")

        exp_term = 10.0 ** (-self.a2 / temp)
        return self.a0 * math.sqrt(temp) / (1.0 + self.a1 * exp_term / temp)

    def dmudt(self, temp: float) -> float:
        """Derivative of dynamic viscosity w.r.t. temperature [Pa s / K]."""
        if temp <= 0:
            raise ValueError(f"Temperature must be positive, got {temp} K")

        # mu = a0 * T^0.5 / D, where D = 1 + a1 * 10^(-a2/T) / T
        exp_term = 10.0 ** (-self.a2 / temp)
        D = 1.0 + self.a1 * exp_term / temp

        # dD/dT = a1 * 10^(-a2/T) * (a2 * ln(10) - T) / T^3
        ln10 = math.log(10.0)
        dD_dT = self.a1 * exp_term * (self.a2 * ln10 - temp) / temp**3

        # d(mu)/dT = a0 * [0.5 * T^-0.5 / D - T^0.5 * dD/dT / D^2]
        return self.a0 * (0.5 / (math.sqrt(temp) * D) - math.sqrt(temp) * dD_dT / D**2)

    def nu(self, temp: float, dens: float) -> float:
        """Kinematic viscosity [m^2/s]."""
        if dens <= 0:
            raise ValueError(f"Density must be positive, got {dens} kg/m^3")
        return self.mu(temp) / dens
