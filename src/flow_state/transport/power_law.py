"""Power-law viscosity model."""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------
from __future__ import annotations

from dataclasses import dataclass


# --------------------------------------------------
# PowerLaw: simple power-law viscosity model
# --------------------------------------------------
@dataclass(frozen=True)
class PowerLaw:
    """
    Simple power-law viscosity model.

    The power law is:
        mu(T) = mu_ref * (T / T_ref)^m

    Useful for simplified analyses or specific temperature ranges.

    References:
        White, F.M. "Viscous Fluid Flow", McGraw-Hill

    Attributes:
        mu_ref: Reference viscosity [Pa s]
        T_ref: Reference temperature [K]
        m: Power exponent [-] (typically 0.5 to 1.0)
        name: Model identifier
    """

    # --------------------------------------------------
    # attributes
    # --------------------------------------------------
    mu_ref: float
    T_ref: float
    m: float
    name: str = "power_law"

    # --------------------------------------------------
    # class methods for standard gases
    #
    # cls is the class itself, passed automatically by @classmethod
    #
    # currently implemented:
    # - air
    # --------------------------------------------------
    @classmethod
    def air(cls, m: float = 0.76) -> PowerLaw:
        """Power law for air with specified exponent (default m=0.76)."""
        return cls(mu_ref=1.716e-5, T_ref=273.15, m=m, name="power_law_air")

    # --------------------------------------------------
    # methods to compute viscosity
    # --------------------------------------------------

    # dynamic viscosity
    def visc_dyn(self, temp: float) -> float:
        """Compute dynamic viscosity [Pa s] at temperature temp [K]."""
        if temp <= 0:
            raise ValueError(f"Temperature must be positive, got {temp} K")
        return self.mu_ref * (temp / self.T_ref) ** self.m

    # kinematic viscosity
    def visc_kin(self, temp: float, dens: float) -> float:
        """Compute kinematic viscosity [m^2/s]."""
        if dens <= 0:
            raise ValueError(f"Density must be positive, got {dens} kg/m^3")
        return self.visc_dyn(temp) / dens
