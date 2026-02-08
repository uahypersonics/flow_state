"""Transport core types."""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------
from __future__ import annotations

from typing import Protocol


# --------------------------------------------------
# TransportModel: protocol for type hints
# --------------------------------------------------
class TransportModel(Protocol):
    """
    Protocol for transport models.

    Enables return type hints like `-> TransportModel` so that registries
    and factories can guarantee they return a valid model instance.
    """

    name: str

    def visc_dyn(self, temp: float) -> float:
        """Compute dynamic viscosity [Pa s] at temperature temp [K]."""
        ...

    def visc_kin(self, temp: float, dens: float) -> float:
        """Compute kinematic viscosity [m^2/s] at temp [K] and dens [kg/m^3]."""
        ...
