"""Transport core types."""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------
from __future__ import annotations

from typing import Protocol


# --------------------------------------------------
# TransportModel: protocol for type hints
#
# the TransportModel protocol defines the expected interface for transport models,
# allowing us to use type hints to indicate that a function returns a valid transport model instance
#
# compile time/linter type check to verify that any class we use as a transport model has the required methods and attributes
# --------------------------------------------------
class TransportModel(Protocol):
    """
    Protocol for transport models.

    Enables return type hints like `-> TransportModel` so that registries
    and factories can guarantee they return a valid model instance.

    this uses structural subtyping (duck typing with type-checker support):
    any class that has these members is considered a valid TransportModel
    without needing to explicitly inherit from this class.
    """

    # --------------------------------------------------
    # required attributes
    # --------------------------------------------------

    # transport model name (e.g. "sutherland", "power_law", "constant")
    name: str

    # --------------------------------------------------
    # required methods
    # --------------------------------------------------

    # function to compute dynamic viscosity (mu) given temperature
    def mu(self, temp: float) -> float:
        """Compute dynamic viscosity [Pa s] at temperature temp [K]."""
        ...

    # derivative of dynamic viscosity w.r.t. temperature (dmu/dT)
    def dmudt(self, temp: float) -> float:
        """Compute d(mu)/dT [Pa s / K] at temperature temp [K]."""
        ...

    # function to compute kinematic viscosity (nu) given temperature and density
    def nu(self, temp: float, dens: float) -> float:
        """Compute kinematic viscosity [m^2/s] at temp [K] and dens [kg/m^3]."""
        ...
