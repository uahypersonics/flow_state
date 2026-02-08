"""Atmosphere core types."""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class AtmosphereState:
    """
    Atmospheric conditions at a given altitude.

    Attributes:
        altitude: Geometric altitude [m]
        temp: Static temperature [K]
        pres: Static pressure [Pa]
        dens: Density [kg/m^3]
    """

    altitude: float
    temp: float
    pres: float
    dens: float
