"""Isentropic flow relations for stagnation to/from static conversions."""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------
from __future__ import annotations

from typing import NamedTuple


# --------------------------------------------------
# StaticState: static (freestream) thermodynamic state
# --------------------------------------------------
class StaticState(NamedTuple):
    """Static (freestream) thermodynamic state."""

    pres: float  # Static pressure [Pa]
    temp: float  # Static temperature [K]
    dens: float  # Static density [kg/m^3]


# --------------------------------------------------
# StagnationState: stagnation (total) thermodynamic state
# --------------------------------------------------
class StagnationState(NamedTuple):
    """Stagnation (total) thermodynamic state."""

    pres: float  # Stagnation pressure [Pa]
    temp: float  # Stagnation temperature [K]
    dens: float  # Stagnation density [kg/m^3]


# --------------------------------------------------
# stag_to_stat: convert stagnation to static conditions
# --------------------------------------------------
def stag_to_stat(
    mach: float,
    pres_stag: float,
    temp_stag: float,
    gamma: float = 1.4,
    r_gas: float = 287.05,
) -> StaticState:
    """
    Convert stagnation conditions to static conditions using isentropic relations.

    Args:
        mach: Mach number [-]
        pres_stag: Stagnation (total) pressure [Pa]
        temp_stag: Stagnation (total) temperature [K]
        gamma: Specific heat ratio [-] (default: 1.4 for air)
        r_gas: Specific gas constant [J/(kg K)] (default: 287.05 for air)

    Returns:
        StaticState with static pressure, temperature, and density.

    Notes:
        Isentropic relations:
        - temp/temp_stag = (1 + (gamma-1)/2 * mach^2)^(-1)
        - pres/pres_stag = (1 + (gamma-1)/2 * mach^2)^(-gamma/(gamma-1))
        - dens/dens_stag = (1 + (gamma-1)/2 * mach^2)^(-1/(gamma-1))

    Example:
        >>> from flow_state.isentropic import stag_to_stat
        >>> static = stag_to_stat(mach=2.0, pres_stag=101325, temp_stag=300)
        >>> print(f"Static T = {static.temp:.2f} K")
        Static T = 166.67 K
    """
    # compute conversion factor
    factor = 1.0 + 0.5 * (gamma - 1.0) * mach * mach

    # compute exponent for pressure ratio
    exp_value = gamma / (gamma - 1.0)

    # temperature ratio: temp/temp_stag = 1/factor
    temp = temp_stag / factor

    # pressure ratio: pres/pres_stag = 1/(factor^exp_value)
    pres = pres_stag / (factor ** exp_value)

    # density from ideal gas law
    dens = pres / (r_gas * temp)

    # return static state
    return StaticState(pres=pres, temp=temp, dens=dens)


# --------------------------------------------------
# stat_to_stag: convert static to stagnation conditions
# --------------------------------------------------
def stat_to_stag(
    mach: float,
    pres: float,
    temp: float,
    gamma: float = 1.4,
    r_gas: float = 287.05,
) -> StagnationState:
    """
    Convert static conditions to stagnation conditions using isentropic relations.

    Args:
        mach: Mach number [-]
        pres: Static pressure [Pa]
        temp: Static temperature [K]
        gamma: Specific heat ratio [-] (default: 1.4 for air)
        r_gas: Specific gas constant [J/(kg K)] (default: 287.05 for air)

    Returns:
        StagnationState with stagnation pressure, temperature, and density.

    Notes:
        Isentropic relations:
        - temp_stag/temp = 1 + (gamma-1)/2 * mach^2
        - pres_stag/pres = (1 + (gamma-1)/2 * mach^2)^(gamma/(gamma-1))
        - dens_stag/dens = (1 + (gamma-1)/2 * mach^2)^(1/(gamma-1))

    Example:
        >>> from flow_state.isentropic import stat_to_stag
        >>> stag = stat_to_stag(mach=2.0, pres=12855, temp=166.67)
        >>> print(f"Stagnation T = {stag.temp:.2f} K")
        Stagnation T = 300.01 K
    """
    # compute conversion factor
    factor = 1.0 + 0.5 * (gamma - 1.0) * mach * mach

    # compute exponent for pressure ratio
    exp_value = gamma / (gamma - 1.0)

    # temperature ratio: temp_stag/temp = factor
    temp_stag = temp * factor

    # pressure ratio: pres_stag/pres = factor^exp_value
    pres_stag = pres * factor ** exp_value

    # density from ideal gas law
    dens_stag = pres_stag / (r_gas * temp_stag)

    # return stagnation state
    return StagnationState(pres=pres_stag, temp=temp_stag, dens=dens_stag)


# --------------------------------------------------
# pressure_ratio: isentropic pressure ratio pres/pres_stag
# --------------------------------------------------
def pressure_ratio(mach: float, gamma: float = 1.4) -> float:
    """
    Compute isentropic pressure ratio pres/pres_stag.

    Args:
        mach: Mach number [-]
        gamma: Specific heat ratio [-]

    Returns:
        Pressure ratio pres/pres_stag [-] = 1/(1 + (gamma-1)/2 * mach^2)^(gamma/(gamma-1))
    """

    # compute conversion factor
    factor = 1.0 + 0.5 * (gamma - 1.0) * mach * mach

    # compute exponent for pressure ratio
    exp_value = gamma / (gamma - 1.0)

    # return pressure ratio
    return factor ** (-exp_value)


# --------------------------------------------------
# temperature_ratio: isentropic temperature ratio temp/temp_stag
# --------------------------------------------------
def temperature_ratio(mach: float, gamma: float = 1.4) -> float:
    """
    Compute isentropic temperature ratio temp/temp_stag.

    Args:
        mach: Mach number [-]
        gamma: Specific heat ratio [-]

    Returns:
        Temperature ratio temp/temp_stag [-] = 1 / (1 + (gamma-1)/2 * mach^2)
    """

    # compute conversion factor
    factor = 1.0 + 0.5 * (gamma - 1.0) * mach * mach

    # return temperature ratio
    return 1.0 / factor


# --------------------------------------------------
# density_ratio: isentropic density ratio dens/dens_stag
# --------------------------------------------------
def density_ratio(mach: float, gamma: float = 1.4) -> float:
    """
    Compute isentropic density ratio dens/dens_stag.

    Args:
        mach: Mach number [-]
        gamma: Specific heat ratio [-]

    Returns:
        Density ratio dens/dens_stag [-] = 1/(1 + (gamma-1)/2 * mach^2)^(1/(gamma-1))
    """

    # compute conversion factor
    factor = 1.0 + 0.5 * (gamma - 1.0) * mach * mach

    # compute exponent for density ratio
    exp_value = 1.0 / (gamma - 1.0)

    # return density ratio
    return factor ** (-exp_value)
