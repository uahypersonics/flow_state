"""Turbulence scales and related quantities."""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------
from __future__ import annotations

from typing import NamedTuple


# --------------------------------------------------
# KolmogorovScales: smallest turbulence scales (dissipation range)
# --------------------------------------------------
class KolmogorovScales(NamedTuple):
    """Kolmogorov turbulence scales (dissipation range)."""

    eta: float      # Kolmogorov length scale [m]
    tau: float      # Kolmogorov time scale [s]
    vel: float      # Kolmogorov velocity scale [m/s]


# --------------------------------------------------
# TaylorScales: intermediate turbulence scales (inertial range)
# --------------------------------------------------
class TaylorScales(NamedTuple):
    """Taylor microscale (intermediate/inertial range)."""

    lmbda: float    # Taylor microscale [m]
    re_lambda: float  # Taylor-scale Reynolds number [-]


# --------------------------------------------------
# kolmogorov_scales: compute Kolmogorov scales from flow parameters
# --------------------------------------------------
def kolmogorov_scales(
    visc_kin: float,
    uvel: float,
    lref: float,
) -> KolmogorovScales:
    """
    Compute Kolmogorov turbulence scales.

    Uses the estimate epsilon = U^3/L for the turbulent dissipation rate,
    which is appropriate for high-Reynolds-number turbulent flows.

    Args:
        visc_kin: Kinematic viscosity [m^2/s]
        uvel: Characteristic velocity [m/s]
        lref: Reference length scale [m]

    Returns:
        KolmogorovScales with:
        - eta: Kolmogorov length scale, eta = (nu^3/epsilon)^(1/4) [m]
        - tau: Kolmogorov time scale, tau = (nu/epsilon)^(1/2) [s]
        - vel: Kolmogorov velocity scale, vel = (nu*epsilon)^(1/4) [m/s]

    Notes:
        The dissipation rate is estimated as epsilon = U^3/L, which assumes:
        - Turbulent kinetic energy is O(U^2)
        - Energy-containing eddies have size O(L)
        - Eddy turnover time is O(L/U)

        This gives:
        - eta/L ~ Re^(-3/4)
        - tau/(L/U) ~ Re^(-1/2)
        - vel/U ~ Re^(-1/4)

    Example:
        >>> from flow_state.turbulence import kolmogorov_scales
        >>> scales = kolmogorov_scales(visc_kin=1.5e-5, uvel=100.0, lref=1.0)
        >>> print(f"eta = {scales.eta:.2e} m")
    """
    if visc_kin <= 0:
        raise ValueError(f"Kinematic viscosity must be positive, got {visc_kin}")
    if uvel <= 0:
        raise ValueError(f"Velocity must be positive, got {uvel}")
    if lref <= 0:
        raise ValueError(f"Reference length must be positive, got {lref}")

    # estimate dissipation rate: epsilon ~ U^3 / L
    epsilon = uvel**3 / lref

    # Kolmogorov length scale: eta = (nu^3 / epsilon)^(1/4)
    eta = (visc_kin**3 / epsilon) ** 0.25

    # Kolmogorov time scale: tau = (nu / epsilon)^(1/2)
    tau = (visc_kin / epsilon) ** 0.5

    # Kolmogorov velocity scale: vel = (nu * epsilon)^(1/4)
    vel = (visc_kin * epsilon) ** 0.25

    return KolmogorovScales(eta=eta, tau=tau, vel=vel)


# --------------------------------------------------
# taylor_scales: compute Taylor microscale from flow parameters
# --------------------------------------------------
def taylor_scales(
    visc_kin: float,
    uvel: float,
    lref: float,
) -> TaylorScales:
    """
    Compute Taylor microscale and Taylor-scale Reynolds number.

    The Taylor microscale is an intermediate scale between the large
    energy-containing eddies and the small Kolmogorov dissipation scale.
    It characterizes the strain rate in the flow.

    Args:
        visc_kin: Kinematic viscosity [m^2/s]
        uvel: Characteristic velocity (or rms velocity fluctuation u') [m/s]
        lref: Reference length scale (integral scale L) [m]

    Returns:
        TaylorScales with:
        - lmbda: Taylor microscale [m]
        - re_lambda: Taylor-scale Reynolds number, Re_lambda = u' * lambda / nu [-]

    Notes:
        Using the relation lambda = L * sqrt(15) * Re^(-1/2), which comes from:
        - epsilon = 15 * nu * (u'/lambda)^2  (isotropic turbulence)
        - epsilon = u'^3 / L  (high-Re estimate)

        Scaling:
        - lambda/L ~ Re^(-1/2)
        - lambda/eta ~ Re^(1/4)

    Example:
        >>> from flow_state.turbulence import taylor_scales
        >>> scales = taylor_scales(visc_kin=1.5e-5, uvel=100.0, lref=1.0)
        >>> print(f"lambda = {scales.lmbda:.2e} m, Re_lambda = {scales.re_lambda:.0f}")
    """
    if visc_kin <= 0:
        raise ValueError(f"Kinematic viscosity must be positive, got {visc_kin}")
    if uvel <= 0:
        raise ValueError(f"Velocity must be positive, got {uvel}")
    if lref <= 0:
        raise ValueError(f"Reference length must be positive, got {lref}")

    # Reynolds number based on integral scale
    re = uvel * lref / visc_kin

    # Taylor microscale: lambda = L * sqrt(15) * Re^(-1/2)
    lmbda = lref * (15.0 ** 0.5) * (re ** -0.5)

    # Taylor-scale Reynolds number: Re_lambda = u' * lambda / nu
    re_lambda = uvel * lmbda / visc_kin

    return TaylorScales(lmbda=lmbda, re_lambda=re_lambda)
