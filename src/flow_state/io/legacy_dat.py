"""
Legacy .dat file output for compatibility with existing tools.

Produces the fixed-width aligned format expected by downstream CFD codes.
"""

from __future__ import annotations

import math
from pathlib import Path

from flow_state.core import FlowState


def _fmt_shifted(val: float) -> str:
    """Format using Fortran-style 0.xxxx exponent notation, right-aligned.

    Standard Python %E gives 1.234E-05; this shifts the mantissa to
    produce 0.1234E-04 to match legacy Fortran output.
    """
    if val == 0.0:
        return f"{0.0:30.16E}"
    mantissa_str, exp_str = f"{val:.16E}".split("E")
    mantissa = float(mantissa_str) / 10.0
    exp = int(exp_str) + 1
    sign = "+" if exp >= 0 else "-"
    return f"{mantissa:.16f}E{sign}{abs(exp):02d}".rjust(30)


def _pa_to_psi(pa: float) -> float:
    """Convert pascals to psi."""
    return pa / 6894.757293168


def write_flow_conditions_dat(
    state: FlowState,
    path: str | Path = "flow_conditions.dat",
) -> None:
    """
    Write flow conditions to a legacy .dat file format.

    Produces the aligned label : value [unit] format expected by
    downstream CFD tools that read flow_conditions.dat.

    Args:
        state: FlowState to write.
        path: Output file path. Defaults to "flow_conditions.dat".
    """
    path = Path(path)

    # shorthand for the fixed-width value format
    def fv(val: float) -> str:
        return f"{val:30.16f}"

    lines: list[str] = []

    # transport model label
    transport_label = state.transport_model or "none"
    if "sutherland" in transport_label.lower():
        viscosity_label = "Standard Sutherland Law"
    else:
        viscosity_label = transport_label

    lines.append(f"Viscosity Option               : {viscosity_label}")

    # gas properties
    lines.append(f"Gas constant, Rgas             : {fv(state.r_gas)} [J/(kg K)]")
    lines.append(f"heat capacity (p = const.), cp : {fv(state.cp)} [J/(kg K)]")
    lines.append(f"heat capacity (V = const.), cv : {fv(state.cv)} [J/(kg K)]")
    lines.append(f"heat capacity ratio, gamma     : {fv(state.gamma)} [-]")

    # Prandtl number
    pr = state.pr if state.pr is not None else 0.0
    lines.append(f"Prandtl number, Pr             : {fv(pr)} [-]")

    # Mach number
    mach = state.mach if state.mach is not None else 0.0
    lines.append(f"freestream Mach number, M      : {fv(mach)} [-]")

    # stagnation pressure (Pa and psi)
    pres_stag = state.pres_stag if state.pres_stag is not None else 0.0
    pres_stag_psi = _pa_to_psi(pres_stag)
    lines.append(
        f"stagnation pressure, ptot      : {fv(pres_stag)} / {fv(pres_stag_psi)} [Pa]/[psi]"
    )

    # freestream static conditions
    lines.append(f"freestream pressure, pfs       : {fv(state.pres)} [Pa]")
    temp_stag = state.temp_stag if state.temp_stag is not None else 0.0
    lines.append(f"stagnation temperature, Ttot   : {fv(temp_stag)} [K]")
    lines.append(f"freestream temperature, Tfs    : {fv(state.temp)} [K]")
    dens_stag = state.dens_stag if state.dens_stag is not None else 0.0
    lines.append(f"stagnation density, rhotot     : {fv(dens_stag)} [kg/m^3]")
    lines.append(f"freestream density, rhofs      : {fv(state.dens)} [kg/m^3]")

    # transport properties
    mu = state.mu if state.mu is not None else 0.0
    lines.append(f"viscosity, mu                  : {fv(mu)} [kg/(m s)]")
    re1 = state.re1 if state.re1 is not None else 0.0
    lines.append(f"unit Reynolds number, re1      : {fv(re1)} [1/m]")

    # 1/sqrt(re1)
    inv_sqrt_re1 = 1.0 / math.sqrt(re1) if re1 > 0 else 0.0
    lines.append(f"1/sqrt(re1)                    : {fv(inv_sqrt_re1)} [sqrt(m)]")

    # velocity and speed of sound
    uvel = state.uvel if state.uvel is not None else 0.0
    lines.append(f"freestream velocity, Ufs       : {fv(uvel)} [m/s]")
    lines.append(f"freestream speed of sound, cfs : {fv(state.a)} [m/s]")

    # reference scales
    lref = state.lref if state.lref is not None else 1.0
    lines.append(f"reference length scale, lref   : {fv(lref)} [m]")
    tref = state.tref if state.tref is not None else 0.0
    lines.append(f"reference time scale, tref     : {fv(tref)} [s]")

    # stagnation enthalpy in MJ/kg
    enth_stag = state.enth_stag if state.enth_stag is not None else 0.0
    enth_stag_mj = enth_stag / 1.0e6
    lines.append(f"stagnation enthalpy            : {fv(enth_stag_mj)} [MJ/kg]")

    # Kolmogorov scales (Fortran-style scientific notation to match legacy)
    if state.kolmogorov is not None:
        eta_str = _fmt_shifted(state.kolmogorov.eta)
        tau_str = _fmt_shifted(state.kolmogorov.tau)
    else:
        eta_str = f"{0.0:30.16E}"
        tau_str = f"{0.0:30.16E}"
    lines.append(f"kolmogorov length scale        : {eta_str} [m]")
    lines.append(f"kolmogorov time scale          : {tau_str} [s]")

    # write to file
    with path.open("w") as fobj:
        fobj.write("\n".join(lines))
        fobj.write("\n")
