"""
Print human-readable summary of FlowState to terminal.
"""

from __future__ import annotations

from flow_state.core import FlowState

# --------------------------------------------------
# human-readable summary
#
# formats the FlowState for terminal display
# used by FlowState.__str__() method
# --------------------------------------------------


def summary(state: FlowState) -> str:
    """Return a human-readable summary of the flow state."""
    lines = [
        "FlowState Summary",
        "=================",
        f"  pres     = {state.pres:.4e} Pa",
        f"  temp     = {state.temp:.2f} K",
        f"  dens     = {state.dens:.4e} kg/m^3",
        f"  a        = {state.a:.2f} m/s",
    ]

    if state.mach is not None:
        lines.append(f"  mach     = {state.mach:.4f}")
    if state.uvel is not None:
        lines.append(f"  uvel     = {state.uvel:.2f} m/s")
    if state.visc_dyn is not None:
        lines.append(f"  visc_dyn = {state.visc_dyn:.4e} Pa*s")
    if state.visc_kin is not None:
        lines.append(f"  visc_kin = {state.visc_kin:.4e} m^2/s")

    lines.extend([
        f"  cp       = {state.cp:.2f} J/(kg K)",
        f"  cv       = {state.cv:.2f} J/(kg K)",
        f"  gamma    = {state.gamma:.4f}",
        f"  r_gas    = {state.r_gas:.2f} J/(kg K)",
    ])

    if state.pr is not None:
        lines.append(f"  pr       = {state.pr:.4f}")
    if state.re1 is not None:
        lines.append(f"  re1      = {state.re1:.4e} 1/m")

    # atmosphere info (if applicable)
    if state.altitude is not None:
        lines.append(f"  altitude = {state.altitude:.2f} m")
    if state.atmosphere_model is not None:
        lines.append(f"  atm_model= {state.atmosphere_model}")

    return "\n".join(lines)
