#!/usr/bin/env python3
"""
Example: Compute flow conditions for BAM6QT wind tunnel.

Demonstrates using flow_state as a Python module:
    - solve() to compute flow state from inputs
    - summary() to print human-readable output
    - write_json() to save results to file

Inputs:
    Mach number: 6
    Stagnation pressure: 140 psi
    Stagnation temperature: 420 K
"""

from flow_state import solve
from flow_state.io import summary, write_json

# --------------------------------------------------
# define inputs
# --------------------------------------------------
mach = 6.0              # mach number [-]
pres_stag_psi = 140.0   # stagnation pressure [psi]
temp_stag = 420.0       # stagnation temperature [K]
lref = 1.0              # reference length [m]

# --------------------------------------------------
# solve for flow state
# --------------------------------------------------
state = solve(
    mach=mach,
    pres_stag=(pres_stag_psi, "psi"),
    temp_stag=temp_stag,
    lref=lref,
)

# --------------------------------------------------
# print summary to terminal
# --------------------------------------------------
print(summary(state))

# --------------------------------------------------
# write results to json file
# --------------------------------------------------
write_json(state, "bam6qt_conditions.json")
