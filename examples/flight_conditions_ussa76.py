#!/usr/bin/env python3
"""
Example: Compute atmospheric flight conditions.

Demonstrates solving for altitude given Mach and unit Reynolds number.
Uses US Standard Atmosphere 1976 to find conditions.

Inputs:
    Mach number: 5.3
    Unit Reynolds number: 12.8e6 1/m
"""

from flow_state import io, solve

#rom flow_state.io import summary, write_json

# --------------------------------------------------
# define inputs
# --------------------------------------------------
mach = 5.3              # mach number [-]
re1 = 12.8e6            # unit reynolds number [1/m]

# --------------------------------------------------
# solve for flow state (finds altitude automatically)
# --------------------------------------------------
state = solve(
    mach=mach,
    re1=re1,
)

# --------------------------------------------------
# print summary to terminal
# --------------------------------------------------
print(io.summary(state))

# --------------------------------------------------
# write results to json file
# --------------------------------------------------
io.write_json(state, "atmospheric_flight.json")
