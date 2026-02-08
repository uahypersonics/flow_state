#!/usr/bin/env python3
"""
Example: Compute atmospheric flight conditions using CIRA86 atmosphere.

Demonstrates solving for altitude given Mach and unit Reynolds number
using the CIRA-86 atmosphere model with latitude and month settings.

CIRA86 accounts for:
    - Latitude variation (polar vs equatorial)
    - Seasonal variation (month of year)
    - Extended altitude range (0-120 km)

Inputs:
    Mach number: 5.3
    Unit Reynolds number: 12.8e6 1/m
    Latitude: 70°N (Arctic region)
    Month: 1 (January - winter conditions)
"""

from pathlib import Path

from flow_state import atmosphere, io, solve

# --------------------------------------------------
# define inputs
# --------------------------------------------------
mach = 5.3              # mach number [-]
re1 = 12.8e6            # unit reynolds number [1/m]
latitude = 70           # latitude [degrees]
month = 1               # month of year (1-12)

# --------------------------------------------------
# solve for flow state using CIRA86 atmosphere
# --------------------------------------------------
state = solve(
    mach=mach,
    re1=re1,
    atm=atmosphere.CIRA86(latitude=latitude, month=month),
)

# --------------------------------------------------
# print summary to terminal
# --------------------------------------------------
print(io.summary(state))

# --------------------------------------------------
# write results to json file (same directory as this script)
# --------------------------------------------------
output_dir = Path(__file__).parent
io.write_json(state, output_dir / "flight_conditions_cira86.json")
