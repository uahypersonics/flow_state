# Config Files

`flow_state` can read flow conditions from TOML configuration files. This is useful for:

- Documenting simulation setups
- Sharing conditions between team members
- Version-controlling flow parameters

## Basic Format

```toml
# flow_config.toml
mach = 7.0
pres = 1000.0
temp = 200.0
```

Load with:

```python
from flow_state.io import read_config

state = read_config("flow_config.toml")
```

Or from the CLI:

```bash
flow-state from-config flow_config.toml
```

## With Atmosphere Model

```toml
mach = 7.0
altitude = 30000
atm = "ussa76"
```

## CIRA86 with Parameters

For CIRA86 with custom latitude and month, use a nested table:

```toml
mach = 7.0
altitude = 30000

[atmosphere]
model = "cira86"
latitude = 70
month = 7
```

## Unit Reynolds Specification

```toml
mach = 6.0
re1 = 10e6
temp = 300.0
```

## Custom Gas Properties

```toml
mach = 5.0
pres = 500.0
temp = 220.0
gamma = 1.3
r_gas = 188.9
```

## Full Example

```toml
# BAM6QT Mach 6 Quiet Tunnel conditions
# Purdue University

mach = 6.0
re1 = 10.4e6
temp = 52.8
```

## Writing Config Files

You can export a `FlowState` to TOML:

```python
from flow_state import solve
from flow_state.io import write_toml

state = solve(mach=7.0, pres=1000.0, temp=200.0)
write_toml(state, "output.toml")
```

## Supported Fields

| Field | Description | Required |
|-------|-------------|----------|
| `mach` | Mach number | Yes |
| `pres` | Static pressure [Pa] | Conditional |
| `temp` | Static temperature [K] | Conditional |
| `altitude` | Altitude [m] | Conditional |
| `atm` | Atmosphere model name | With altitude |
| `re1` | Unit Reynolds number [1/m] | Conditional |

Valid combinations:

1. `mach` + `pres` + `temp`
2. `mach` + `altitude` + `atm`
3. `mach` + `re1` + `temp`
4. `mach` + `re1` + `altitude`
