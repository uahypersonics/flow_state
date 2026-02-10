# Config Files

`flow_state` reads flow conditions from TOML configuration files. Useful for documenting setups, sharing conditions, and version control.

## Usage

### CLI

```bash
flow-state solve --config myconfig.toml
flow-state solve -c myconfig.toml -o results.json
```

### Python

```python
from flow_state.io import read_config
from flow_state import solve

kwargs = read_config("myconfig.toml")
state = solve(**kwargs)
```

## Format

### Stagnation Conditions (Wind Tunnel)

```toml
mach = 6.0
pres_stag = [140, "psi"]
temp_stag = 420
```

### Flight Conditions

```toml
mach = 7.0
altitude = 25000
```

With atmosphere model:

```toml
mach = 7.0
altitude = 30000

[atmosphere]
model = "cira86"
latitude = 35
month = 7
```

## Supported Fields

| Field | Type | Description |
|-------|------|-------------|
| `mach` | float | Mach number (required) |
| `pres_stag` | float or [value, "unit"] | Stagnation pressure |
| `temp_stag` | float | Stagnation temperature [K] |
| `altitude` | float or [value, "unit"] | Altitude |
| `pres` | float or [value, "unit"] | Static pressure |
| `temp` | float or [value, "unit"] | Static temperature |
| `gas` | string | Gas model: "air", "n2" |
| `lref` | float | Reference length [m] |

### Atmosphere Section

| Field | Type | Description |
|-------|------|-------------|
| `model` | string | "ussa76" or "cira86" |
| `latitude` | float | Latitude for CIRA86 [deg] |
| `month` | int | Month for CIRA86 (1-12) |

## Unit Syntax

Use `[value, "unit"]` arrays for non-SI units:

```toml
pres_stag = [140, "psi"]
altitude = [100000, "ft"]
temp = [25, "C"]
```

## Downloadable Examples

See [Examples](../examples.md#config-files) for ready-to-use config files (BAM6QT, AEDC T9, HIFiRE-1, STORT).
