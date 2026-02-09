# Command Line Interface

`flow_state` includes a CLI for quick calculations without writing Python code.

## Basic Usage

```bash
flow-state solve --mach 7.0 --p-inf 1000 --t-inf 200
```

Output:

```
============================================================
                       FLOW STATE SUMMARY
============================================================
Mach number              M         =       7.000
------------------------------------------------------------
                    Freestream Conditions
------------------------------------------------------------
Static pressure          p_inf     =   1.000e+03 Pa
Static temperature       T_inf     =     200.000 K
...
```

## With Atmosphere Model

```bash
flow-state solve --mach 7.0 --altitude 30000 --atm ussa76
```

## From Unit Reynolds Number

```bash
flow-state solve --mach 6.0 --re1 10e6 --t-inf 300
```

## Output Formats

### JSON Output

```bash
flow-state solve --mach 7.0 --p-inf 1000 --t-inf 200 --format json
```

```json
{
  "mach": 7.0,
  "p_inf": 1000.0,
  "t_inf": 200.0,
  "rho_inf": 0.01742,
  "u_inf": 1986.1,
  ...
}
```

### Quiet Mode

Suppress the header, show only values:

```bash
flow-state solve --mach 7.0 --p-inf 1000 --t-inf 200 --quiet
```

## From Config File

```bash
flow-state from-config examples/flow_config.toml
```

See [Config Files](config-files.md) for the TOML format.

## Available Options

```bash
flow-state solve --help
```

| Option | Description |
|--------|-------------|
| `--mach` | Mach number (required) |
| `--p-inf` | Static pressure [Pa] |
| `--t-inf` | Static temperature [K] |
| `--altitude` | Altitude [m] |
| `--atm` | Atmosphere model (`ussa76` or `cira86`) |
| `--re1` | Unit Reynolds number [1/m] |
| `--gamma` | Ratio of specific heats (default: 1.4) |
| `--r-gas` | Gas constant [J/(kg·K)] (default: 287.05) |
| `--format` | Output format (`table` or `json`) |
| `--quiet` | Minimal output |

## Examples

**High-altitude flight:**
```bash
flow-state solve --mach 25 --altitude 70000 --atm ussa76
```

**Wind tunnel conditions:**
```bash
flow-state solve --mach 6 --re1 10e6 --t-inf 65
```

**Different gas (CO₂):**
```bash
flow-state solve --mach 5 --p-inf 500 --t-inf 220 --gamma 1.3 --r-gas 188.9
```
