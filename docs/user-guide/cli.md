# Command Line Interface

`flow_state` includes a CLI for quick calculations without writing Python code.

## Commands

### `flow-state init`

Generate a template configuration file:

```bash
flow-state init                    # creates flow_config.toml
flow-state init -o myconfig.toml   # custom filename
flow-state init --force            # overwrite existing
```

### `flow-state solve`

Compute flow state from direct options or a config file.

## Direct Options

For quick one-off calculations:

```bash
# Wind tunnel (stagnation conditions)
flow-state solve --mach 6 --pres-stag 140 --pres-stag-unit psi --temp-stag 420

# Flight conditions
flow-state solve --mach 7 --altitude 25000

# With atmosphere model
flow-state solve --mach 7 --altitude 30000 --atm ussa76

# Nitrogen gas
flow-state solve --mach 10 --pres-stag 1800 --pres-stag-unit psi --temp-stag 1000 --gas n2
```

## Config File

For reproducible/documented conditions:

```bash
flow-state solve --config bam6qt.toml
flow-state solve -c myconfig.toml -o results.json
```

See [Config Files](config-files.md) for the TOML format and [Examples](../examples.md#cli) for downloadable configs.

## Options Reference

| Option | Short | Description |
|--------|-------|-------------|
| `--mach` | `-m` | Mach number |
| `--pres-stag` | | Stagnation pressure (default: Pa) |
| `--pres-stag-unit` | | Pressure unit: Pa, psi, atm, bar |
| `--temp-stag` | | Stagnation temperature [K] |
| `--altitude` | `-a` | Altitude (default: m) |
| `--altitude-unit` | | Altitude unit: m, ft, km |
| `--atm` | | Atmosphere model: ussa76, cira86 |
| `--gas` | | Gas type: air, n2 |
| `--lref` | | Reference length [m] |
| `--config` | `-c` | Config file (TOML) |
| `--output` | `-o` | Output file (JSON) |
| `--quiet` | `-q` | Suppress summary output |

## Output

By default, prints a summary to stdout:

```
FlowState Summary
=================
  pres     = 6.1136e+02 Pa
  temp     = 51.22 K
  ...
```

Use `--output` to write JSON, `--quiet` to suppress the summary.
