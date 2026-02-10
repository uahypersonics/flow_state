# Output Formats

`flow_state` can output results to JSON or TOML files for use in other tools or for record-keeping.

## JSON Output

### CLI

Use `--output` to write results to a JSON file:

```bash
flow-state solve --mach 6 --pres-stag 140 --pres-stag-unit psi --temp-stag 420 --output results.json
```

### Python

```python
from flow_state import solve
from flow_state.io import write_json

state = solve(mach=6, pres_stag=(140, "psi"), temp_stag=420)
write_json(state, "results.json")
```

### Format

Each value is stored as `[value, "unit"]` for self-documentation:

```json
{
  "gas_model": "air",
  "transport_model": "sutherland",
  "pres": [611.36, "Pa"],
  "temp": [51.22, "K"],
  "dens": [0.0416, "kg/m^3"],
  "mach": [6.0, "-"],
  "uvel": [860.5, "m/s"],
  "re1": [2.45e6, "1/m"],
  "pres_stag": [965266.0, "Pa"],
  "temp_stag": [420.0, "K"],
  "provenance": {
    "builder": "solve",
    "inputs": {
      "mach": 6.0,
      "pres_stag": [140, "psi"],
      "temp_stag": 420
    }
  }
}
```

The `provenance` field records the original inputs exactly as provided, useful for reproducibility.

## TOML Output

```python
from flow_state import solve
from flow_state.io import write_toml

state = solve(mach=6, pres_stag=(140, "psi"), temp_stag=420)
write_toml(state, "results.toml")
```

## Converting to Dict

For custom processing, use `to_dict()`:

```python
state = solve(mach=6, pres_stag=(140, "psi"), temp_stag=420)
data = state.to_dict()

# Access values
pres_value, pres_unit = data["pres"]  # [611.36, "Pa"]
```
