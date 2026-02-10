# Basic Usage

## The `solve()` Function

The main entry point is `solve()`, which examines the parameters you provide and dispatches to the appropriate calculation.

```python
from flow_state import solve
```

### Input Modes

| Mode | Parameters | Use Case |
|------|------------|----------|
| Stagnation | `mach`, `pres_stag`, `temp_stag` | Wind tunnels |
| Flight | `mach`, `altitude`, `atm` | Trajectory points |
| Static | `mach`, `pres`, `temp` | Direct specification |
| Reynolds | `mach`, `re1`, `temp` | Matching Re conditions |

See [Examples](../examples.md) for concrete usage of each mode.

### Unit Support

Dimensional inputs accept `(value, "unit")` tuples:

```python
state = solve(mach=6, pres_stag=(140, "psi"), temp_stag=420)
state = solve(mach=7, altitude=(100000, "ft"), atm="ussa76")
```

| Quantity | Units |
|----------|-------|
| Pressure | Pa, psi, atm, bar, torr |
| Temperature | K, C, F, R |
| Length | m, ft, km, mi |

## The FlowState Object

`solve()` returns a `FlowState` -- a frozen dataclass with all computed properties:

```python
state = solve(mach=6.0, pres_stag=(140, "psi"), temp_stag=420)

state.mach       # Mach number
state.pres       # Static pressure [Pa]
state.temp       # Static temperature [K]
state.dens       # Density [kg/m³]
state.uvel       # Velocity [m/s]
state.re1        # Unit Reynolds number [1/m]
state.pres_stag  # Stagnation pressure [Pa]
state.temp_stag  # Stagnation temperature [K]
```

Print a formatted summary:

```python
print(state)
```

See [API Reference](../api/core.md) for the complete list of properties.

## Gas Models

Default is air (γ=1.4, R=287.05 J/kg·K). Use `gas` parameter for others:

```python
state = solve(mach=10, pres_stag=(1800, "psi"), temp_stag=1000, gas="n2")
```

Supported: `"air"`, `"n2"`
