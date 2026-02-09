# flow_state

**Lightweight utility for constructing compressible flow states and reference conditions.**

`flow_state` is a pure Python package (no NumPy required) for computing thermodynamic and aerodynamic properties of compressible flows. It's designed for hypersonic research, CFD preprocessing, and educational use.

## Features

- **Unified solver**: Single `solve()` function dispatches to the right calculation
- **Atmosphere models**: USSA76 (1976 US Standard) and CIRA86 (latitude/season dependent)
- **Pure Python**: No external dependencies for core calculations
- **CLI included**: Quick calculations from the terminal
- **TOML config**: Define flow conditions in configuration files

## Quick Start

```bash
pip install flow-state
```

```python
from flow_state import solve

# From Mach number and freestream conditions
state = solve(mach=7.0, pres=1000.0, temp=200.0)
print(state)

# From Mach and altitude using atmosphere model
state = solve(mach=7.0, altitude=30_000, atm="ussa76")
print(f"Velocity: {state.uvel:.1f} m/s")
print(f"Unit Reynolds: {state.re1:.2e} /m")
```

See [Installation](installation.md) for more options.

## Why flow_state?

Setting up CFD simulations often means converting between "Mach 7 at 30 km" and actual pressure/temperature/Reynolds values. `flow_state` handles these conversions with proper gas dynamics built in.

## License

BSD-3-Clause. See [LICENSE](https://github.com/uahypersonics/flow_state/blob/main/LICENSE) for details.
