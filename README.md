# flow_state

Compressible flow state calculations for Python.

[![License](https://img.shields.io/badge/License-BSD_3--Clause-blue.svg)](LICENSE)
[![Python](https://img.shields.io/badge/python-≥3.11-blue.svg)](https://www.python.org/downloads/)
[![Ruff](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/astral-sh/ruff/main/assets/badge/v2.json)](https://github.com/astral-sh/ruff)

## Install

```bash
pip install git+https://github.com/uahypersonics/flow_state.git
```

## Quick Start

```python
from flow_state import solve

# From Mach and altitude
state = solve(mach=2.0, altitude=10000)

# From Mach and stagnation conditions (wind tunnel)
state = solve(mach=6, pres_stag=(140, "psi"), temp_stag=420)

# From Mach and target unit Reynolds number
state = solve(mach=5.3, re1=12.8e6)

# Access results
print(state)  # Full summary
print(state.pres, state.temp, state.re1)
```

## Features

- **Smart solver**: Automatically selects the right equations based on your inputs
- **Unit support**: Use SI or provide tuples like `(30000, "ft")`, `(140, "psi")`
- **Atmosphere models**: US Standard Atmosphere 1976 and CIRA-86
- **Gas models**: Perfect gas (air, nitrogen, custom)
- **Transport**: Sutherland viscosity law
- **Output**: JSON, TOML, or legacy `.dat` format

## CLI

```bash
# Create a config template
flow-state init

# Edit flow_config.toml, then solve
flow-state solve
```

## Atmosphere Models

```python
from flow_state import solve, atmosphere

# Default: US Standard Atmosphere 1976
state = solve(mach=2, altitude=10000)

# CIRA-86 with latitude and month
state = solve(mach=2, altitude=10000, atm=atmosphere.CIRA86(latitude=70, month=1))
```

## Documentation

Full documentation: https://uahypersonics.github.io/flow_state

## Citation

If you use `flow_state` in your research, please cite it using the "Cite this repository" button on GitHub, or:

```bibtex
@software{flow_state,
  title = {flow_state: Compressible Flow State Calculations},
  author = {Hader, Christoph},
  year = {2026},
  url = {https://github.com/uahypersonics/flow_state}
}
```

## License

BSD-3-Clause. See [LICENSE](LICENSE) for details.
