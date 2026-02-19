# flow_state

Compressible flow state calculations for Python.

[![Test](https://github.com/uahypersonics/flow_state/actions/workflows/test.yml/badge.svg)](https://github.com/uahypersonics/flow_state/actions/workflows/test.yml)
[![codecov](https://codecov.io/gh/uahypersonics/flow_state/branch/main/graph/badge.svg)](https://codecov.io/gh/uahypersonics/flow_state)
[![PyPI](https://img.shields.io/pypi/v/flow-state-calculator)](https://pypi.org/project/flow-state-calculator/)
[![Docs](https://img.shields.io/badge/docs-mkdocs-blue)](https://uahypersonics.github.io/flow_state/)
[![Webapp](https://img.shields.io/badge/webapp-streamlit-red)](https://flow-state-calculator.streamlit.app/)
[![License](https://img.shields.io/badge/License-BSD_3--Clause-blue.svg)](LICENSE)
[![Python](https://img.shields.io/badge/python-≥3.11-blue.svg)](https://www.python.org/downloads/)
[![Ruff](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/astral-sh/ruff/main/assets/badge/v2.json)](https://github.com/astral-sh/ruff)

## Install

```bash
pip install flow-state-calculator
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

If you use `flow_state` in your research, please cite it:

```bibtex
@software{flow_state,
  author = {Hader, Christoph},
  title = {flow\_state: Compressible Flow State Calculations},
  url = {https://github.com/uahypersonics/flow_state},
  year = {2026}
}
```

## Releasing

This project uses [Semantic Versioning](https://semver.org/) (`vMAJOR.MINOR.PATCH`):

- **MAJOR** (`v1.0.0`, `v2.0.0`): Breaking API changes
- **MINOR** (`v0.3.0`, `v0.4.0`): New features, backward-compatible
- **PATCH** (`v0.3.1`, `v0.3.2`): Bug fixes, minor corrections

To publish a new version to [PyPI](https://pypi.org/project/flow-state-calculator/):

1. Update the version in `src/flow_state/_version.py`
2. Regenerate the API architecture diagram:
   ```bash
   pydeps src/flow_state --noshow --max-bacon=4 --cluster -o docs/assets/architecture.svg
   ```
3. Commit and push to `main`
4. Tag and push:
   ```bash
   git tag vMAJOR.MINOR.PATCH
   git push origin vMAJOR.MINOR.PATCH
   ```

The GitHub Actions workflow will automatically build and publish to PyPI via Trusted Publishing.

## License

BSD-3-Clause. See [LICENSE](LICENSE) for details.
