# flow_state

`flow_state` is a lightweight utility to compute thermodynamic and aerodynamic properties for compressible flows.

## Features

- **CLI**: Quick calculations from the terminal
- **API**: Enables scripting and automation
- **Atmosphere models**: USSA76 and CIRA86 (latitude/season dependent)

## Quick Start

Install package (see [Installation](installation.md) for detailed instructions).
```bash
pip install flow-state-calculator
```

Use `flow_state` package (see [User Guide](user-guide/index.md) for detailed examples)
```python
# load package
from flow_state import solve
# solve from known values
state = solve(mach=7.0, pres=1000.0, temp=200.0)
# print results
print(state)
```

## Why flow_state?

Specify the known quantities (Mach, altitude, Re, etc.), `flow_state` computes the rest.

## License

BSD-3-Clause. See [LICENSE](https://github.com/uahypersonics/flow_state/blob/main/LICENSE) for details.
