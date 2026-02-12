# Transport Models

Viscosity models for computing temperature-dependent transport properties.

## Available Models

| Class | Description | Presets |
|-------|-------------|---------|
| `Sutherland` | Standard Sutherland's law | `.air()`, `.nitrogen()` |
| `SutherlandLowTemp` | Low-temperature corrected | `.air()` |
| `SutherlandBlended` | Smooth polynomial blend | `.air()` |
| `Keyes` | High-temperature model | `.air()`, `.nitrogen()` |
| `PowerLaw` | Simple power law | `.air(m=0.76)` |

## Methods

All transport models provide:

| Method | Returns | Description |
|--------|---------|-------------|
| `mu(temp)` | `float` | Dynamic viscosity [Pa s] |
| `nu(temp, dens)` | `float` | Kinematic viscosity [m^2/s] |
| `dmudt(temp)` | `float` | Viscosity temperature derivative [Pa s / K] |

## Usage

```python
from flow_state.transport import Sutherland

# create model
model = Sutherland.air()

# compute viscosity at 300 K
T = 300.0
mu = model.mu(T)           # ~1.85e-5 Pa s
dmudt = model.dmudt(T)     # derivative for CFD applications
nu = model.nu(T, rho=1.2)  # kinematic viscosity
```

### Custom Parameters

```python
from flow_state.transport import Sutherland

# custom gas
model = Sutherland.custom(
    mu_ref=1.8e-5,
    T_ref=300.0,
    S=120.0,
    name="my_gas"
)
```

### Model Selection

```python
from flow_state.transport import (
    Sutherland,
    SutherlandLowTemp,
    SutherlandBlended,
    Keyes,
    PowerLaw,
)

# standard (moderate temperatures)
model = Sutherland.air()

# low-temperature flows (cryogenic, expansion tunnels)
model = SutherlandLowTemp.air()

# smooth derivative (CFD stability)
model = SutherlandBlended.air()

# high-temperature (hypersonic)
model = Keyes.air()

# simplified analysis
model = PowerLaw.air(m=0.76)
```

## Theory

See [Transport Theory](../theory/transport.md) for the underlying formulas.
