# FlowState

The `FlowState` class is a frozen dataclass representing a complete compressible flow state.

## Overview

```python
from flow_state import FlowState
```

`FlowState` objects are returned by all solver functions. They are immutable (frozen) to ensure consistency of the computed properties.

## Class Definition

```python
class FlowState:
    """Frozen dataclass representing a complete flow state."""
    # static conditions
    pres: float           # Static pressure [Pa]
    temp: float           # Static temperature [K]
    dens: float           # Density [kg/m^3]
    a: float              # Speed of sound [m/s]
    
    # flow kinematics
    mach: float           # Mach number [-]
    uvel: float           # Velocity [m/s]
    
    # transport
    mu: float       # Dynamic viscosity [Pa-s]
    nu: float       # Kinematic viscosity [m^2/s]
    re1: float            # Unit Reynolds number [1/m]
    
    # gas properties
    gamma: float          # Ratio of specific heats [-]
    r_gas: float          # Specific gas constant [J/(kg-K)]
    
    # stagnation conditions
    pres_stag: float      # Stagnation pressure [Pa]
    temp_stag: float      # Stagnation temperature [K]
    dens_stag: float      # Stagnation density [kg/m^3]
    
    # metadata
    altitude: float | None = None
    atmosphere_model: str | None = None
```

## Properties

### Static Conditions

| Property | Type | Description |
|----------|------|-------------|
| `pres` | `float` | Static pressure [Pa] |
| `temp` | `float` | Static temperature [K] |
| `dens` | `float` | Density [kg/m^3] |
| `a` | `float` | Speed of sound [m/s] |

### Flow Kinematics

| Property | Type | Description |
|----------|------|-------------|
| `mach` | `float` | Mach number |
| `uvel` | `float` | Velocity [m/s] |

### Transport Properties

| Property | Type | Description |
|----------|------|-------------|
| `mu` | `float` | Dynamic viscosity [Pa-s] |
| `nu` | `float` | Kinematic viscosity [m^2/s] |
| `re1` | `float` | Unit Reynolds number [1/m] |

### Stagnation Conditions

| Property | Type | Description |
|----------|------|-------------|
| `pres_stag` | `float` | Stagnation pressure [Pa] |
| `temp_stag` | `float` | Stagnation temperature [K] |
| `dens_stag` | `float` | Stagnation density [kg/m^3] |

### Gas Properties

| Property | Type | Description |
|----------|------|-------------|
| `gamma` | `float` | Ratio of specific heats |
| `r_gas` | `float` | Specific gas constant [J/(kg-K)] |

### Metadata

| Property | Type | Description |
|----------|------|-------------|
| `altitude` | `float \| None` | Altitude [m], if computed from atmosphere |
| `atmosphere_model` | `str \| None` | Name of atmosphere model used |

## Methods

### `__str__`

Returns a formatted summary table of all flow properties.

```python
state = solve(mach=7.0, pres=1000.0, temp=200.0)
print(state)
```

### `__repr__`

Returns a concise representation for debugging.

```python
state = solve(mach=7.0, pres=1000.0, temp=200.0)
repr(state)  # FlowState(M=7.00, p=1000.0 Pa, T=200.0 K)
```

## Example

```python
from flow_state import solve

state = solve(mach=7.0, altitude=30_000, atm="ussa76")

# Access individual properties
print(f"Mach: {state.mach}")
print(f"Velocity: {state.uvel:.1f} m/s")
print(f"Unit Reynolds: {state.re1:.2e} /m")
print(f"Stagnation temperature: {state.temp_stag:.1f} K")
```
