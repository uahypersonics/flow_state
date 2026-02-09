# Basic Usage

## The `solve()` Function

The main entry point is the `solve()` function, which determines how to compute a flow state based on the parameters you provide.

```python
from flow_state import solve
```

### From Mach and Freestream Conditions

The simplest case—specify Mach number, static pressure, and static temperature:

```python
state = solve(mach=7.0, pres=1000.0, temp=200.0)
```

### From Mach and Altitude

Use an atmosphere model to get pressure and temperature at a given altitude:

```python
state = solve(mach=7.0, altitude=30_000, atm="ussa76")
```

See [Atmosphere Models](atmosphere-models.md) for available models.

### From Mach, Unit Reynolds, and Temperature

Common for wind tunnel conditions:

```python
state = solve(mach=6.0, re1=10e6, temp=300.0)
```

### From Mach, Unit Reynolds, and Altitude

Combines atmosphere lookup with Reynolds number constraint:

```python
state = solve(mach=7.0, re1=5e6, altitude=30_000, atm="ussa76")
```

## The FlowState Object

All solver functions return a `FlowState` object—a frozen dataclass containing computed properties:

```python
state = solve(mach=7.0, pres=1000.0, temp=200.0)

# Freestream conditions
state.mach      # Mach number
state.pres      # Static pressure [Pa]
state.temp      # Static temperature [K]
state.dens      # Density [kg/m³]
state.uvel      # Velocity [m/s]
state.a         # Speed of sound [m/s]

# Derived quantities
state.re1       # Unit Reynolds number [1/m]

# Stagnation conditions
state.pres_stag # Stagnation pressure [Pa]
state.temp_stag # Stagnation temperature [K]
state.dens_stag # Stagnation density [kg/m³]

# Gas properties
state.gamma     # Ratio of specific heats
state.r_gas     # Specific gas constant [J/(kg·K)]

# Metadata (when using atmosphere models)
state.altitude         # Altitude [m], if applicable
state.atmosphere_model # Name of atmosphere model used
```

## Display Methods

`FlowState` provides two display formats:

```python
# Concise representation (useful in debugger)
repr(state)  # FlowState(M=7.00, p=1000.0 Pa, T=200.0 K)

# Full summary table
print(state)
```

The full summary shows all properties in a formatted table:

```
============================================================
                       FLOW STATE SUMMARY
============================================================
Mach number              M         =       7.000
------------------------------------------------------------
                    Static Conditions
------------------------------------------------------------
Static pressure          p         =   1.000e+03 Pa
Static temperature       T         =     200.000 K
...
```

## Custom Gas Properties

By default, all calculations use air with γ = 1.4 and R = 287.05 J/(kg·K). You can provide a custom gas model:

```python
from flow_state import solve
from flow_state.gas import PerfectGas

co2 = PerfectGas(gamma=1.3, r_gas=188.9)
state = solve(mach=5.0, pres=500.0, temp=220.0, gas=co2)
```
