# Solvers

The `solve()` function is the main entry point for computing flow states.

## solve

```python
from flow_state import solve

solve(
    *,
    mach: float | None = None,
    pres: float | tuple | None = None,
    temp: float | tuple | None = None,
    pres_stag: float | tuple | None = None,
    temp_stag: float | tuple | None = None,
    altitude: float | tuple | None = None,
    atm: str | Callable = "ussa76",
    re1: float | None = None,
    gas: PerfectGas | None = None,
) -> FlowState
```

Compute a flow state from the given parameters. The function examines which parameters are provided and dispatches to the appropriate calculation.

**Parameters:**

- `mach`: Mach number
- `pres`: Static pressure [Pa] or `(value, "unit")`
- `temp`: Static temperature [K] or `(value, "unit")`
- `pres_stag`: Stagnation pressure [Pa] or `(value, "unit")`
- `temp_stag`: Stagnation temperature [K] or `(value, "unit")`
- `altitude`: Altitude [m] or `(value, "unit")`
- `atm`: Atmosphere model (`"ussa76"`, `"cira86"`, or callable)
- `re1`: Unit Reynolds number [1/m]
- `gas`: Gas model (default: air)

**Returns:** `FlowState`

### Unit Support

Dimensional inputs can be specified with units:

```python
state = solve(mach=6, pres_stag=(140, "psi"), temp_stag=420)
state = solve(altitude=(30000, "ft"), mach=0.8)
state = solve(pres=101325, temp=(25, "C"), mach=2.0)
```

Supported units:

- **Pressure**: Pa, psi, atm, bar, torr
- **Temperature**: K, C, F, R (Rankine)
- **Length/altitude**: m, ft, km, mi

### Dispatch Logic

`solve()` examines the provided parameters and dispatches to the appropriate calculation:

| Parameters Provided | Calculation |
|---------------------|-------------|
| `mach`, `pres`, `temp` | From static conditions |
| `mach`, `pres_stag`, `temp_stag` | From stagnation conditions |
| `mach`, `altitude`, `atm` | Atmosphere lookup |
| `mach`, `re1`, `temp` | From unit Reynolds + temperature |
| `mach`, `re1`, `altitude` | From unit Reynolds + altitude |

### Examples

```python
from flow_state import solve

# From Mach, pressure, temperature
state = solve(mach=7.0, pres=1000.0, temp=200.0)

# From Mach and altitude
state = solve(mach=7.0, altitude=30_000, atm="ussa76")

# From Mach and unit Reynolds
state = solve(mach=6.0, re1=10e6, temp=300.0)

# From stagnation conditions
state = solve(mach=6.0, pres_stag=(140, "psi"), temp_stag=420)
```

---

## Isentropic Relations

Low-level functions for isentropic flow calculations.

```python
from flow_state import isentropic
```

### Temperature Ratio

```python
isentropic.temperature_ratio(mach: float, gamma: float = 1.4) -> float
```

Returns $T_0 / T = 1 + \frac{\gamma - 1}{2} M^2$

### Pressure Ratio

```python
isentropic.pressure_ratio(mach: float, gamma: float = 1.4) -> float
```

Returns $p_0 / p = \left(1 + \frac{\gamma - 1}{2} M^2\right)^{\gamma/(\gamma-1)}$

### Density Ratio

```python
isentropic.density_ratio(mach: float, gamma: float = 1.4) -> float
```

Returns $\rho_0 / \rho = \left(1 + \frac{\gamma - 1}{2} M^2\right)^{1/(\gamma-1)}$

### Example

```python
from flow_state import isentropic

M = 7.0
gamma = 1.4

T0_over_T = isentropic.temperature_ratio(M, gamma)
p0_over_p = isentropic.pressure_ratio(M, gamma)
rho0_over_rho = isentropic.density_ratio(M, gamma)

print(f"T0/T = {T0_over_T:.2f}")   # 10.80
print(f"p0/p = {p0_over_p:.1f}")   # 4140.0
```
