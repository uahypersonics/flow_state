# Atmosphere

The `atmosphere` module provides standard atmosphere models.

```python
from flow_state import atmosphere
```

## AtmosphereState

A named tuple returned by atmosphere models:

```python
from flow_state.atmosphere import AtmosphereState

# Fields
AtmosphereState(
    temperature: float,  # Temperature [K]
    pressure: float,     # Pressure [Pa]
    density: float,      # Density [kg/m³]
)
```

---

## USSA76

US Standard Atmosphere 1976. Valid from 0 to 86 km.

### Function

```python
from flow_state import atmosphere

atmosphere.ussa76(altitude: float) -> AtmosphereState
```

**Parameters:**

- `altitude`: Geometric altitude in meters (0 to 86,000)

**Returns:** `AtmosphereState` with temperature, pressure, and density

### Example

```python
from flow_state import atmosphere

# Sea level
sl = atmosphere.ussa76(0)
print(f"Sea level: T={sl.temperature:.1f} K, p={sl.pressure:.0f} Pa")

# 30 km altitude
h30 = atmosphere.ussa76(30_000)
print(f"30 km: T={h30.temperature:.1f} K, p={h30.pressure:.1f} Pa")
```

---

## CIRA86

COSPAR International Reference Atmosphere 1986. Valid from 0 to 120 km with latitude and seasonal variations.

### Class

```python
from flow_state import atmosphere

cira = atmosphere.CIRA86(latitude: int = 45, month: int = 1)
state = cira(altitude: float) -> AtmosphereState
```

**Parameters:**

- `latitude`: Latitude in degrees North (0, 10, 20, ..., 80)
- `month`: Month (1 = January, ..., 12 = December)

### Example

```python
from flow_state import atmosphere

# Winter polar atmosphere
winter_polar = atmosphere.CIRA86(latitude=70, month=1)

# Summer equatorial atmosphere  
summer_equatorial = atmosphere.CIRA86(latitude=0, month=7)

# Compare at 50 km
h = 50_000
polar = winter_polar(h)
equatorial = summer_equatorial(h)

print(f"Polar (70°N, Jan): T={polar.temperature:.1f} K")
print(f"Equatorial (Jul):  T={equatorial.temperature:.1f} K")
```

---

## Registry

Get atmosphere models by name:

```python
from flow_state.atmosphere import get_atmosphere_model

get_atmosphere_model(name: str) -> Callable[[float], AtmosphereState]
```

**Parameters:**

- `name`: Model name (`"ussa76"` or `"cira86"`)

**Returns:** Callable that takes altitude and returns `AtmosphereState`

### Available Models

| Name | Model |
|------|-------|
| `"ussa76"` | US Standard Atmosphere 1976 |
| `"cira86"` | CIRA86 (default: 45°N, January) |

---

## Custom Atmosphere Models

Any callable with signature `(altitude: float) -> AtmosphereState` can be used:

```python
from flow_state import solve
from flow_state.atmosphere import AtmosphereState

def isothermal_atmosphere(altitude: float) -> AtmosphereState:
    """Simple isothermal atmosphere for testing."""
    T = 250.0  # Constant temperature
    p = 101325 * math.exp(-altitude / 7400)  # Scale height 7.4 km
    rho = p / (287.05 * T)
    return AtmosphereState(temperature=T, pressure=p, density=rho)

state = solve(mach=7.0, altitude=30_000, atm=isothermal_atmosphere)
```
