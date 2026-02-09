# Atmosphere Models

`flow_state` includes standard atmosphere models to compute pressure, temperature, and density at a given altitude.

## Available Models

### USSA76 (US Standard Atmosphere 1976)

The default model. Valid from 0 to 86 km altitude.

```python
from flow_state import solve

state = solve(mach=7.0, altitude=30_000, atm="ussa76")
```

### CIRA86 (COSPAR International Reference Atmosphere)

Latitude and season-dependent model. Valid from 0 to 120 km altitude.

```python
from flow_state import solve, atmosphere

# Using string with defaults (45°N, January)
state = solve(mach=7.0, altitude=30_000, atm="cira86")

# With specific latitude and month
cira = atmosphere.CIRA86(latitude=70, month=7)  # 70°N, July
state = solve(mach=7.0, altitude=30_000, atm=cira)
```

CIRA86 parameters:

- `latitude`: Degrees North (0-80°, in 10° increments)
- `month`: 1-12 (January = 1)

## Direct Atmosphere Access

You can query atmosphere models directly:

```python
from flow_state import atmosphere

# USSA76
atm_state = atmosphere.ussa76(30_000)
print(f"T = {atm_state.temperature:.1f} K")
print(f"p = {atm_state.pressure:.1f} Pa")
print(f"ρ = {atm_state.density:.4f} kg/m³")

# CIRA86
cira = atmosphere.CIRA86(latitude=70, month=1)
atm_state = cira(30_000)
```

## Using Atmosphere in TOML Config

In configuration files, specify the atmosphere model:

```toml
mach = 7.0
altitude = 30000
atm = "ussa76"
```

For CIRA86 with custom parameters:

```toml
mach = 7.0
altitude = 30000

[atmosphere]
model = "cira86"
latitude = 70
month = 7
```

See [Config Files](config-files.md) for more details.

## Custom Atmosphere Models

You can pass any callable that takes altitude and returns an `AtmosphereState`:

```python
from flow_state import solve
from flow_state.atmosphere import AtmosphereState

def my_atmosphere(altitude: float) -> AtmosphereState:
    """Custom exponential atmosphere."""
    t = 288.15 - 0.0065 * altitude
    p = 101325 * (t / 288.15) ** 5.256
    rho = p / (287.05 * t)
    return AtmosphereState(temperature=t, pressure=p, density=rho)

state = solve(mach=7.0, altitude=10_000, atm=my_atmosphere)
```

## Comparison of Models

| Model  | Altitude Range | Latitude Dependent | Season Dependent |
|--------|----------------|-------------------|------------------|
| USSA76 | 0–86 km        | No                | No               |
| CIRA86 | 0–120 km       | Yes (0–80°N)      | Yes (monthly)    |

For most CFD applications in the lower atmosphere (< 50 km), USSA76 is sufficient. Use CIRA86 when:

- Operating at high altitudes (> 50 km)
- Latitude effects matter (polar vs. equatorial)
- Seasonal variations are important
