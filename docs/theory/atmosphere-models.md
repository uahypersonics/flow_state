# Atmosphere Models

`flow_state` includes standard atmosphere models to compute pressure, temperature, and density at a given altitude.

For usage examples, see [Examples](../examples.md#flight-conditions) and [Config Files](../user-guide/config-files.md).

## Available Models

### USSA76 (US Standard Atmosphere 1976)

The default model. Valid from 0 to 86 km altitude. Defines a single reference atmosphere independent of latitude and season.

### CIRA86 (COSPAR International Reference Atmosphere)

Latitude and season-dependent model. Valid from 0 to 120 km altitude.

Parameters:

- `latitude`: Degrees North (0-80°, in 10° increments)
- `month`: 1-12 (January = 1)

## Comparison

| Model  | Altitude Range | Latitude Dependent | Season Dependent |
|--------|----------------|-------------------|------------------|
| USSA76 | 0-86 km        | No                | No               |
| CIRA86 | 0-120 km       | Yes (0-80°N)      | Yes (monthly)    |

## When to Use Which

**USSA76** is sufficient for:

- Most CFD applications in the lower atmosphere (< 50 km)
- General-purpose calculations
- When latitude/season effects are negligible

**CIRA86** is preferred when:

- Operating at high altitudes (> 50 km)
- Latitude effects matter (polar vs. equatorial trajectories)
- Seasonal variations are important
