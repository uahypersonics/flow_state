# Atmosphere Models

`flow_state` includes standard atmosphere models to compute pressure, temperature, and density at a given altitude.

For usage examples, see [Examples](../examples.md#flight-conditions) and [Config Files](../user-guide/config-files.md).

## Overview

| Model | Function/Class | Altitude Range | Lat/Season | Reference |
|-------|----------------|----------------|------------|----------|
| [USSA76](#ussa76) | `ussa76()` | 0–86 km | No | [@ussa1976] |
| [CIRA86](#cira86) | `CIRA86()` | 0–120 km | Yes | [@cira1986] |

## US Standard Atmosphere 1976 (USSA76) {#ussa76}

The default model [@ussa1976]. Defines a single reference atmosphere independent of latitude and season.

- Valid from 0 to 86 km (geometric altitude)
- Piecewise linear temperature profile with defined lapse rates
- Assumes hydrostatic equilibrium and ideal gas

### Layer Structure

| Layer | Base Altitude | Base Temp | Lapse Rate |
|-------|---------------|-----------|------------|
| Troposphere | 0 km | 288.15 K | -6.5 K/km |
| Tropopause | 11 km | 216.65 K | 0 K/km |
| Stratosphere 1 | 20 km | 216.65 K | +1.0 K/km |
| Stratosphere 2 | 32 km | 228.65 K | +2.8 K/km |
| Stratopause | 47 km | 270.65 K | 0 K/km |
| Mesosphere 1 | 51 km | 270.65 K | -2.8 K/km |
| Mesosphere 2 | 71 km | 214.65 K | -2.0 K/km |

### Equations

For gradient layers (\(\lambda \neq 0\)):

\[
T = T_b + \lambda (h - h_b)
\]

\[
p = p_b \left( \frac{T}{T_b} \right)^{-g_0 / (\lambda R)}
\]

For isothermal layers (\(\lambda = 0\)):

\[
p = p_b \exp\left( -\frac{g_0 (h - h_b)}{R T_b} \right)
\]

where \(g_0 = 9.80665\) m/s² is standard gravity and \(R = 287.05\) J/(kg·K).

## COSPAR International Reference Atmosphere (CIRA86) {#cira86}

Latitude and season-dependent model [@cira1986].

- Valid from 0 to 120 km altitude
- Tabulated data interpolated for latitude (0–80°N, 10° increments) and month (1–12)
- More accurate for high-altitude and polar/equatorial trajectories

Parameters:

| Parameter | Range | Description |
|-----------|-------|-------------|
| `latitude` | 0–80° | Degrees North (10° increments) |
| `month` | 1–12 | Month of year (January = 1) |

## When to Use Which

**USSA76** is sufficient for:

- Most CFD applications in the lower atmosphere (< 50 km)
- General-purpose calculations
- When latitude/season effects are negligible

**CIRA86** is preferred when:

- Operating at high altitudes (> 50 km)
- Latitude effects matter (polar vs. equatorial trajectories)
- Seasonal variations are important

## References

\bibliography
