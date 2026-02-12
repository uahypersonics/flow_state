# Transport Models

Viscosity models available in `flow_state`. All models compute [dynamic viscosity](https://www.grc.nasa.gov/www/k-12/airplane/viscosity.html) \(\mu\) as a function of temperature.

## Overview

| Model | Class | Gases | Reference |
|-------|-------|-------|-----------|
| [Sutherland's Law](#sutherland) | `Sutherland` | Air, N2, Custom | [@sutherland1893] |
| [Sutherland's Law with LTC](#sutherland_ltc) | `SutherlandLowTemp` | Air | [@mack1969] |
| [Sutherland's Law with LTC (Blended)](#sutherland_blended) | `SutherlandBlended` | Air | - |
| [Keyes](#keyes) | `Keyes` | Air, N2 | [@keyes1951] |
| [Power Law](#power-law) | `PowerLaw` | Air | [@white2006] |

## Sutherland's Law {#sutherland}

The standard Sutherland formula [@sutherland1893]:

\[
\mu(T) = \mu_{\text{ref}} \left( \frac{T}{T_{\text{ref}}} \right)^{3/2} \frac{T_{\text{ref}} + S}{T + S}
\]

Valid for moderate temperatures (~100 K to ~1900 K for air).

| Gas | \(\mu_{\text{ref}} \, [Pa \cdot s]\) | \(T_{\text{ref}} \, [K]\) | \(S \, [K]\) |
|-----|----------------------------|------------------------|-----------|
| Air | \(1.716 \times 10^{-5}\) | 273.15 | 110.4 |
| Nitrogen | \(1.663 \times 10^{-5}\) | 273.15 | 106.7 |

## Sutherland's Law with Low-Temperature Correction (LTC) {#sutherland_ltc}

Prevents unphysical viscosity at very low temperatures where Sutherland's law breaks down [@mack1969]:

\[
\mu(T) = \begin{cases}
C_0 T_1 & T < T_1 \\
C_0 T & T_1 \leq T \leq S \\
\mu_{\text{ref}} \left( \frac{T}{T_{\text{ref}}} \right)^{3/2} \frac{T_{\text{ref}} + S}{T + S} & T > S
\end{cases}
\]

where:

- \(\mu_{\text{ref}} = 1.716 \times 10^{-5}\) kg/(m·s)
- \(T_{\text{ref}} = 273.15\) K
- \(S = 110.4\) K
- \(C_0 = 6.93873 \times 10^{-8}\) kg/(m·s·K)
- \(T_1 = 40\) K

## Sutherland's Law with Low-Temperature Correction Blended (LTC-blended) {#sutherland_blended}

Smoother transition using 8th-order polynomial blending. This avoids discontinuities in the derivative that can cause numerical issues:

\[
\mu(T) = \begin{cases}
C_0 T & T < T_1 \\
a_0 \sum_{i=1}^{8} a_i \frac{T^{8-i}}{S^{8-i}} & T_1 \leq T \leq T_2 \\
\mu_{\text{ref}} \left( \frac{T}{T_{\text{ref}}} \right)^{3/2} \frac{T_{\text{ref}} + S}{T + S} & T > T_2
\end{cases}
\]

where:

- \(\mu_{\text{ref}} = 1.716 \times 10^{-5}\) kg/(m·s)
- \(T_{\text{ref}} = 273.15\) K
- \(S = 110.4\) K
- \(C_0 = 6.93873 \times 10^{-8}\) kg/(m·s·K)
- \(T_1 = 100\) K
- \(T_2 = 130\) K

Polynomial coefficients:

| \(i\) | \(a_i\) |
|-------|--------|
| 0 | \(7.659704848 \times 10^{-6}\) kg/(m·s) |
| 1 | \(-44.79148053679334\) |
| 2 | \(319.5188079744342\) |
| 3 | \(-971.6235566382709\) |
| 4 | \(1632.645086771892\) |
| 5 | \(-1637.375578884298\) |
| 6 | \(980.2775658900685\) |
| 7 | \(-323.4667180557399\) |
| 8 | \(45.8157988617632\) |

## Keyes Model {#keyes}

High-temperature viscosity law [@keyes1951] [@priebe2012] [@roy2006]:

\[
\mu(T) = \frac{a_0 \sqrt{T}}{1 + a_1 \cdot 10^{-a_2/T} / T}
\]

Often used for hypersonic flows where temperatures can be very high.

| Gas | \(a_0 [kg/(m \cdot s \cdot \sqrt{K})]\) | \(a_1 [K]\) | \(a_2 [-]\) |
|-----|-------------------------|-------------|-------------|
| Air | \(1.488 \times 10^{-6}\) | 122.1 | 5.0 |
| Nitrogen | \(1.418 \times 10^{-6}\) | 116.4 | 5.0 |

## Power Law {#power-law}

Simple power-law model [@white2006]:

\[
\mu(T) = \mu_{\text{ref}} \left( \frac{T}{T_{\text{ref}}} \right)^m
\]

Useful for simplified analyses or specific temperature ranges. Typical exponent \(m\) ranges from 0.5 to 1.0.

## Usage

All transport models provide three methods:

| Method | Returns | Description |
|--------|---------|-------------|
| `mu(temp)` | `float` | Dynamic viscosity \(\mu\) [Pa s] |
| `nu(temp, dens)` | `float` | Kinematic viscosity \(\nu = \mu / \rho\) [m^2/s] |
| `dmudt(temp)` | `float` | Temperature derivative \(d\mu/dT\) [Pa s / K] |

Example:

```python
from flow_state.transport import Sutherland

model = Sutherland.air()
T = 300.0  # K

mu = model.mu(T)           # 1.85e-5 Pa s
dmudt = model.dmudt(T)     # derivative for CFD applications
nu = model.nu(T, rho=1.2)  # kinematic viscosity
```

## References

\bibliography
