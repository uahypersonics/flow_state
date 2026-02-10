# Gas Models

`flow_state` provides three gas models with increasing complexity for different temperature regimes. See [Gas Properties](gas-properties.md) for definitions of \(R\), \(\gamma\), \(c_p\), \(c_v\).

## Overview

| Model | Class | Valid Range | Real-Gas Effects |
|-------|-------|-------------|------------------|
| [Perfect Gas](#perfect-gas) | `PerfectGas` | All | None (constant γ, R) |
| [Park](#park) | `ParkGas` | ~800–2500 K | Vibrational excitation |
| [Equilibrium Air](#equilibrium-air) | `EquilibriumAir` | 300–15000 K | Vibration, dissociation, ionization |

## Perfect Gas

Constant \(\gamma\) and \(R\). Uses the ideal gas law \(p = \rho R T\).

Use when temperatures are below ~800 K or when real-gas effects are negligible.

## Park

Harmonic oscillator model that captures the decrease in \(\gamma\) as vibrational modes become excited:

\[
c_v = c_{v,\text{trans}} + c_{v,\text{rot}} + c_{v,\text{vib}}(T)
\]

The vibrational contribution uses characteristic temperatures:

- \(\theta_{\text{vib,N}_2} = 3395\) K
- \(\theta_{\text{vib,O}_2} = 2239\) K

\[
c_{v,\text{vib}} = R \left( \frac{\theta_{\text{vib}}}{T} \right)^2 \frac{e^{\theta_{\text{vib}}/T}}{(e^{\theta_{\text{vib}}/T} - 1)^2}
\]

Use for moderate high-temperature flows (~800–2500 K) where vibrational excitation matters but dissociation has not yet begun.

## Equilibrium Air

Tannehill curve fits for air in chemical equilibrium, accounting for:

- Vibrational excitation of N₂, O₂
- Dissociation: O₂ ↔ 2O (starts ~2500 K), N₂ ↔ 2N (starts ~4000 K)
- NO formation
- Ionization (>9000 K)

The effective gas constant increases with dissociation (lower average molecular weight):

\[
R_{\text{eff}}(T, p) = Z(T, p) \cdot R_{\text{cold}}
\]

where \(Z\) is the compressibility factor from curve fits.

Valid range: 300 K < T < 15000 K, 10⁻⁴ atm < p < 100 atm.

Use for high-enthalpy flows where dissociation and ionization are significant.
