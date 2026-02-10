# Gas Properties

The thermodynamic state of a gas is characterized by:

| Property | Symbol | Units | Description |
|----------|--------|-------|-------------|
| Specific gas constant | \(R\) | J/(kg·K) | \(R = R_u / M\) where \(R_u = 8314\) J/(kmol·K) |
| Specific heat ratio | \(\gamma\) | — | \(\gamma = c_p / c_v\) |
| Specific heat (const. pressure) | \(c_p\) | J/(kg·K) | Energy to raise temperature at constant pressure |
| Specific heat (const. volume) | \(c_v\) | J/(kg·K) | Energy to raise temperature at constant volume |

These are related by:

\[
R = c_p - c_v, \qquad \gamma = \frac{c_p}{c_v}, \qquad c_p = \frac{\gamma R}{\gamma - 1}, \qquad c_v = \frac{R}{\gamma - 1}
\]

For a **perfect gas**, these properties are constant. For **real gases** at high temperatures, they become temperature-dependent — see [Gas Models](gas-models.md).

## Standard Values

| Gas | \(R\) [J/(kg·K)] | \(\gamma\) | \(c_p\) [J/(kg·K)] | \(c_v\) [J/(kg·K)] |
|-----|------------------|------------|--------------------|--------------------|
| Air | 287.05 | 1.4 | 1004.7 | 717.6 |
| N₂  | 296.8  | 1.4 | 1039.8 | 742.9 |

