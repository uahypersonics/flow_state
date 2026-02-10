# Definitions

Key dimensionless parameters computed by `flow_state`.

## Speed of Sound

The speed of sound in a perfect gas:

\[
a = \sqrt{\gamma R T}
\]

See [Gas Properties](gas-properties.md) for values of \(\gamma\) and \(R\).

## Mach Number

The ratio of flow velocity to the local speed of sound:

\[
M = \frac{U}{a}
\]

## Reynolds Number

The unit Reynolds number (Reynolds number per unit length):

\[
\text{Re}_1 = \frac{\rho U}{\mu}
\]

where \(\mu\) is the dynamic viscosity. See [Transport Models](transport.md) for viscosity laws.

The Reynolds number based on a reference length \(L\):

\[
\text{Re}_L = \text{Re}_1 \cdot L = \frac{\rho U L}{\mu}
\]
