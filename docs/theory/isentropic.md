# Isentropic Relations

For isentropic (adiabatic, reversible) flow, the stagnation properties relate to static properties through the Mach number.

## Stagnation Temperature

\[
\frac{T_0}{T} = 1 + \frac{\gamma - 1}{2} M^2
\]

## Stagnation Pressure

\[
\frac{p_0}{p} = \left( 1 + \frac{\gamma - 1}{2} M^2 \right)^{\frac{\gamma}{\gamma - 1}}
\]

## Stagnation Density

\[
\frac{\rho_0}{\rho} = \left( 1 + \frac{\gamma - 1}{2} M^2 \right)^{\frac{1}{\gamma - 1}}
\]

These relations are implemented in `flow_state.isentropic` and used by the solvers to convert between stagnation and static conditions.
