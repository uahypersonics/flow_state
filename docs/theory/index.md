# Theory

This page summarizes the equations used in `flow_state`. The relations below assume a calorically perfect gas with constant specific heat ratio \(\gamma\). Temperature-dependent gas models (Park vibrational excitation, Tannehill equilibrium air) are also available.

See also:

- [Transport Models](transport.md): Viscosity laws (Sutherland, Keyes, etc.)
- [Atmosphere Models](atmosphere-models.md): USSA76 and CIRA86

## Ideal Gas Law

The equation of state for a perfect gas:

\[
p = \rho R T
\]

where:

- \(p\) = pressure [Pa]
- \(\rho\) = density [kg/m^3]
- \(R\) = specific gas constant [J/(kg-K)]
- \(T\) = temperature [K]

For air: \(R = 287.05\) J/(kg-K)

## Speed of Sound

The speed of sound in a perfect gas:

\[
a = \sqrt{\gamma R T}
\]

## Mach Number

The Mach number is the ratio of flow velocity to the local speed of sound:

\[
M = \frac{U}{a}
\]

## Dynamic Pressure

The dynamic pressure represents the kinetic energy per unit volume:

\[
q = \frac{1}{2} \rho U^2
\]

This can also be written in terms of Mach number:

\[
q = \frac{1}{2} \gamma p M^2
\]

## Reynolds Number

The unit Reynolds number (Reynolds number per unit length):

\[
\text{Re}_1 = \frac{\rho U}{\mu} = \frac{\rho U L}{\mu L} = \frac{\text{Re}}{L}
\]

where \(\mu\) is the dynamic viscosity. See [Transport Models](transport.md) for viscosity laws.

## Isentropic Relations

For isentropic (adiabatic, reversible) flow, the stagnation properties relate to static properties through the Mach number.

### Stagnation Temperature

\[
\frac{T_0}{T} = 1 + \frac{\gamma - 1}{2} M^2
\]

### Stagnation Pressure

\[
\frac{p_0}{p} = \left( 1 + \frac{\gamma - 1}{2} M^2 \right)^{\frac{\gamma}{\gamma - 1}}
\]

### Stagnation Density

\[
\frac{\rho_0}{\rho} = \left( 1 + \frac{\gamma - 1}{2} M^2 \right)^{\frac{1}{\gamma - 1}}
\]

## Working with Stagnation Conditions

For a Mach 7 flow:

\[
\frac{T_0}{T} = 1 + \frac{0.4}{2} (7)^2 = 1 + 9.8 = 10.8
\]

So if \(T = 200\) K, then \(T_0 = 2160\) K.

\[
\frac{p_0}{p} = (10.8)^{3.5} \approx 4140
\]

These high stagnation values at hypersonic speeds are why:

1. Wind tunnels often use heated, high-pressure supply air
2. Flight vehicles experience extreme heating at leading edges

## Normal Shock Relations

Across a normal shock wave, the upstream (1) and downstream (2) conditions are related by:

### Pressure Ratio

\[
\frac{p_2}{p_1} = \frac{2\gamma M_1^2 - (\gamma - 1)}{\gamma + 1}
\]

### Temperature Ratio

\[
\frac{T_2}{T_1} = \frac{[2\gamma M_1^2 - (\gamma - 1)][(\gamma - 1)M_1^2 + 2]}{(\gamma + 1)^2 M_1^2}
\]

### Density Ratio

\[
\frac{\rho_2}{\rho_1} = \frac{(\gamma + 1) M_1^2}{(\gamma - 1) M_1^2 + 2}
\]

### Downstream Mach Number

\[
M_2^2 = \frac{(\gamma - 1) M_1^2 + 2}{2\gamma M_1^2 - (\gamma - 1)}
\]

### Stagnation Pressure Ratio

The loss in stagnation pressure across a shock:

\[
\frac{p_{02}}{p_{01}} = \left[ \frac{(\gamma + 1) M_1^2}{(\gamma - 1) M_1^2 + 2} \right]^{\frac{\gamma}{\gamma - 1}} \left[ \frac{\gamma + 1}{2\gamma M_1^2 - (\gamma - 1)} \right]^{\frac{1}{\gamma - 1}}
\]

## US Standard Atmosphere 1976

The USSA76 model divides the atmosphere into layers with defined lapse rates:

| Layer | Base Altitude | Base Temp | Lapse Rate |
|-------|---------------|-----------|------------|
| Troposphere | 0 km | 288.15 K | -6.5 K/km |
| Tropopause | 11 km | 216.65 K | 0 K/km |
| Stratosphere 1 | 20 km | 216.65 K | +1.0 K/km |
| Stratosphere 2 | 32 km | 228.65 K | +2.8 K/km |
| Stratopause | 47 km | 270.65 K | 0 K/km |
| Mesosphere 1 | 51 km | 270.65 K | -2.8 K/km |
| Mesosphere 2 | 71 km | 214.65 K | -2.0 K/km |

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

where \(g_0 = 9.80665\) m/s^2 is standard gravity.

## References

1. Anderson, J. D. (2006). *Hypersonic and High-Temperature Gas Dynamics*. AIAA.
2. U.S. Standard Atmosphere, 1976. NOAA/NASA/USAF.
3. COSPAR International Reference Atmosphere (CIRA-86).
