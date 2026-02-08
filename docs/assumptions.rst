Assumptions
===========

``flow_state`` is built on several simplifying assumptions that make it
suitable for a wide range of aerodynamic analyses while keeping the code
lightweight and easy to use.

Calorically Perfect Gas
-----------------------

The gas models assume a **calorically perfect gas**, meaning:

- Specific heat ratio (γ) is constant
- Specific gas constant (R) is constant
- Specific heats (cp, cv) are constant

This approximation is valid for:

- Air at temperatures below approximately 800 K
- Moderate Mach numbers where real-gas effects are negligible
- Conditions where dissociation and ionization do not occur

**Not suitable for:**

- High-temperature flows (T > 2000 K)
- Hypersonic re-entry conditions with significant chemistry
- Flows with combustion or chemical reactions


Sutherland Viscosity Law
------------------------

The transport model uses **Sutherland's viscosity law**:

.. math::

   \mu(T) = \mu_{ref} \left(\frac{T}{T_{ref}}\right)^{3/2} \frac{T_{ref} + S}{T + S}

Where:

- μ_ref: Reference viscosity at T_ref
- T_ref: Reference temperature
- S: Sutherland constant

This empirical correlation is valid for:

- Temperatures from approximately 100 K to 1900 K for air
- Single-species gases

**Default constants for air:**

- μ_ref = 1.716 × 10⁻⁵ Pa·s
- T_ref = 273.15 K
- S = 110.4 K

**Note:** Literature values vary slightly. If precise values are needed
for your application, consult NIST or other authoritative sources.


Prandtl Number
--------------

When a transport model is provided, a **constant Prandtl number** of 0.72
is assumed. This is typical for air-like gases at moderate temperatures.


Unit Reynolds Number
--------------------

The unit Reynolds number is computed as:

.. math::

   Re/m = \frac{\rho V}{\mu}

This represents the Reynolds number per unit length and is useful for
scaling and similarity analyses.
