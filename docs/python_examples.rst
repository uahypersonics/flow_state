Python Examples
===============

These examples demonstrate how to use ``flow_state`` as a Python library.


Example 1: Basic Flow State
---------------------------

Compute a basic flow state from Mach number, pressure, and temperature:

.. code-block:: python

   from flow_state import from_M_p_T
   from flow_state.gas import PerfectGas
   from flow_state.transport import Sutherland

   # Create gas and transport models
   air = PerfectGas.air()
   sutherland = Sutherland.air()

   # Build flow state
   state = from_M_p_T(
       M=2.0,
       p=101325,  # Pa
       T=300,     # K
       gas=air,
       transport=sutherland,
   )

   # Access properties
   print(f"Velocity: {state.V:.2f} m/s")
   print(f"Density: {state.rho:.4f} kg/m³")
   print(f"Speed of sound: {state.a:.2f} m/s")
   print(f"Dynamic viscosity: {state.mu:.6e} Pa·s")
   print(f"Unit Reynolds number: {state.Re_per_m:.2e} 1/m")


Example 2: Using from_pT with Mach
----------------------------------

Build a state from pressure and temperature, optionally specifying Mach:

.. code-block:: python

   from flow_state import from_pT
   from flow_state.gas import PerfectGas

   air = PerfectGas.air()

   # Without Mach - just thermodynamic properties
   state1 = from_pT(p=50000, T=250, gas=air)
   print(f"Density: {state1.rho:.4f} kg/m³")
   print(f"Speed of sound: {state1.a:.2f} m/s")

   # With Mach - also computes velocity
   state2 = from_pT(p=50000, T=250, gas=air, M=3.0)
   print(f"Mach: {state2.M}")
   print(f"Velocity: {state2.V:.2f} m/s")


Example 3: TOML Serialization
-----------------------------

Save and load flow states:

.. code-block:: python

   from flow_state import from_M_p_T
   from flow_state.core import FlowState
   from flow_state.gas import PerfectGas
   from flow_state.transport import Sutherland

   # Create a state
   state = from_M_p_T(
       M=5.0,
       p=5000,
       T=220,
       gas=PerfectGas.air(),
       transport=Sutherland.air(),
       notes="High-altitude hypersonic condition",
   )

   # Save to TOML
   state.to_toml("my_condition.toml")

   # Load from TOML
   loaded = FlowState.from_toml("my_condition.toml")
   print(f"Loaded Mach: {loaded.M}")
   print(f"Notes: {loaded.notes}")


Example 4: Custom Gas Properties
--------------------------------

Define a custom gas with specific γ and R:

.. code-block:: python

   from flow_state import from_M_p_T
   from flow_state.gas import PerfectGas
   from flow_state.transport import Sutherland

   # Custom gas (e.g., a mixture or different species)
   custom_gas = PerfectGas.custom(
       gamma=1.3,
       R=260.0,  # J/(kg·K)
       name="custom_mixture",
   )

   # Custom transport (optional)
   custom_transport = Sutherland.custom(
       mu_ref=1.5e-5,
       T_ref=273.15,
       S=100.0,
       name="custom_transport",
   )

   state = from_M_p_T(
       M=2.0,
       p=101325,
       T=300,
       gas=custom_gas,
       transport=custom_transport,
   )

   print(f"Gas model: {state.gas_model}")
   print(f"gamma: {state.gamma}")
   print(f"R: {state.R} J/(kg·K)")


Example 5: Legacy Output Format
-------------------------------

Generate the legacy ``flow_conditions.dat`` file:

.. code-block:: python

   from flow_state import from_M_p_T
   from flow_state.gas import PerfectGas
   from flow_state.transport import Sutherland
   from flow_state.io import write_flow_conditions_dat

   state = from_M_p_T(
       M=2.0,
       p=101325,
       T=300,
       gas=PerfectGas.air(),
       transport=Sutherland.air(),
   )

   # Write legacy format
   write_flow_conditions_dat(state, "flow_conditions.dat")


Example 6: Accessing Derived Properties
---------------------------------------

The FlowState dataclass computes several derived properties automatically:

.. code-block:: python

   from flow_state import from_M_p_T
   from flow_state.gas import PerfectGas
   from flow_state.transport import Sutherland

   state = from_M_p_T(
       M=2.0,
       p=101325,
       T=300,
       gas=PerfectGas.air(),
       transport=Sutherland.air(),
   )

   # Derived thermodynamic properties
   print(f"cp: {state.cp:.2f} J/(kg·K)")
   print(f"cv: {state.cv:.2f} J/(kg·K)")

   # Transport-derived properties
   print(f"Kinematic viscosity: {state.nu:.6e} m²/s")
   print(f"Prandtl number: {state.Pr}")
   print(f"Unit Reynolds number: {state.Re_per_m:.2e} 1/m")

   # Print full summary
   print(state.summary())
