# Examples

Practical examples of using `flow_state` for common tasks.

## Wind Tunnel Conditions

### BAM6QT Quiet Tunnel

The Boeing/AFOSR Mach 6 Quiet Tunnel at Purdue:

```python
from flow_state import solve

# BAM6QT typical quiet conditions
state = solve(mach=6.0, re1=10.4e6, temp=52.8)

print(f"Mach:     {state.mach}")
print(f"Re/m:     {state.re1:.2e}")
print(f"T:        {state.temp:.1f} K")
print(f"p:        {state.pres:.1f} Pa")
print(f"rho:      {state.dens:.4f} kg/m³")
print(f"U:        {state.uvel:.1f} m/s")
```

### Comparing Conditions

```python
from flow_state import solve

# Same Mach, different Reynolds numbers
conditions = [
    {"re1": 5e6, "label": "Low Re"},
    {"re1": 10e6, "label": "Medium Re"},
    {"re1": 20e6, "label": "High Re"},
]

for cond in conditions:
    state = solve(mach=6.0, re1=cond["re1"], temp=65.0)
    print(f"{cond['label']:12s}: p = {state.pres:8.1f} Pa")
```

## Flight Conditions

### Altitude Sweep

```python
from flow_state import solve

altitudes = [20_000, 30_000, 40_000, 50_000, 60_000]  # meters

print("Alt [km]    T [K]      p [Pa]     Re/m")
print("-" * 45)

for alt in altitudes:
    state = solve(mach=7.0, altitude=alt, atm="ussa76")
    print(f"{alt/1000:6.0f}    {state.temp:7.1f}    {state.pres:9.1f}    {state.re1:.2e}")
```

### Trajectory Point

```python
from flow_state import solve, atmosphere

# Reentry vehicle at specific trajectory point
mach = 15.0
altitude = 50_000  # 50 km

# Using latitude-dependent atmosphere for polar trajectory
cira = atmosphere.CIRA86(latitude=70, month=1)
state = solve(mach=mach, altitude=altitude, atm=cira)

print(f"Trajectory Point: M={mach}, h={altitude/1000} km")
print(f"Velocity:         {state.uvel:.0f} m/s")
print(f"Stag. temp:       {state.temp_stag:.0f} K")
```

## CFD Preprocessing

### Generating Boundary Conditions

```python
from flow_state import solve

# Define simulation conditions
state = solve(mach=7.0, altitude=30_000, atm="ussa76")

# Access values for your solver
print(f"# Freestream boundary conditions")
print(f"mach = {state.mach}")
print(f"pres = {state.pres}  # Pa")
print(f"temp = {state.temp}  # K")
print(f"dens = {state.dens}  # kg/m³")
```

## Batch Processing

### Parameter Study

```python
from flow_state import solve

# Mach number sweep
machs = [5.0, 6.0, 7.0, 8.0, 10.0]
altitude = 30_000

print("Mach    U [m/s]    Re/m       T0 [K]")
print("-" * 45)

for mach in machs:
    state = solve(mach=mach, altitude=altitude, atm="ussa76")
    print(f"{mach:4.1f}    {state.uvel:7.1f}    {state.re1:.2e}    {state.temp_stag:.0f}")
```

## Comparison with Literature

### Verify Against Anderson

From Anderson's *Hypersonic and High-Temperature Gas Dynamics*:

```python
from flow_state import solve, isentropic

# Given: M=10, altitude 30 km (USSA76)
state = solve(mach=10.0, altitude=30_000, atm="ussa76")

print(f"Freestream conditions at M=10, h=30 km:")
print(f"  T = {state.temp:.2f} K")
print(f"  p = {state.pres:.2f} Pa")
print(f"  U = {state.uvel:.1f} m/s")

print(f"\nStagnation conditions:")
print(f"  T0 = {state.temp_stag:.1f} K")
print(f"  p0 = {state.pres_stag:.0f} Pa")

print(f"\nIsentropic ratios:")
print(f"  T0/T = {isentropic.temperature_ratio(10.0, 1.4):.2f}")
print(f"  p0/p = {isentropic.pressure_ratio(10.0, 1.4):.1f}")
```
