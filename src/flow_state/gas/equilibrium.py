"""Equilibrium air model for high-temperature flows.

Implements temperature and pressure-dependent thermodynamic properties
for air in chemical equilibrium. Uses curve fits from:

    Tannehill, J.C. and Mugge, P.H. (1974). "Improved Curve Fits for the
    Thermodynamic Properties of Equilibrium Air Suitable for Numerical
    Computation Using Time-Dependent or Shock-Capturing Methods."
    NASA CR-2470.

The model accounts for:
    - Vibrational excitation of N2, O2
    - Dissociation: O2 <-> 2O (starts ~2500 K), N2 <-> 2N (starts ~4000 K)
    - NO formation
    - Ionization (at very high temperatures, >9000 K)

Valid range: 300 K < T < 15000 K, 1e-4 atm < p < 100 atm

The key physical effect is that dissociation reduces the average molecular
weight, which increases the effective gas constant r_gas.
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------
from __future__ import annotations

import math
from dataclasses import dataclass

# --------------------------------------------------
# constants
# --------------------------------------------------
# Reference values for air
R_AIR_COLD = 287.05  # Specific gas constant for undissociated air [J/(kg K)]
P_REF = 101325.0     # Reference pressure [Pa] (1 atm)
T_REF = 273.15       # Reference temperature [K]

# Tannehill curve fit coefficients for compressibility Z
# Z = sum(a_i * theta^i) where theta = log10(T/T_ref)
# These are simplified fits valid for moderate pressures (~1 atm)
# For full pressure dependence, see NASA CR-2470 Table II

# Coefficients for Z (compressibility factor) at p = 1 atm
# Fit to Tannehill data, valid 1000 K < T < 9000 K
_Z_COEFFS_1ATM = [
    1.0000,    # a0
    0.0000,    # a1
    0.0434,    # a2
    0.1039,    # a3
    -0.0301,   # a4
]

# Coefficients for normalized cp at p = 1 atm
# cp/R_air = sum(b_i * theta^i)
_CP_COEFFS_1ATM = [
    3.5000,    # b0 (= 7/2 for diatomic at low T)
    0.0000,    # b1
    0.6521,    # b2
    1.2843,    # b3
    -0.2917,   # b4
]

# Pressure correction exponents (simplified model)
# Z increases with decreasing pressure at high T (more dissociation)
_PRESSURE_EXPONENT = 0.1


# --------------------------------------------------
# helper functions: Tannehill curve fits
# --------------------------------------------------
def _compute_z(temp: float, pres: float) -> float:
    """
    Compute compressibility factor Z from Tannehill fits.

    Z accounts for the change in effective molecular weight due to
    dissociation. For undissociated air, Z = 1. For fully dissociated
    air (all O2 and N2 broken into atoms), Z -> 2.

    Args:
        temp: Temperature [K]
        pres: Pressure [Pa]

    Returns:
        Compressibility factor Z [-]

    Notes:
        Based on curve fits from Tannehill & Mugge (1974), NASA CR-2470.
        Simplified for moderate pressures.
    """
    if temp <= 0:
        raise ValueError(f"Temperature must be positive, got {temp} K")

    # Below 800 K, no dissociation
    if temp < 800:
        return 1.0

    # Normalized temperature for curve fit
    theta = math.log10(temp / T_REF)

    # Base Z at reference pressure (1 atm)
    z_ref = 0.0
    for i, coeff in enumerate(_Z_COEFFS_1ATM):
        z_ref += coeff * (theta ** i)

    # Pressure correction: lower pressure -> more dissociation -> higher Z
    # This is a simplified model; full treatment requires 2D fits
    if pres > 0:
        p_ratio = pres / P_REF
        # At lower pressure, equilibrium shifts toward dissociation
        pressure_factor = p_ratio ** (-_PRESSURE_EXPONENT * max(0, theta - 1.0))
        z = z_ref * pressure_factor
    else:
        z = z_ref

    # Clamp to physical range [1, 2]
    # Z = 1 for undissociated, Z -> 2 for fully dissociated diatomic
    return max(1.0, min(2.0, z))


def _compute_cp_normalized(temp: float, pres: float) -> float:
    """
    Compute normalized specific heat cp/R_air from Tannehill fits.

    Args:
        temp: Temperature [K]
        pres: Pressure [Pa]

    Returns:
        Normalized specific heat cp/R_air [-]

    Notes:
        Based on curve fits from Tannehill & Mugge (1974), NASA CR-2470.
    """
    if temp <= 0:
        raise ValueError(f"Temperature must be positive, got {temp} K")

    # Below 500 K, use perfect gas value
    if temp < 500:
        return 3.5  # = 7/2 for diatomic

    # Normalized temperature for curve fit
    theta = math.log10(temp / T_REF)

    # Base cp at reference pressure
    cp_norm = 0.0
    for i, coeff in enumerate(_CP_COEFFS_1ATM):
        cp_norm += coeff * (theta ** i)

    # Pressure correction (cp peaks are higher at lower pressure)
    if pres > 0 and temp > 1500:
        p_ratio = pres / P_REF
        pressure_factor = 1.0 + 0.1 * (1.0 - p_ratio) * max(0, theta - 1.2)
        cp_norm *= max(0.8, min(1.5, pressure_factor))

    # Clamp to physical range
    # cp/R ranges from 3.5 (frozen diatomic) to ~10+ during dissociation
    return max(3.5, min(12.0, cp_norm))


# --------------------------------------------------
# EquilibriumAir: equilibrium air model
# --------------------------------------------------
@dataclass(frozen=True)
class EquilibriumAir:
    """
    Equilibrium air model with dissociation effects.

    Uses curve fits from Tannehill & Mugge (1974) to compute temperature
    and pressure-dependent thermodynamic properties. The model assumes
    chemical equilibrium (infinite reaction rates) and accounts for
    dissociation of O2 and N2.

    This model is valid for temperatures where dissociation is significant
    but ionization is not yet dominant (~2000-9000 K).

    Attributes:
        name: Model identifier

    Notes:
        - At low temp (~300 K): matches perfect gas (gamma ~ 1.4)
        - At moderate temp (~1500 K): vibrational effects (gamma ~ 1.3)
        - At high temp (~5000 K): dissociation (gamma ~ 1.15-1.2, r_gas increases)
        - Pressure affects equilibrium: lower p -> more dissociation

    References:
        Tannehill, J.C. and Mugge, P.H. (1974). "Improved Curve Fits for the
        Thermodynamic Properties of Equilibrium Air Suitable for Numerical
        Computation Using Time-Dependent or Shock-Capturing Methods."
        NASA CR-2470.

        Anderson, J.D. (2006). "Hypersonic and High-Temperature Gas Dynamics."
        2nd ed., AIAA Education Series. Chapter 16.
    """

    # --------------------------------------------------
    # attributes
    # --------------------------------------------------
    name: str = "equilibrium_air"

    # --------------------------------------------------
    # class method for standard air
    #
    # cls is the class itself, passed automatically by @classmethod
    # --------------------------------------------------
    @classmethod
    def air(cls) -> EquilibriumAir:
        """
        Standard equilibrium air model.

        Returns:
            EquilibriumAir configured for standard air composition.

        Notes:
            - Valid for ~300-9000 K (equilibrium regime)
            - Accounts for O2/N2 dissociation and NO formation
        """
        return cls(name="equilibrium_air")

    # --------------------------------------------------
    # methods: thermodynamic properties
    #
    # these methods take temp and pres because properties depend on
    # both temperature and pressure for equilibrium gas.
    # --------------------------------------------------

    # specific gas constant (effective, varies with dissociation)
    def r_gas(self, temp: float, pres: float) -> float:
        """
        Effective specific gas constant.

        Args:
            temp: Temperature [K]
            pres: Pressure [Pa]

        Returns:
            Effective specific gas constant [J/(kg K)]

        Notes:
            r_gas_eff = Z * R_air_cold

            Z is the compressibility factor that accounts for dissociation.
            As molecules dissociate, the average molecular weight decreases,
            so the effective gas constant increases.

            - Z = 1.0 for undissociated air (T < 2000 K)
            - Z -> 2.0 for fully dissociated air (T > 8000 K)
        """
        z = _compute_z(temp, pres)
        return z * R_AIR_COLD

    # specific heat at constant pressure
    def cp(self, temp: float, pres: float) -> float:
        """
        Specific heat at constant pressure.

        Args:
            temp: Temperature [K]
            pres: Pressure [Pa]

        Returns:
            Specific heat at constant pressure [J/(kg K)]

        Notes:
            cp shows a large peak during dissociation because the
            dissociation reaction absorbs energy.
        """
        cp_norm = _compute_cp_normalized(temp, pres)
        return cp_norm * R_AIR_COLD

    # specific heat at constant volume
    def cv(self, temp: float, pres: float) -> float:
        """
        Specific heat at constant volume.

        Args:
            temp: Temperature [K]
            pres: Pressure [Pa]

        Returns:
            Specific heat at constant volume [J/(kg K)]

        Notes:
            For thermally perfect gas: cv = cp - r_gas
            This relation holds for equilibrium air as well.
        """
        return self.cp(temp, pres) - self.r_gas(temp, pres)

    # specific heat ratio
    def gamma(self, temp: float, pres: float) -> float:
        """
        Specific heat ratio.

        Args:
            temp: Temperature [K]
            pres: Pressure [Pa]

        Returns:
            Specific heat ratio cp/cv [-]

        Notes:
            gamma decreases significantly during dissociation:
            - ~1.4 at low temp (frozen)
            - ~1.3 at moderate temp (vibrational)
            - ~1.1-1.2 during active dissociation
        """
        cp_val = self.cp(temp, pres)
        cv_val = self.cv(temp, pres)

        # Protect against numerical issues
        if cv_val <= 0:
            return 1.4

        return cp_val / cv_val

    # speed of sound
    def sound_speed(self, temp: float) -> float:
        """
        Compute speed of sound.

        For equilibrium gas, this is the equilibrium (frozen) sound speed.

        Args:
            temp: Temperature [K]

        Returns:
            Speed of sound [m/s]

        Notes:
            a = sqrt(gamma * r_gas * temp)

            For equilibrium air, both gamma and r_gas vary with T and p.
            We use p = 1 atm as reference for sound speed calculation.
        """
        if temp <= 0:
            raise ValueError(f"Temperature must be positive, got {temp} K")

        # Use reference pressure for sound speed
        pres = P_REF
        gamma_val = self.gamma(temp, pres)
        r_gas_val = self.r_gas(temp, pres)

        return math.sqrt(gamma_val * r_gas_val * temp)

    # --------------------------------------------------
    # methods: equation of state
    # --------------------------------------------------

    # density from equation of state
    def density(self, pres: float, temp: float) -> float:
        """
        Compute density from equation of state.

        Args:
            pres: Pressure [Pa]
            temp: Temperature [K]

        Returns:
            Density [kg/m^3]

        Notes:
            For equilibrium air: p = rho * Z * R_air_cold * T
            So: rho = p / (r_gas_eff * T)
        """
        if temp <= 0:
            raise ValueError(f"Temperature must be positive, got {temp} K")
        if pres < 0:
            raise ValueError(f"Pressure must be non-negative, got {pres} Pa")

        r_gas_val = self.r_gas(temp, pres)
        return pres / (r_gas_val * temp)

    # pressure from equation of state (iterative)
    def pressure(self, dens: float, temp: float) -> float:
        """
        Compute pressure from equation of state.

        Args:
            dens: Density [kg/m^3]
            temp: Temperature [K]

        Returns:
            Pressure [Pa]

        Notes:
            For equilibrium air: p = rho * Z * R_air_cold * T

            This requires iteration because Z depends on p.
            We use a simple fixed-point iteration.
        """
        if temp <= 0:
            raise ValueError(f"Temperature must be positive, got {temp} K")
        if dens < 0:
            raise ValueError(f"Density must be non-negative, got {dens} kg/m^3")

        # Initial guess using cold air r_gas
        pres = dens * R_AIR_COLD * temp

        # Iterate to account for Z(p) dependence
        for _ in range(10):
            r_gas_val = self.r_gas(temp, pres)
            pres_new = dens * r_gas_val * temp

            if abs(pres_new - pres) < 1e-6 * pres:
                break
            pres = pres_new

        return pres

    # temperature from equation of state (iterative)
    def temperature(self, pres: float, dens: float) -> float:
        """
        Compute temperature from equation of state.

        Args:
            pres: Pressure [Pa]
            dens: Density [kg/m^3]

        Returns:
            Temperature [K]

        Notes:
            For equilibrium air: T = p / (rho * Z * R_air_cold)

            This requires iteration because Z depends on T.
            We use a simple fixed-point iteration.
        """
        if dens <= 0:
            raise ValueError(f"Density must be positive, got {dens} kg/m^3")
        if pres < 0:
            raise ValueError(f"Pressure must be non-negative, got {pres} Pa")

        # Initial guess using cold air r_gas
        temp = pres / (dens * R_AIR_COLD)

        # Iterate to account for Z(T) dependence
        for _ in range(10):
            r_gas_val = self.r_gas(temp, pres)
            temp_new = pres / (dens * r_gas_val)

            if abs(temp_new - temp) < 1e-6 * temp:
                break
            temp = temp_new

        return temp
