"""Sutherland viscosity models."""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------
from __future__ import annotations

from dataclasses import dataclass


# --------------------------------------------------
# Sutherland: standard Sutherland viscosity law
# --------------------------------------------------
@dataclass(frozen=True)
class Sutherland:
    """
    Standard Sutherland viscosity law for temperature-dependent dynamic viscosity.

    The Sutherland formula is:
        mu(T) = mu_ref * (T / T_ref)^(3/2) * (T_ref + S) / (T + S)

    Attributes:
        mu_ref: Reference viscosity [Pa s]
        T_ref: Reference temperature [K]
        S: Sutherland constant [K]
        name: Model identifier

    Notes:
        - Valid for moderate temperatures (~100 K to ~1900 K for air)
        - At very low temperatures, accuracy degrades (use SutherlandLowTemp)
    """

    # --------------------------------------------------
    # attributes
    # --------------------------------------------------
    mu_ref: float
    T_ref: float
    S: float
    name: str = "sutherland"

    # --------------------------------------------------
    # class methods for standard gases
    #
    # cls is the class itself (Sutherland), passed automatically
    # by @classmethod. Using cls(...) instead of Sutherland(...)
    # so it works correctly with subclasses.
    #
    # the classmethod is a convenient way to create preset models
    # example: Sutherland.air() returns a Sutherland instance for air
    #
    # currently implemented:
    # - air
    # - nitrogen
    # - custom
    # --------------------------------------------------
    @classmethod
    def air(cls) -> Sutherland:
        """Sutherland model for air (mu_ref=1.716e-5 at 273.15 K, S=110.4 K)."""
        return cls(mu_ref=1.716e-5, T_ref=273.15, S=110.4, name="sutherland_air")

    @classmethod
    def nitrogen(cls) -> Sutherland:
        """Sutherland model for nitrogen (mu_ref=1.663e-5 at 273.15 K, S=106.7 K)."""
        return cls(mu_ref=1.663e-5, T_ref=273.15, S=106.7, name="sutherland_nitrogen")

    @classmethod
    def custom(cls, mu_ref: float, T_ref: float, S: float, name: str = "sutherland_custom") -> Sutherland:
        """Create a custom Sutherland model."""
        return cls(mu_ref=mu_ref, T_ref=T_ref, S=S, name=name)

    # --------------------------------------------------
    # methods to compute viscosity
    # --------------------------------------------------

    def mu(self, temp: float) -> float:
        """Dynamic viscosity [Pa s] at temperature temp [K]."""
        if temp <= 0:
            raise ValueError(f"Temperature must be positive, got {temp} K")
        ratio = (temp / self.T_ref) ** 1.5
        factor = (self.T_ref + self.S) / (temp + self.S)
        return self.mu_ref * ratio * factor

    def dmudt(self, temp: float) -> float:
        """Derivative of dynamic viscosity w.r.t. temperature [Pa s / K]."""
        if temp <= 0:
            raise ValueError(f"Temperature must be positive, got {temp} K")
        # d/dT [mu_ref * (T/T_ref)^1.5 * (T_ref + S)/(T + S)]
        c1 = self.mu_ref * (self.T_ref + self.S) / self.T_ref**1.5
        return c1 * temp**0.5 * (0.5 * temp + 1.5 * self.S) / (temp + self.S)**2

    def nu(self, temp: float, dens: float) -> float:
        """Kinematic viscosity [m^2/s] at temperature temp [K] and density dens [kg/m^3]."""
        if dens <= 0:
            raise ValueError(f"Density must be positive, got {dens} kg/m^3")
        return self.mu(temp) / dens


# --------------------------------------------------
# SutherlandLowTemp: Sutherland with low-temperature correction
# --------------------------------------------------
@dataclass(frozen=True)
class SutherlandLowTemp:
    """
    Sutherland's law with low-temperature correction.

    For T < T1 (40 K): mu is constant (frozen at T1 value)
    For T1 <= T <= T2 (110.4 K): mu varies linearly with T
    For T > T2: Standard Sutherland's law

    This prevents unphysical viscosity values at very low temperatures
    where Sutherland's law breaks down.

    Attributes:
        c1: Sutherland coefficient [kg/(m·s·K^0.5)], default 1.458e-6
        c2: Linear coefficient [kg/(m·s·K)], default 6.93873e-8
        S: Sutherland constant [K], default 110.4
        T1: Lower transition temperature [K], default 40.0
        T2: Upper transition temperature [K], default 110.4
        name: Model identifier
    """

    c1: float = 1.458e-6
    c2: float = 6.93873e-8
    S: float = 110.4
    T1: float = 40.0
    T2: float = 110.4
    name: str = "sutherland_low_temp"

    # --------------------------------------------------
    # class methods for standard gases
    #
    # cls is the class itself, passed automatically by @classmethod
    #
    # currently implemented:
    # - air
    # --------------------------------------------------
    @classmethod
    def air(cls) -> SutherlandLowTemp:
        """Low-temperature corrected Sutherland model for air."""
        return cls(name="sutherland_low_temp_air")

    # --------------------------------------------------
    # methods to compute viscosity
    # --------------------------------------------------

    def mu(self, temp: float) -> float:
        """Dynamic viscosity [Pa s] at temperature temp [K]."""
        if temp <= 0:
            raise ValueError(f"Temperature must be positive, got {temp} K")

        if temp < self.T1:
            # Constant below T1
            return self.c2 * self.T1
        elif temp <= self.T2:
            # Linear between T1 and T2
            return self.c2 * temp
        else:
            # Standard Sutherland above T2
            return self.c1 * temp ** 1.5 / (temp + self.S)

    def dmudt(self, temp: float) -> float:
        """Derivative of dynamic viscosity w.r.t. temperature [Pa s / K]."""
        if temp <= 0:
            raise ValueError(f"Temperature must be positive, got {temp} K")

        if temp < self.T1:
            # Constant below T1
            return 0.0
        elif temp <= self.T2:
            # Linear between T1 and T2
            return self.c2
        else:
            # Standard Sutherland above T2
            # d/dT [c1 * T^1.5 / (T + S)]
            return self.c1 * temp**0.5 * (0.5 * temp + 1.5 * self.S) / (temp + self.S)**2

    def nu(self, temp: float, dens: float) -> float:
        """Kinematic viscosity [m^2/s]."""
        if dens <= 0:
            raise ValueError(f"Density must be positive, got {dens} kg/m^3")
        return self.mu(temp) / dens


# --------------------------------------------------
# SutherlandBlended: Sutherland with polynomial blending (profcom style)
# --------------------------------------------------
@dataclass(frozen=True)
class SutherlandBlended:
    """
    Sutherland's law with blended low-temperature correction (profcom style).

    Uses polynomial blending between 100-130 K for smooth transition:
    - T > 130 K: Standard Sutherland
    - 100 K < T <= 130 K: 8th-order polynomial blend
    - T <= 100 K: Linear with temperature

    This provides a smoother transition than SutherlandLowTemp.

    Attributes:
        c1: Sutherland coefficient [kg/(m·s·K^0.5)]
        c2: Linear coefficient [kg/(m·s·K)]
        S: Sutherland constant [K]
        name: Model identifier
    """

    c1: float = 1.458e-6
    c2: float = 6.93873e-8
    S: float = 110.4
    name: str = "sutherland_blended"

    # --------------------------------------------------
    # blending polynomial coefficients (from profcom)
    # --------------------------------------------------
    _visc0: float = 7.659704848e-6
    _a1: float = -4.479148053679334e1
    _a2: float = 3.195188079744342e2
    _a3: float = -9.716235566382709e2
    _a4: float = 1.632645086771892e3
    _a5: float = -1.637375578884298e3
    _a6: float = 9.802775658900685e2
    _a7: float = -3.234667180557399e2
    _a8: float = 4.58157988617632e1

    # --------------------------------------------------
    # class methods for standard gases
    #
    # cls is the class itself, passed automatically by @classmethod
    #
    # currently implemented:
    # - air
    # --------------------------------------------------
    @classmethod
    def air(cls) -> SutherlandBlended:
        """Blended Sutherland model for air."""
        return cls(name="sutherland_blended_air")

    # --------------------------------------------------
    # methods to compute viscosity
    # --------------------------------------------------

    def mu(self, temp: float) -> float:
        """Dynamic viscosity [Pa s] at temperature temp [K]."""
        if temp <= 0:
            raise ValueError(f"Temperature must be positive, got {temp} K")

        if temp > 130.0:
            # Standard Sutherland
            return self.c1 * temp ** 1.5 / (temp + self.S)
        elif temp > 100.0:
            # Polynomial blend
            t_norm = temp / self.S
            visc = (
                self._a1 * t_norm**7
                + self._a2 * t_norm**6
                + self._a3 * t_norm**5
                + self._a4 * t_norm**4
                + self._a5 * t_norm**3
                + self._a6 * t_norm**2
                + self._a7 * t_norm
                + self._a8
            )
            return visc * self._visc0
        else:
            # Linear
            return self.c2 * temp

    def dmudt(self, temp: float) -> float:
        """Derivative of dynamic viscosity w.r.t. temperature [Pa s / K]."""
        if temp <= 0:
            raise ValueError(f"Temperature must be positive, got {temp} K")

        if temp > 130.0:
            # Standard Sutherland
            return self.c1 * temp**0.5 * (0.5 * temp + 1.5 * self.S) / (temp + self.S)**2
        elif temp > 100.0:
            # Polynomial blend derivative
            t_norm = temp / self.S
            dvisc_dt_norm = (
                7 * self._a1 * t_norm**6
                + 6 * self._a2 * t_norm**5
                + 5 * self._a3 * t_norm**4
                + 4 * self._a4 * t_norm**3
                + 3 * self._a5 * t_norm**2
                + 2 * self._a6 * t_norm
                + self._a7
            )
            return dvisc_dt_norm * self._visc0 / self.S
        else:
            # Linear
            return self.c2

    def nu(self, temp: float, dens: float) -> float:
        """Kinematic viscosity [m^2/s]."""
        if dens <= 0:
            raise ValueError(f"Density must be positive, got {dens} kg/m^3")
        return self.mu(temp) / dens
