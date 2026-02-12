"""
FlowState dataclass - the central data structure for flow conditions.
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

from flow_state.turbulence import KolmogorovScales, TaylorScales

# --------------------------------------------------
# SI units for each field (used by FlowState.to_dict)
# dimensionless quantities use "-" as the unit
# --------------------------------------------------

_UNITS: dict[str, str] = {
    # static conditions
    "pres": "Pa",
    "temp": "K",
    "dens": "kg/m^3",
    "a": "m/s",
    # flow kinematics
    "mach": "-",
    "uvel": "m/s",
    # transport
    "mu": "Pa*s",
    "nu": "m^2/s",
    "re1": "1/m",
    # gas properties
    "cp": "J/(kg*K)",
    "cv": "J/(kg*K)",
    "gamma": "-",
    "r_gas": "J/(kg*K)",
    "pr": "-",
    # reference scales
    "lref": "m",
    "tref": "s",
    # stagnation conditions
    "pres_stag": "Pa",
    "temp_stag": "K",
    "dens_stag": "kg/m^3",
    "enth_stag": "J/kg",
    # atmosphere
    "altitude": "m",
    # kolmogorov scales
    "eta": "m",
    "tau": "s",
    "vel": "m/s",
    # taylor scales
    "lmbda": "m",
    "re_lambda": "-",
}


# --------------------------------------------------
# FlowState dataclass
#
# this is the central data structure for flow_state
# it holds all computed flow properties in SI units
# --------------------------------------------------


@dataclass
class FlowState:
    """
    Complete flow state representation.

    All quantities are in SI units:
        - pres: pressure [Pa]
        - temp: temperature [K]
        - dens: density [kg/m^3]
        - a: speed of sound [m/s]
        - mach: Mach number [-]
        - uvel: velocity [m/s]
        - mu: dynamic viscosity [Pa*s]
        - nu: kinematic viscosity [m^2/s]
        - cp: specific heat at constant pressure [J/(kg K)]
        - cv: specific heat at constant volume [J/(kg K)]
        - gamma: ratio of specific heats [-]
        - r_gas: specific gas constant [J/(kg K)]
        - pr: Prandtl number [-]
        - re1: unit Reynolds number [1/m]
        - lref: reference length scale [m]
        - tref: reference time scale [s]
        - pres_stag: stagnation pressure [Pa]
        - temp_stag: stagnation temperature [K]
        - dens_stag: stagnation density [kg/m^3]
        - enth_stag: stagnation enthalpy [J/kg]
        - kolmogorov: Kolmogorov turbulence scales
        - taylor: Taylor microscale
    """

    # --------------------------------------------------
    # dataclass fields
    # --------------------------------------------------

    # model names (strings, not objects)
    gas_model: str
    transport_model: str | None

    # thermodynamic state
    pres: float
    temp: float
    dens: float
    a: float

    # flow kinematics (optional - may be None if not specified)
    mach: float | None
    uvel: float | None

    # transport properties (optional - None if no transport model)
    mu: float | None
    nu: float | None

    # gas properties
    cp: float
    cv: float
    gamma: float
    r_gas: float
    pr: float | None = None
    re1: float | None = None

    # reference length scale (for turbulence scales)
    lref: float = 1.0
    tref: float | None = None  # reference time scale [s] = lref / uvel

    # stagnation (total) conditions - computed from static + Mach via isentropic relations
    pres_stag: float | None = None
    temp_stag: float | None = None
    dens_stag: float | None = None
    enth_stag: float | None = None

    # turbulence scales - computed from transport + lref
    kolmogorov: KolmogorovScales | None = None
    taylor: TaylorScales | None = None

    # atmosphere (for atmospheric flight cases)
    altitude: float | None = None
    atmosphere_model: str | None = None

    # optional fields
    notes: str | None = None
    provenance: dict[str, Any] | None = None

    # --------------------------------------------------
    # string representation
    # --------------------------------------------------

    def __str__(self) -> str:
        """Return a human-readable summary."""
        from flow_state.io.print_summary import summary
        return summary(self)

    def __repr__(self) -> str:
        """Return a concise representation for debugging."""
        parts = [f"FlowState(mach={self.mach}"]
        if self.altitude is not None:
            parts.append(f"altitude={self.altitude:.0f}")
        parts.append(f"pres={self.pres:.4g}")
        parts.append(f"temp={self.temp:.1f}")
        if self.re1 is not None:
            parts.append(f"re1={self.re1:.3g}")
        return ", ".join(parts) + ")"

    # --------------------------------------------------
    # serialization methods
    #
    # to_dict() converts the FlowState dataclass to a plain Python dictionary
    # this is needed because:
    # - json.dumps() cannot directly serialize a dataclass
    # - NamedTuples (kolmogorov, taylor) need to be converted to dicts
    #
    # each numeric value is output as [value, "unit"] for downstream safety
    # dimensionless quantities use "-" as the unit
    #
    # to_json() wraps to_dict() and returns a JSON string
    # --------------------------------------------------

    def _with_unit(self, key: str, value: float | None) -> list | None:
        """Return [value, unit] or None if value is None."""
        if value is None:
            return None
        return [value, _UNITS[key]]

    def to_dict(self) -> dict[str, Any]:
        """
        Convert FlowState to a dictionary for JSON serialization.

        Each numeric value is output as [value, "unit"] for self-documentation
        and downstream tool safety. Nested NamedTuples (kolmogorov, taylor)
        are converted to dicts with the same format.
        """
        # helper for kolmogorov/taylor nested dicts
        def scales_dict(scales, keys):
            if scales is None:
                return None
            return {k: self._with_unit(k, getattr(scales, k)) for k in keys}

        return {
            # model info (strings, no units)
            "gas_model": self.gas_model,
            "transport_model": self.transport_model,
            # static conditions
            "pres": self._with_unit("pres", self.pres),
            "temp": self._with_unit("temp", self.temp),
            "dens": self._with_unit("dens", self.dens),
            "a": self._with_unit("a", self.a),
            # flow kinematics
            "mach": self._with_unit("mach", self.mach),
            "uvel": self._with_unit("uvel", self.uvel),
            # transport
            "mu": self._with_unit("mu", self.mu),
            "nu": self._with_unit("nu", self.nu),
            "re1": self._with_unit("re1", self.re1),
            # gas properties
            "cp": self._with_unit("cp", self.cp),
            "cv": self._with_unit("cv", self.cv),
            "gamma": self._with_unit("gamma", self.gamma),
            "r_gas": self._with_unit("r_gas", self.r_gas),
            "pr": self._with_unit("pr", self.pr),
            # reference scales
            "lref": self._with_unit("lref", self.lref),
            "tref": self._with_unit("tref", self.tref),
            # stagnation conditions
            "pres_stag": self._with_unit("pres_stag", self.pres_stag),
            "temp_stag": self._with_unit("temp_stag", self.temp_stag),
            "dens_stag": self._with_unit("dens_stag", self.dens_stag),
            "enth_stag": self._with_unit("enth_stag", self.enth_stag),
            # turbulence scales
            "kolmogorov": scales_dict(self.kolmogorov, ["eta", "tau", "vel"]),
            "taylor": scales_dict(self.taylor, ["lmbda", "re_lambda"]),
            # atmosphere (if applicable)
            "altitude": self._with_unit("altitude", self.altitude),
            "atmosphere_model": self.atmosphere_model,
            # metadata (no units)
            "notes": self.notes,
            "provenance": self.provenance,
        }
