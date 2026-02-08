"""
Solvers for computing FlowState from various input combinations.

All functions take some known inputs and return a complete FlowState.
Some use direct computation, others use iteration (bisection) internally.
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------

from __future__ import annotations

from collections.abc import Callable

from flow_state.atmosphere import get_atmosphere_model
from flow_state.core import FlowState
from flow_state.gas import PerfectGas
from flow_state.isentropic import stag_to_stat, stat_to_stag
from flow_state.math_utils import bisect
from flow_state.transport import Sutherland
from flow_state.turbulence import kolmogorov_scales, taylor_scales
from flow_state.units import convert_length, convert_pressure, convert_temperature, convert_velocity

# --------------------------------------------------
# helper: resolve atmosphere model from string or callable
# --------------------------------------------------


def _resolve_atmosphere(
    atm: str | Callable,
) -> tuple[Callable, str]:
    """
    Resolve atmosphere model from string name or callable.

    Returns:
        (callable, model_name) tuple
    """
    if isinstance(atm, str):
        return get_atmosphere_model(atm), atm
    # callable - try to get name from class
    name = getattr(atm.__class__, "__name__", None)
    if name and name not in ("function", "partial"):
        # it's a class instance like CIRA86 or USSA76
        return atm, name.lower()
    # fallback to function name or "custom"
    name = getattr(atm, "__name__", None)
    if name is None or name == "<lambda>":
        name = "custom"
    return atm, name


# --------------------------------------------------
# helper: compute re1 from static conditions
# --------------------------------------------------


def _compute_re1(
    pres: float,
    temp: float,
    mach: float,
    gas: PerfectGas,
    transport: Sutherland,
) -> float:
    """Compute unit Reynolds number from static conditions."""
    dens = gas.density(pres, temp)
    a = gas.sound_speed(temp)
    uvel = mach * a
    visc_dyn = transport.visc_dyn(temp)
    return dens * uvel / visc_dyn


# --------------------------------------------------
# helper: build FlowState from static conditions
# --------------------------------------------------


def _build_state(
    pres: float,
    temp: float,
    mach: float | None,
    uvel: float | None,
    gas: PerfectGas,
    transport: Sutherland | None,
    pr: float | None,
    lref: float,
    notes: str | None,
    provenance: dict,
    altitude: float | None = None,
    atmosphere_model: str | None = None,
) -> FlowState:
    """Build FlowState from computed values."""
    dens = gas.density(pres, temp)
    a = gas.sound_speed(temp)
    gamma = gas.gamma(temp, pres)
    r_gas = gas.r_gas(temp, pres)
    cp = gas.cp(temp, pres)

    # compute mach/uvel if one is provided
    if mach is not None and uvel is None:
        uvel = mach * a
    elif uvel is not None and mach is None:
        mach = uvel / a if a > 0 else None

    # transport properties
    visc_dyn = None
    visc_kin = None
    re1 = None
    computed_pr = pr

    if transport is not None:
        visc_dyn = transport.visc_dyn(temp)
        visc_kin = transport.visc_kin(temp, dens)
        if computed_pr is None:
            computed_pr = 0.72
        # compute re1 if we have velocity
        if uvel is not None and visc_dyn > 0:
            re1 = dens * uvel / visc_dyn

    # stagnation conditions (if mach is known)
    pres_stag = None
    temp_stag = None
    dens_stag = None
    enth_stag = None

    if mach is not None:
        pres_stag, temp_stag, dens_stag = stat_to_stag(mach, pres, temp, gamma, r_gas)
        enth_stag = cp * temp_stag

    # reference time scale
    tref = lref / uvel if uvel is not None and uvel > 0 else None

    # turbulence scales (if transport is available)
    kol = None
    tay = None

    if visc_kin is not None and uvel is not None and uvel > 0:
        kol = kolmogorov_scales(visc_kin, uvel, lref)
        tay = taylor_scales(visc_kin, uvel, lref)

    return FlowState(
        gas_model=gas.name,
        transport_model=transport.name if transport else None,
        pres=pres,
        temp=temp,
        dens=dens,
        a=a,
        mach=mach,
        uvel=uvel,
        visc_dyn=visc_dyn,
        visc_kin=visc_kin,
        re1=re1,
        cp=cp,
        cv=gas.cv(temp, pres),
        gamma=gamma,
        r_gas=r_gas,
        pr=computed_pr,
        lref=lref,
        tref=tref,
        pres_stag=pres_stag,
        temp_stag=temp_stag,
        dens_stag=dens_stag,
        enth_stag=enth_stag,
        kolmogorov=kol,
        taylor=tay,
        altitude=altitude,
        atmosphere_model=atmosphere_model,
        notes=notes,
        provenance=provenance,
    )


# --------------------------------------------------
# type alias for dimensional inputs that accept optional units
# --------------------------------------------------
# Users can provide either:
#   - A plain float (assumed SI): pres=101325
#   - A tuple with units: pres=(14.7, "psi"), temp=(25, "C")
# This allows solve() to accept mixed units without manual conversion.
ValueWithUnit = float | tuple[float, str]

# --------------------------------------------------
# parse value helper function for solve() to handle ValueWithUnit inputs
# --------------------------------------------------
# This function takes a ValueWithUnit and a converter function, and returns the value in SI units
def _parse_value(
    value: ValueWithUnit | None,
    converter: callable,
) -> float | None:
    """
    Parse a value that may include a unit tuple.

    If value is a tuple (val, unit), convert to SI using the converter
    If value is a plain float, return as is (assumed SI units)
    If value is None, return None
    """

    # check if value is None -> return None
    if value is None:
        return None

    # check if value is a tuple -> convert to SI units using converter
    if isinstance(value, tuple):
        # value is a tuple of (val, unit) -> as specified by ValueWithUnit => split
        val, unit = value
        # convert to SI using the provided converter function (e.g. convert_pressure, convert_temperature)
        return converter(val, unit)

    # return value as is if none of the previous conditions were met (assumed SI)
    return value


# --------------------------------------------------
# main solve function that dispatches to specific solvers based on input combinations
# *, at the beginning means all arguments must be passed as keywords for clarity, order doesn't matter
# --------------------------------------------------
def solve(
    # inputs can be provided in any combination, but must be passed as keywords
    *,
    gas: PerfectGas | None = None,
    transport: Sutherland | None = None,
    # static conditions
    pres: ValueWithUnit | None = None,
    temp: ValueWithUnit | None = None,
    mach: float | None = None,
    uvel: ValueWithUnit | None = None,
    # stagnation conditions
    pres_stag: ValueWithUnit | None = None,
    temp_stag: ValueWithUnit | None = None,
    # atmosphere
    altitude: ValueWithUnit | None = None,
    atm: str | Callable = "ussa76",
    # target re1
    re1: float | None = None,
    # reference length for turbulence scales
    lref: ValueWithUnit = 1.0,
    # options
    pr: float | None = None,
    notes: str | None = None,
) -> FlowState:
    """
    Solve for FlowState from any valid combination of inputs.

    Automatically selects the appropriate solver based on which inputs
    are provided. Defaults to air with Sutherland viscosity.

    Dimensional inputs can be specified as:
        - A float in SI units: pres=101325
        - A tuple with units: pres=(14.7, "psi"), temp=(70, "F")

    Supported units:
        - Pressure: Pa, psi, atm, bar, torr
        - Temperature: K, C, F, R (Rankine)
        - Length/altitude: m, ft, km, mi
        - Velocity: m/s, ft/s, kts, mph, kph

    Valid input combinations:
        - pres, temp [, mach or uvel]  -> from_mach_pres_temp
        - mach, pres_stag, temp_stag   -> from_mach_pres_stag_temp_stag
        - altitude, mach               -> from_mach_altitude_atmosphere
        - re1, temp, mach              -> from_re1_temp_mach (solves for pres)
        - re1, pres, mach              -> from_re1_pres_mach (solves for temp)
        - re1, pres_stag, temp_stag    -> from_re1_pres_stag_temp_stag (solves for mach)
        - re1, mach                    -> from_mach_re1_atmosphere (solves for altitude)

    Examples:
        >>> solve(mach=6, pres_stag=(140, "psi"), temp_stag=420)
        >>> solve(altitude=(30000, "ft"), mach=0.8)
        >>> solve(pres=101325, temp=(25, "C"), mach=2.0)

    Args:
        gas: Gas model (default: air)
        transport: Transport model (default: Sutherland for air)
        pres: Static pressure [Pa] or (value, unit)
        temp: Static temperature [K] or (value, unit)
        mach: Mach number [-]
        uvel: Velocity [m/s] or (value, unit)
        pres_stag: Stagnation pressure [Pa] or (value, unit)
        temp_stag: Stagnation temperature [K] or (value, unit)
        altitude: Geometric altitude [m] or (value, unit)
        atm: Atmosphere model - "ussa76", "cira86", or callable (default: "ussa76")
        re1: Target unit Reynolds number [1/m]
        lref: Reference length scale [m] or (value, unit) for turbulence scales (default: 1.0)
        pr: Prandtl number [-]
        notes: Optional notes

    Returns:
        FlowState with all computed properties including stagnation conditions
        and turbulence scales.

    Raises:
        ValueError: If the input combination is not valid.
    """

    # --------------------------------------------------
    # capture provenance before unit conversion
    # this records the original inputs exactly as passed by the user
    # --------------------------------------------------
    raw_inputs = {}
    if pres is not None:
        raw_inputs["pres"] = pres
    if temp is not None:
        raw_inputs["temp"] = temp
    if mach is not None:
        raw_inputs["mach"] = mach
    if uvel is not None:
        raw_inputs["uvel"] = uvel
    if pres_stag is not None:
        raw_inputs["pres_stag"] = pres_stag
    if temp_stag is not None:
        raw_inputs["temp_stag"] = temp_stag
    if altitude is not None:
        raw_inputs["altitude"] = altitude
    if re1 is not None:
        raw_inputs["re1"] = re1
    if lref != 1.0:
        raw_inputs["lref"] = lref
    if pr is not None:
        raw_inputs["pr"] = pr
    if notes is not None:
        raw_inputs["notes"] = notes

    # convert tuples to lists for JSON serialization
    def tuple_to_list(val):
        if isinstance(val, tuple):
            return list(val)
        return val

    provenance = {
        "builder": "solve",
        "inputs": {k: tuple_to_list(v) for k, v in raw_inputs.items()}
    }

    # parse values with units
    # note: pres, temp, ... are passed as ValueWithUnit, which can be either a float (assumed SI) or a tuple (value, unit)
    pres = _parse_value(pres, convert_pressure)
    temp = _parse_value(temp, convert_temperature)
    pres_stag = _parse_value(pres_stag, convert_pressure)
    temp_stag = _parse_value(temp_stag, convert_temperature)
    altitude = _parse_value(altitude, convert_length)
    uvel = _parse_value(uvel, convert_velocity)
    lref_si = _parse_value(lref, convert_length)

    # use perfect gas model as default if no explicit gas model is provided
    if gas is None:
        gas = PerfectGas.air()

    # use Sutherland transport model as default if no explicit transport model is provided
    if transport is None:
        transport = Sutherland.air()

    # detect which inputs were provided
    has_pres = pres is not None
    has_temp = temp is not None
    has_mach = mach is not None
    has_uvel = uvel is not None
    has_pres_stag = pres_stag is not None
    has_temp_stag = temp_stag is not None
    has_altitude = altitude is not None
    has_re1 = re1 is not None

    # --------------------------------------------------
    # dispatch to specific solvers based on input combination
    # raise ValueError if no valid combination is found and state cannot be computed
    # --------------------------------------------------

    # re1 solvers
    if has_re1:
        # re1 + temp + mach -> solve for pres
        if has_temp and has_mach and not has_pres:
            return from_re1_temp_mach(re1, temp, mach, gas, transport, lref=lref_si, pr=pr, notes=notes, provenance=provenance)
        # re1 + pres + mach -> solve for temp
        if has_pres and has_mach and not has_temp:
            return from_re1_pres_mach(re1, pres, mach, gas, transport, lref=lref_si, pr=pr, notes=notes, provenance=provenance)
        # re1 + pres_stag + temp_stag -> solve for mach
        if has_pres_stag and has_temp_stag:
            return from_re1_pres_stag_temp_stag(re1, pres_stag, temp_stag, gas, transport, lref=lref_si, pr=pr, notes=notes, provenance=provenance)
        # re1 + mach -> solve for conditions for standard atmosphere (need to iterate for altitude that yields target re1)
        if has_mach and not has_altitude and not has_pres and not has_temp:
            return from_mach_re1_atmosphere(re1, mach, gas, transport, atm=atm, lref=lref_si, pr=pr, notes=notes, provenance=provenance)
        # currently unavailable combinations
        raise ValueError(
            "Invalid input combination with re1. Valid combinations:\n"
            "  - re1, temp, mach (solves for pressure)\n"
            "  - re1, pres, mach (solves for temperature)\n"
            "  - re1, pres_stag, temp_stag (solves for Mach)\n"
            "  - re1, mach (solves for altitude)"
        )

    # altitude solver
    if has_altitude:
        # altitude + mach -> solve for static conditions
        if has_mach:
            return from_mach_altitude_atmosphere(altitude, mach, gas, transport, atm=atm, lref=lref_si, pr=pr, notes=notes, provenance=provenance)
        # currently unavailable combinations
        raise ValueError("altitude requires mach to be specified.")

    # stagnation solvers
    if has_pres_stag or has_temp_stag:
        # stagnation + mach -> solve for static conditions
        if has_mach and has_pres_stag and has_temp_stag:
            return from_mach_pres_stag_temp_stag(mach, pres_stag, temp_stag, gas, transport, lref=lref_si, pr=pr, notes=notes, provenance=provenance)
        # currently unavailable combinations
        raise ValueError("Stagnation conditions require mach, pres_stag, and temp_stag.")

    # static condition solvers
    if has_pres and has_temp:
        # compute mach from uvel if needed
        if has_uvel and not has_mach:
            a = gas.sound_speed(temp)
            mach = uvel / a if a > 0 else None
        return from_mach_pres_temp(mach, pres, temp, gas, transport, lref=lref_si, pr=pr, notes=notes, provenance=provenance)

    # no valid combination found
    raise ValueError(
        "Invalid input combination. Provide one of:\n"
        "  - pres, temp [, mach or uvel]\n"
        "  - mach, pres, temp\n"
        "  - mach, pres_stag, temp_stag\n"
        "  - altitude, mach\n"
        "  - re1, temp, mach\n"
        "  - re1, pres, mach\n"
        "  - re1, pres_stag, temp_stag\n"
        "  - re1, mach"
    )


# --------------------------------------------------
# compute full state from mach, pres, temp
# --------------------------------------------------
def from_mach_pres_temp(
    mach: float | None,
    pres: float,
    temp: float,
    gas: PerfectGas,
    transport: Sutherland | None = None,
    lref: float = 1.0,
    pr: float | None = None,
    notes: str | None = None,
    provenance: dict | None = None,
) -> FlowState:
    """
    Solve FlowState from Mach number, pressure, and temperature.

    Args:
        mach: Mach number [-]
        pres: Static pressure [Pa]
        temp: Static temperature [K]
        gas: Gas model
        transport: Optional transport model
        lref: Reference length scale [m] for turbulence scales
        pr: Optional Prandtl number [-]
        notes: Optional notes
        provenance: Optional provenance dict (passed from solve())

    Returns:
        FlowState with computed properties.
    """
    if provenance is None:
        provenance = {"inputs": {"mach": mach, "pres": pres, "temp": temp}}
    provenance["builder"] = "from_mach_pres_temp"
    return _build_state(pres, temp, mach, None, gas, transport, pr, lref, notes, provenance)


# --------------------------------------------------
# compute full state from mach, pres_stag, temp_stag
# --------------------------------------------------
def from_mach_pres_stag_temp_stag(
    mach: float,
    pres_stag: float,
    temp_stag: float,
    gas: PerfectGas,
    transport: Sutherland | None = None,
    lref: float = 1.0,
    pr: float | None = None,
    notes: str | None = None,
    provenance: dict | None = None,
) -> FlowState:
    """
    Solve FlowState from Mach number and stagnation conditions.

    Uses isentropic relations to convert stagnation to static conditions.

    Args:
        mach: Mach number [-]
        pres_stag: Stagnation pressure [Pa]
        temp_stag: Stagnation temperature [K]
        gas: Gas model
        transport: Optional transport model
        lref: Reference length scale [m] for turbulence scales
        pr: Optional Prandtl number [-]
        notes: Optional notes
        provenance: Optional provenance dict (passed from solve())

    Returns:
        FlowState with static properties from isentropic relations.
    """
    # isentropic expansion to static conditions
    gamma = gas.gamma(temp_stag, pres_stag)
    r_gas = gas.r_gas(temp_stag, pres_stag)
    static = stag_to_stat(mach, pres_stag, temp_stag, gamma=gamma, r_gas=r_gas)

    if provenance is None:
        provenance = {"inputs": {"mach": mach, "pres_stag": pres_stag, "temp_stag": temp_stag}}
    provenance["builder"] = "from_mach_pres_stag_temp_stag"
    return _build_state(static.pres, static.temp, mach, None, gas, transport, pr, lref, notes, provenance)


# --------------------------------------------------
# compute full state from altitude, mach
# --------------------------------------------------
def from_mach_altitude_atmosphere(
    altitude: float,
    mach: float,
    gas: PerfectGas,
    transport: Sutherland | None = None,
    atm: str | Callable = "ussa76",
    lref: float = 1.0,
    pr: float | None = None,
    notes: str | None = None,
    provenance: dict | None = None,
) -> FlowState:
    """
    Solve FlowState from altitude and Mach number.

    Args:
        altitude: Geometric altitude [m]
        mach: Mach number [-]
        gas: Gas model
        transport: Optional transport model
        atm: Atmosphere model - "ussa76", "cira86", or callable (default: "ussa76")
        lref: Reference length scale [m] for turbulence scales
        pr: Optional Prandtl number [-]
        notes: Optional notes
        provenance: Optional provenance dict (passed from solve())

    Returns:
        FlowState at the specified altitude and Mach.
    """
    atm_model, atm_name = _resolve_atmosphere(atm)
    atm = atm_model(altitude)
    if provenance is None:
        provenance = {"inputs": {"altitude": altitude, "mach": mach}}
    provenance["builder"] = "from_mach_altitude_atmosphere"
    return _build_state(
        atm.pres, atm.temp, mach, None, gas, transport, pr, lref, notes, provenance,
        altitude=altitude, atmosphere_model=atm_name,
    )


# --------------------------------------------------
# solve for pres given re1, temp, mach
# --------------------------------------------------
def from_re1_temp_mach(
    re1: float,
    temp: float,
    mach: float,
    gas: PerfectGas,
    transport: Sutherland,
    pres_bounds: tuple[float, float] = (1.0, 1e7),
    tol: float = 1e-10,
    lref: float = 1.0,
    pr: float | None = None,
    notes: str | None = None,
    provenance: dict | None = None,
) -> FlowState:
    """
    Solve FlowState from target Re1, temperature, and Mach.

    Finds the pressure that yields the target unit Reynolds number.

    Args:
        re1: Target unit Reynolds number [1/m]
        temp: Static temperature [K]
        mach: Mach number [-]
        gas: Gas model
        transport: Transport model (required for Re1)
        pres_bounds: Search range for pressure [Pa]
        tol: Convergence tolerance
        lref: Reference length scale [m] for turbulence scales
        pr: Optional Prandtl number [-]
        notes: Optional notes
        provenance: Optional provenance dict (passed from solve())

    Returns:
        FlowState with pressure solved from target Re1.
    """
    def residual(pres: float) -> float:
        return _compute_re1(pres, temp, mach, gas, transport) - re1

    pres = bisect(residual, pres_bounds[0], pres_bounds[1], rel_tol=tol)
    if provenance is None:
        provenance = {"inputs": {"re1": re1, "temp": temp, "mach": mach}}
    provenance["builder"] = "from_re1_temp_mach"
    return _build_state(pres, temp, mach, None, gas, transport, pr, lref, notes, provenance)


# --------------------------------------------------
# solve for temp given re1, pres, mach
# --------------------------------------------------
def from_re1_pres_mach(
    re1: float,
    pres: float,
    mach: float,
    gas: PerfectGas,
    transport: Sutherland,
    temp_bounds: tuple[float, float] = (50.0, 5000.0),
    tol: float = 1e-10,
    lref: float = 1.0,
    pr: float | None = None,
    notes: str | None = None,
    provenance: dict | None = None,
) -> FlowState:
    """
    Solve FlowState from target Re1, pressure, and Mach.

    Finds the temperature that yields the target unit Reynolds number.

    Args:
        re1: Target unit Reynolds number [1/m]
        pres: Static pressure [Pa]
        mach: Mach number [-]
        gas: Gas model
        transport: Transport model (required for Re1)
        temp_bounds: Search range for temperature [K]
        tol: Convergence tolerance
        lref: Reference length scale [m] for turbulence scales
        pr: Optional Prandtl number [-]
        notes: Optional notes
        provenance: Optional provenance dict (passed from solve())

    Returns:
        FlowState with temperature solved from target Re1.
    """
    def residual(temp: float) -> float:
        return _compute_re1(pres, temp, mach, gas, transport) - re1

    temp = bisect(residual, temp_bounds[0], temp_bounds[1], rel_tol=tol)
    if provenance is None:
        provenance = {"inputs": {"re1": re1, "pres": pres, "mach": mach}}
    provenance["builder"] = "from_re1_pres_mach"
    return _build_state(pres, temp, mach, None, gas, transport, pr, lref, notes, provenance)


# --------------------------------------------------
# solve for mach given re1, pres_stag, temp_stag
# --------------------------------------------------
def from_re1_pres_stag_temp_stag(
    re1: float,
    pres_stag: float,
    temp_stag: float,
    gas: PerfectGas,
    transport: Sutherland,
    mach_bounds: tuple[float, float] = (0.01, 20.0),
    tol: float = 1e-10,
    lref: float = 1.0,
    pr: float | None = None,
    notes: str | None = None,
    provenance: dict | None = None,
) -> FlowState:
    """
    Solve FlowState from target Re1 and stagnation conditions.

    Finds the Mach number that yields the target unit Reynolds number.

    Args:
        re1: Target unit Reynolds number [1/m]
        pres_stag: Stagnation pressure [Pa]
        temp_stag: Stagnation temperature [K]
        gas: Gas model
        transport: Transport model (required for Re1)
        mach_bounds: Search range for Mach number [-]
        tol: Convergence tolerance
        lref: Reference length scale [m] for turbulence scales
        pr: Optional Prandtl number [-]
        notes: Optional notes
        provenance: Optional provenance dict (passed from solve())

    Returns:
        FlowState with Mach solved from target Re1.
    """
    # cache gas properties (constant for perfect gas at fixed stagnation conditions)
    gamma = gas.gamma(temp_stag, pres_stag)
    r_gas = gas.r_gas(temp_stag, pres_stag)

    def residual(mach: float) -> float:
        static = stag_to_stat(mach, pres_stag, temp_stag, gamma=gamma, r_gas=r_gas)
        return _compute_re1(static.pres, static.temp, mach, gas, transport) - re1

    mach = bisect(residual, mach_bounds[0], mach_bounds[1], rel_tol=tol)
    static = stag_to_stat(mach, pres_stag, temp_stag, gamma=gamma, r_gas=r_gas)
    if provenance is None:
        provenance = {"inputs": {"re1": re1, "pres_stag": pres_stag, "temp_stag": temp_stag}}
    provenance["builder"] = "from_re1_pres_stag_temp_stag"
    return _build_state(static.pres, static.temp, mach, None, gas, transport, pr, lref, notes, provenance)


# --------------------------------------------------
# solve for altitude given re1, mach
# --------------------------------------------------
def from_mach_re1_atmosphere(
    re1: float,
    mach: float,
    gas: PerfectGas,
    transport: Sutherland,
    atm: str | Callable = "ussa76",
    altitude_bounds: tuple[float, float] = (0.0, 47000.0),
    tol: float = 1e-10,
    lref: float = 1.0,
    pr: float | None = None,
    notes: str | None = None,
    provenance: dict | None = None,
) -> FlowState:
    """
    Solve FlowState from Mach and target Re1 using atmosphere model.

    Finds the altitude that yields the target unit Reynolds number.

    Args:
        re1: Target unit Reynolds number [1/m]
        mach: Mach number [-]
        gas: Gas model
        transport: Transport model (required for Re1)
        atm: Atmosphere model - "ussa76", "cira86", or callable (default: "ussa76")
        altitude_bounds: Search range for altitude [m]
        tol: Convergence tolerance
        lref: Reference length scale [m] for turbulence scales
        pr: Optional Prandtl number [-]
        notes: Optional notes
        provenance: Optional provenance dict (passed from solve())

    Returns:
        FlowState with altitude solved from target Re1.
    """
    atm_model, atm_name = _resolve_atmosphere(atm)

    def residual(altitude: float) -> float:
        atm = atm_model(altitude)
        return _compute_re1(atm.pres, atm.temp, mach, gas, transport) - re1

    altitude = bisect(residual, altitude_bounds[0], altitude_bounds[1], rel_tol=tol)
    atm = atm_model(altitude)
    if provenance is None:
        provenance = {"inputs": {"re1": re1, "mach": mach}}
    provenance["builder"] = "from_mach_re1_atmosphere"
    return _build_state(
        atm.pres, atm.temp, mach, None, gas, transport, pr, lref, notes, provenance,
        altitude=altitude, atmosphere_model=atm_name,
    )
