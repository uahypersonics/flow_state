"""Transport model registry and factory."""

# --------------------------------------------------
# import necessary modules
# --------------------------------------------------
from __future__ import annotations

from typing import Any

from flow_state.transport.core import TransportModel
from flow_state.transport.keyes import Keyes
from flow_state.transport.power_law import PowerLaw
from flow_state.transport.sutherland import Sutherland, SutherlandBlended, SutherlandLowTemp

# --------------------------------------------------
# model registry
# --------------------------------------------------
MODELS: dict[str, type] = {
    "sutherland": Sutherland,
    "sutherland_low_temp": SutherlandLowTemp,
    "sutherland_blended": SutherlandBlended,
    "keyes": Keyes,
    "power_law": PowerLaw,
}

# --------------------------------------------------
# get_transport_model: factory to instantiate transport models by name
# --------------------------------------------------
def get_transport_model(name: str, **kwargs: Any) -> TransportModel:
    """
    Get a transport model instance by name.

    Args:
        name: Model name (case-insensitive). One of:
            "sutherland", "sutherland_low_temp", "sutherland_blended",
            "keyes", "power_law"
        **kwargs: Model-specific parameters. If none provided, uses .air() preset.
            For Sutherland: mu_ref, T_ref, S
            For Keyes: a0, a1, a2
            For PowerLaw: mu_ref, T_ref, m

    Returns:
        A transport model instance with mu() and nu() methods.

    Raises:
        ValueError: If model name is not recognized.

    Example:
        >>> model = get_transport_model("sutherland")  # uses .air() preset
        >>> mu = model.visc_dyn(300)

        >>> model = get_transport_model("keyes")
        >>> mu = model.visc_dyn(2000)  # high temperature
    """
    key = name.lower()
    if key not in MODELS:
        available = ", ".join(MODELS.keys())
        raise ValueError(f"Unknown transport model '{name}'. Available: {available}")

    cls = MODELS[key]
    if kwargs:
        return cls(**kwargs)
    return cls.air()

# --------------------------------------------------
# available_transport_models: list available transport model names
# --------------------------------------------------
def available_transport_models() -> list[str]:
    """
    List available transport model names.

    Returns:
        List of model names that can be passed to get_transport_model().
    """
    return list(MODELS.keys())
