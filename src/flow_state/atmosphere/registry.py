"""
Atmosphere model registry and dispatcher.
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------
from __future__ import annotations

from collections.abc import Callable
from typing import Any

from flow_state.atmosphere.cira86 import CIRA86
from flow_state.atmosphere.core import AtmosphereState
from flow_state.atmosphere.ussa76 import USSA76

# --------------------------------------------------
# model registry
# maps lowercase model names to their corresponding classes
# each class is stored in its own module (e.g. ussa76.py) and imported here
# --------------------------------------------------
MODELS: dict[str, type] = {
    "ussa76": USSA76,
    "cira86": CIRA86,
}


# --------------------------------------------------
# get_atmosphere_model: factory to instantiate atmosphere models by name
# --------------------------------------------------
def get_atmosphere_model(name: str, **kwargs: Any) -> Callable[[float], AtmosphereState]:
    """
    Get an atmosphere model instance by name.

    Args:
        name: Model name (case-insensitive). One of: "ussa76", "cira86".
        **kwargs: Model-specific parameters passed to the constructor.
            - USSA76: no additional parameters
            - CIRA86: latitude (float), month (int)

    Returns:
        An atmosphere model instance that can be called with altitude.

    Raises:
        ValueError: If model name is not recognized.

    Example:
        >>> from flow_state.atmosphere import get_model
        >>> model = get_model("ussa76")
        >>> state = model(10000)  # 10 km
        >>> print(f"T = {state.temp:.2f} K")

        >>> model = get_model("cira86", latitude=45, month=7)
        >>> state = model(20000)
    """
    key = name.lower()
    if key not in MODELS:
        available = ", ".join(MODELS.keys())
        raise ValueError(f"Unknown atmosphere model '{name}'. Available: {available}")

    return MODELS[key](**kwargs)


# --------------------------------------------------
# available_atmosphere_models: list registered model names
# --------------------------------------------------
def available_atmosphere_models() -> list[str]:
    """
    List available atmosphere model names.

    Returns:
        List of model names that can be passed to get_atmosphere_model().

    Example:
        >>> from flow_state.atmosphere import available_atmosphere_models
        >>> print(available_atmosphere_models())
        ['ussa76', 'cira86']
    """
    return list(MODELS.keys())
