"""
Read TOML configuration file for flow_state CLI.
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------

from __future__ import annotations

import tomllib
from pathlib import Path
from typing import Any

# --------------------------------------------------
# read config file
#
# reads a TOML config file and returns a dict of kwargs for solve()
# TOML arrays [value, "unit"] are converted to tuples (value, "unit")
# --------------------------------------------------


def read_config(path: Path) -> dict[str, Any]:
    """
    Read a TOML config file and return kwargs for solve().

    Args:
        path: path to the TOML config file

    Returns:
        dict of keyword arguments for solve()

    Raises:
        FileNotFoundError: if config file does not exist
        tomllib.TOMLDecodeError: if config file is invalid TOML
    """
    with open(path, "rb") as f:
        cfg = tomllib.load(f)

    # convert [value, unit] lists to tuples (TOML arrays -> Python lists)
    def to_tuple(val: Any) -> Any:
        if isinstance(val, list) and len(val) == 2:
            return (val[0], val[1])
        return val

    # build kwargs for solve()
    kwargs: dict[str, Any] = {}

    # map config keys to solve() parameters
    param_keys = [
        "mach", "pres", "temp", "uvel",
        "pres_stag", "temp_stag",
        "altitude", "re1", "lref", "pr", "notes"
    ]

    for key in param_keys:
        if key in cfg:
            kwargs[key] = to_tuple(cfg[key])

    # handle atmosphere model
    # supports: atm = "cira86" or [atmosphere] table with model/latitude/month
    if "atm" in cfg:
        kwargs["atm"] = cfg["atm"]
    elif "atmosphere" in cfg:
        atm_cfg = cfg["atmosphere"]
        model = atm_cfg.get("model", "ussa76")
        if model.lower() == "cira86":
            from flow_state.atmosphere import CIRA86
            latitude = atm_cfg.get("latitude", 45.0)
            month = atm_cfg.get("month", 1)
            kwargs["atm"] = CIRA86(latitude=latitude, month=month)
        else:
            kwargs["atm"] = model

    return kwargs
