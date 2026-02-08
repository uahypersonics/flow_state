"""Compressible flow state utilities."""

from flow_state import atmosphere, io
from flow_state._version import __version__
from flow_state.core import FlowState
from flow_state.isentropic import stag_to_stat, stat_to_stag
from flow_state.solvers import (
    from_mach_altitude_atmosphere,
    from_mach_pres_stag_temp_stag,
    from_mach_pres_temp,
    from_mach_re1_atmosphere,
    from_re1_pres_mach,
    from_re1_pres_stag_temp_stag,
    from_re1_temp_mach,
    solve,
)

__all__ = [
    "__version__",
    # namespaces
    "atmosphere",
    "io",
    # core
    "FlowState",
    "solve",
    # builders (for advanced usage)
    "from_mach_pres_temp",
    "from_mach_pres_stag_temp_stag",
    "from_mach_altitude_atmosphere",
    "from_re1_temp_mach",
    "from_re1_pres_mach",
    "from_re1_pres_stag_temp_stag",
    "from_mach_re1_atmosphere",
    # isentropic relations
    "stag_to_stat",
    "stat_to_stag",
]

