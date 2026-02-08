"""
Write FlowState data to file (JSON, TOML, etc).
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------

from __future__ import annotations

import json
import re
from pathlib import Path

from flow_state.core import FlowState

# --------------------------------------------------
# write JSON output
#
# writes a FlowState to a JSON file using FlowState.to_dict()
# post-processes to collapse [value, "unit"] arrays to single lines
# --------------------------------------------------


def write_json(state: FlowState, path: str | Path, indent: int = 2) -> None:
    """
    Write FlowState to a JSON file.

    Args:
        state: the FlowState to serialize
        path: path to the output JSON file
        indent: indentation level for pretty-printing (default: 2)
    """
    path = Path(path)
    text = json.dumps(state.to_dict(), indent=indent)

    # collapse 2-element arrays like [value, "unit"] to single line
    # pattern handles varying indentation levels
    text = re.sub(
        r'\[\n\s+([^,\[\]]+),\n\s+"([^"]*)"\n\s+\]',
        r'[\1, "\2"]',
        text
    )
    path.write_text(text)


# --------------------------------------------------
# write TOML output (optional, requires tomli_w)
#
# writes a FlowState to a TOML file using FlowState.to_dict()
# --------------------------------------------------


def write_toml(state: FlowState, path: str | Path) -> None:
    """
    Write FlowState to a TOML file.

    Args:
        state: the FlowState to serialize
        path: path to the output TOML file

    Raises:
        ImportError: if tomli_w is not installed
    """
    path = Path(path)
    try:
        import tomli_w
    except ImportError:
        raise ImportError("tomli_w is required for TOML output: pip install tomli-w")

    path.write_text(tomli_w.dumps(state.to_dict()))
