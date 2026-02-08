"""
tests for io functions (read_config, write_json)
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------

import json
import tempfile
from pathlib import Path

from flow_state.io import read_config, write_json
from flow_state.solvers import solve

# --------------------------------------------------
# tests for write_json
# --------------------------------------------------


class TestWriteJSON:
    """tests for JSON output"""

    def test_write_json_file(self) -> None:
        """write a state to a JSON file and verify contents"""
        state = solve(mach=2.0, pres=101325.0, temp=300.0)

        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "test_state.json"
            write_json(state, path)

            content = path.read_text()
            data = json.loads(content)

            # check for expected keys - values now include units as [value, "unit"]
            assert data["pres"] == [101325.0, "Pa"]
            assert data["temp"] == [300.0, "K"]
            assert data["mach"] == [2.0, "-"]
            assert "gas_model" in data
            assert "transport_model" in data

    def test_json_includes_provenance(self) -> None:
        """verify provenance is included in JSON output"""
        state = solve(mach=2.0, pres=101325.0, temp=300.0)

        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "test_state.json"
            write_json(state, path)

            content = path.read_text()
            data = json.loads(content)

            assert "provenance" in data
            assert data["provenance"]["builder"] == "from_mach_pres_temp"
            assert "inputs" in data["provenance"]


# --------------------------------------------------
# tests for read_config
# --------------------------------------------------


class TestReadConfig:
    """tests for config file reading"""

    def test_read_config_simple(self) -> None:
        """read a simple config file with basic values"""
        config_content = """
mach = 2.0
pres = 101325.0
temp = 300.0
"""
        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "test_config.toml"
            path.write_text(config_content)

            kwargs = read_config(path)

            assert kwargs["mach"] == 2.0
            assert kwargs["pres"] == 101325.0
            assert kwargs["temp"] == 300.0

    def test_read_config_with_units(self) -> None:
        """read config with unit tuples like [140, "psi"]"""
        config_content = """
mach = 6.0
pres_stag = [140, "psi"]
temp_stag = 420
"""
        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "test_config.toml"
            path.write_text(config_content)

            kwargs = read_config(path)

            assert kwargs["mach"] == 6.0
            # unit tuple should be converted from list to tuple
            assert kwargs["pres_stag"] == (140, "psi")
            assert kwargs["temp_stag"] == 420

    def test_read_config_with_notes(self) -> None:
        """read config with notes field"""
        config_content = """
mach = 2.0
pres = 101325.0
temp = 300.0
notes = "Test case"
"""
        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "test_config.toml"
            path.write_text(config_content)

            kwargs = read_config(path)

            assert kwargs["notes"] == "Test case"
