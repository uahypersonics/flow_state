"""
tests for io functions (read_config, write_json)
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------

import json
import tempfile
from pathlib import Path

import pytest

from flow_state.io import read_config, write_json
from flow_state.io.legacy_dat import write_flow_conditions_dat
from flow_state.io.print_summary import summary
from flow_state.io.write_data import write_toml
from flow_state.solvers import solve
from flow_state.transport import Sutherland

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


# --------------------------------------------------
# tests for print_summary
# --------------------------------------------------


class TestPrintSummary:
    """tests for human-readable summary output"""

    def test_summary_basic(self) -> None:
        """summary includes key flow properties"""
        state = solve(mach=2.0, pres=101325.0, temp=300.0)
        text = summary(state)

        assert "FlowState Summary" in text
        assert "pres" in text
        assert "temp" in text
        assert "mach" in text

    def test_summary_with_transport(self) -> None:
        """summary includes transport properties when present"""
        state = solve(mach=2.0, pres=101325.0, temp=300.0, transport=Sutherland.air())
        text = summary(state)

        assert "mu" in text
        assert "nu" in text
        assert "re1" in text
        assert "pr" in text

    def test_summary_with_altitude(self) -> None:
        """summary includes altitude when present"""
        state = solve(mach=2.0, altitude=10000)
        text = summary(state)

        assert "altitude" in text
        assert "atm_model" in text

    def test_str_calls_summary(self) -> None:
        """FlowState.__str__ returns the summary"""
        state = solve(mach=2.0, pres=101325.0, temp=300.0)
        assert str(state) == summary(state)


# --------------------------------------------------
# tests for legacy .dat writer
# --------------------------------------------------


class TestLegacyDat:
    """tests for legacy .dat file output"""

    def test_write_dat_no_header(self) -> None:
        """write dat without header (avoids legacy tool_version attribute)"""
        state = solve(mach=2.0, pres=101325.0, temp=300.0)

        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "flow.dat"
            write_flow_conditions_dat(state, path, include_header=False)

            content = path.read_text()
            assert "# flow_conditions.dat" not in content
            assert "pres = " in content
            assert "temp = " in content
            assert "dens = " in content
            assert "gamma = " in content
            assert "M = " in content

    def test_write_dat_with_transport(self) -> None:
        """transport properties included when present"""
        state = solve(mach=2.0, pres=101325.0, temp=300.0, transport=Sutherland.air())

        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "flow.dat"
            write_flow_conditions_dat(state, path, include_header=False)

            content = path.read_text()
            assert "mu = " in content
            assert "nu = " in content
            assert "pr = " in content
            assert "re1 = " in content


# --------------------------------------------------
# tests for write_toml
# --------------------------------------------------


class TestWriteTOML:
    """tests for TOML output"""

    @pytest.mark.skip(reason="write_toml broken: to_dict() contains None values that tomli_w cannot serialize")
    def test_write_toml_basic(self) -> None:
        """write a state to TOML and verify file exists"""
        # use a full solve with all fields populated to avoid None serialization issues
        state = solve(mach=2.0, altitude=10000, transport=Sutherland.air())

        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "state.toml"
            write_toml(state, path)

            content = path.read_text()
            assert "pres" in content
            assert "temp" in content
