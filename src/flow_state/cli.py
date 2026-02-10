"""
Command-line interface for flow_state.

Commands:
    flow-state init   - Generate a template config file (flow_config.toml)
    flow-state solve  - Compute flow state from config, output to JSON
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------

from __future__ import annotations

from pathlib import Path
from typing import Annotated

import typer

from flow_state._version import __version__
from flow_state.io import read_config, write_json
from flow_state.solvers import solve

# --------------------------------------------------
# default file names
# --------------------------------------------------

# set a default config file name
DEFAULT_CONFIG = "flow_config.toml"
# set a default output file name
DEFAULT_OUTPUT = "flow_state.json"


# --------------------------------------------------
# set up cli using typer (reference: https://typer.tiangolo.com/)
# --------------------------------------------------

cli = typer.Typer(
    # set name of cli command
    name="flow-state",
    # set help text shown when running `flow-state --help`
    help="Compute flow state",
    # disable shell completion command (not needed for this rudimentary cli)
    add_completion=False,
)


# --------------------------------------------------
# version callback
# --------------------------------------------------

# define a callback function to print version and exit when --version is used
# this function is called by typer when the --version option is passed
def version_callback(value: bool) -> None:
    """Print version and exit"""
    # only run if --version was actually passed (value=True)
    if value:
        # print version to stdout
        typer.echo(f"flow-state version {__version__}")
        # exit with code 0 (success): nothing else to do after printing version
        raise typer.Exit()


# --------------------------------------------------
# cli callback (runs before any command)
# --------------------------------------------------

# the @cli.callback() decorator marks this function to run on every cli invocation
# this is where we handle global options like --version that apply to all commands
# without this, --version would only work if attached to a specific command
@cli.callback()
def main(
    # version option:
    # - whether to print version and exit
    # - type: bool | None (None means not provided)
    # - cli flags to set this option: --version or -v
    # - callback: version_callback runs immediately when this option is passed
    # - is_eager: True means run callback before parsing other arguments
    #   (so `flow-state --version solve --config missing.toml` still works)
    # - default if option not provided in cli: None (do nothing)
    version: Annotated[
        bool | None,
        typer.Option(
            "--version",
            "-v",
            help="Show version and exit.",
            callback=version_callback,
            is_eager=True,
        ),
    ] = None,
) -> None:
    """flow-state: Compute flow states"""
    # pass means "do nothing" - the callback just handles --version
    # actual work happens in the commands (init, solve)
    pass


# --------------------------------------------------
# template for flow_config.toml
# --------------------------------------------------

CONFIG_TEMPLATE = '''\
# flow_state configuration file
# =============================
# Edit the values below and run: flow-state solve

# Supported units:
#   pressure    : Pa, psi, atm, bar, torr
#   temperature : K, C, F, R (Rankine)
#   length      : m, ft, km, mi
#   velocity    : m/s, ft/s, kts, mph, kph

# Use [value, "unit"] tuples for non-SI units, e.g.:
#   pres_stag = [140, "psi"]
#   altitude = [30000, "ft"]

# --------------------------------------------------
# Input mode: Choose ONE of the following sections
# --------------------------------------------------

# Option 1: Mach + stagnation conditions (wind tunnel)
mach = 6.0
pres_stag = [140, "psi"]  # or 965266.0 for Pa
temp_stag = 420           # Kelvin

# Option 2: Altitude + Mach (flight conditions)
# altitude = [30000, "ft"]
# mach = 0.8

# Option 3: Static conditions
# pres = 101325    # Pa
# temp = 300       # K
# mach = 2.0

# --------------------------------------------------
# Reference length scale (for turbulence scales)
# --------------------------------------------------
lref = 1.0  # [m]

# --------------------------------------------------
# Optional notes
# --------------------------------------------------
# notes = "BAM6QT Mach 6 tunnel conditions"
'''


# --------------------------------------------------
# register cli command "init"
#
# init is used to generate a template config file (flow_config.toml) with example inputs and documentation
#
# NOTE: in typer, the function signature is the cli interface:
# - each parameter becomes a cli option (--name) or argument
# - type hints define validation and conversion
# - typer.Option() sets flag names, help text, etc
# - default values make options optional
# - the docstring becomes the help text for the command
# --------------------------------------------------

# register command using a decorator
@cli.command("init")
# define function for "init" command
def cmd_init(
    # output option:
    # - set path to write the config file
    # - type: Path (automatically validated)
    # - cli flags to set this option: --output or -o
    # - default if option not provided in cli: flow_config.toml
    output: Annotated[
        Path,
        typer.Option(
            "--output", "-o",
            help="Output config file path",
        ),
    ] = Path(DEFAULT_CONFIG),
    # force option:
    # - whether to overwrite existing file
    # - type: bool (becomes a flag, no value needed)
    # - cli flags to set this option: --force or -f
    # - default if option not provided in cli: False (don't overwrite)
    force: Annotated[
        bool,
        typer.Option(
            "--force", "-f",
            help="Overwrite existing file",
        ),
    ] = False,
) -> None:
    """
    Generate a template configuration file (flow_config.toml) with example inputs and documentation
    """
    if output.exists() and not force:
        typer.echo(f"Error: {output} already exists. Use --force to overwrite", err=True)
        raise typer.Exit(1)

    output.write_text(CONFIG_TEMPLATE)
    typer.echo(f"Created {output}")
    typer.echo("Edit the file, then run: flow-state solve")


# --------------------------------------------------
# solve command: compute flow state from config or direct options
# --------------------------------------------------


@cli.command("solve")
def cmd_solve(
    # ----- Direct input options -----
    mach: Annotated[
        float | None,
        typer.Option(
            "--mach", "-m",
            help="Mach number",
        ),
    ] = None,
    pres_stag: Annotated[
        float | None,
        typer.Option(
            "--pres-stag",
            help="Stagnation pressure (default: Pa)",
        ),
    ] = None,
    pres_stag_unit: Annotated[
        str | None,
        typer.Option(
            "--pres-stag-unit",
            help="Stagnation pressure unit (Pa, psi, atm, bar)",
        ),
    ] = None,
    temp_stag: Annotated[
        float | None,
        typer.Option(
            "--temp-stag",
            help="Stagnation temperature (K)",
        ),
    ] = None,
    altitude: Annotated[
        float | None,
        typer.Option(
            "--altitude", "-a",
            help="Altitude (default: m)",
        ),
    ] = None,
    altitude_unit: Annotated[
        str | None,
        typer.Option(
            "--altitude-unit",
            help="Altitude unit (m, ft, km)",
        ),
    ] = None,
    gas: Annotated[
        str | None,
        typer.Option(
            "--gas",
            help="Gas type (air, n2)",
        ),
    ] = None,
    atm: Annotated[
        str | None,
        typer.Option(
            "--atm",
            help="Atmosphere model (ussa76, cira86)",
        ),
    ] = None,
    lref: Annotated[
        float | None,
        typer.Option(
            "--lref",
            help="Reference length (m)",
        ),
    ] = None,
    # ----- Config file option -----
    config: Annotated[
        Path | None,
        typer.Option(
            "--config", "-c",
            help="Input config file (TOML). Ignored if direct options provided.",
        ),
    ] = None,
    # ----- Output options -----
    output: Annotated[
        Path | None,
        typer.Option(
            "--output", "-o",
            help="Output file (JSON). If not specified, no file is written.",
        ),
    ] = None,
    quiet: Annotated[
        bool,
        typer.Option(
            "--quiet", "-q",
            help="Suppress summary output.",
        ),
    ] = False,
) -> None:
    """
    Compute flow state from direct options or config file.

    Examples:
        flow-state solve --mach 6 --pres-stag 140 --pres-stag-unit psi --temp-stag 420
        flow-state solve --mach 7 --altitude 25000
        flow-state solve --config myconfig.toml
    """
    # Check if any direct options are provided
    has_direct_options = any([mach, pres_stag, temp_stag, altitude])

    if has_direct_options:
        # Build kwargs from direct options
        kwargs = {}
        if mach is not None:
            kwargs["mach"] = mach
        if pres_stag is not None:
            if pres_stag_unit:
                kwargs["pres_stag"] = (pres_stag, pres_stag_unit)
            else:
                kwargs["pres_stag"] = pres_stag
        if temp_stag is not None:
            kwargs["temp_stag"] = temp_stag
        if altitude is not None:
            if altitude_unit:
                kwargs["altitude"] = (altitude, altitude_unit)
            else:
                kwargs["altitude"] = altitude
        if gas is not None:
            kwargs["gas"] = gas
        if atm is not None:
            kwargs["atm"] = atm
        if lref is not None:
            kwargs["lref"] = lref
    else:
        # Fall back to config file
        config_path = config if config else Path(DEFAULT_CONFIG)
        if not config_path.exists():
            typer.echo(f"Error: {config_path} not found.", err=True)
            typer.echo("Provide direct options (--mach, etc.) or run 'flow-state init' to create a config.", err=True)
            raise typer.Exit(1)

        try:
            kwargs = read_config(config_path)
        except Exception as e:
            typer.echo(f"Error parsing {config_path}: {e}", err=True)
            raise typer.Exit(1)

    # solve for flow state
    try:
        state = solve(**kwargs)
    except (ValueError, TypeError) as e:
        typer.echo(f"Error computing flow state: {e}", err=True)
        raise typer.Exit(1)

    # write JSON output if requested
    if output:
        write_json(state, output)
        typer.echo(f"Wrote {output}")

    # print summary unless quiet
    if not quiet:
        typer.echo("")
        typer.echo(str(state))


# --------------------------------------------------
# main entry point (for testing, allows running cli.py directly)
# --------------------------------------------------

if __name__ == "__main__":
    cli()
