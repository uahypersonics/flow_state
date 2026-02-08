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
# solve command: compute flow state from config
# --------------------------------------------------


@cli.command("solve")
def cmd_solve(
    # config option:
    # - path to input config file
    # - type: Path (automatically validated)
    # - cli flags to set this option: --config or -c
    # - default if option not provided in cli: flow_config.toml
    config: Annotated[
        Path,
        typer.Option(
            "--config", "-c",
            help="Input config file (TOML)",
        ),
    ] = Path(DEFAULT_CONFIG),
    # output option:
    # - path to write the output JSON file
    # - type: Path (automatically validated)
    # - cli flags to set this option: --output or -o
    # - default if option not provided in cli: flow_state.json
    output: Annotated[
        Path,
        typer.Option(
            "--output", "-o",
            help="Output file (JSON)",
        ),
    ] = Path(DEFAULT_OUTPUT),
    # quiet option:
    # - whether to suppress summary output
    # - type: bool (becomes a flag, no value needed)
    # - cli flags to set this option: --quiet or -q
    # - default if option not provided in cli: False (show summary)
    quiet: Annotated[
        bool,
        typer.Option(
            "--quiet", "-q",
            help="Suppress summary output.",
        ),
    ] = False,
) -> None:
    """
    Compute flow state from config file.

    Reads flow_config.toml (or specified file), computes the flow state,
    and writes results to flow_state.json.
    """
    # check config exists
    if not config.exists():
        typer.echo(f"Error: {config} not found. Run 'flow-state init' first.", err=True)
        raise typer.Exit(1)

    # read config file using io.read_config
    try:
        kwargs = read_config(config)
    except Exception as e:
        typer.echo(f"Error parsing {config}: {e}", err=True)
        raise typer.Exit(1)

    # solve for flow state
    try:
        state = solve(**kwargs)
    except (ValueError, TypeError) as e:
        typer.echo(f"Error computing flow state: {e}", err=True)
        raise typer.Exit(1)

    # write JSON output using io.write_json
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
