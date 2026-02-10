# Installation

## Requirements

- Python 3.11 or later (uses `tomllib` from the standard library)

## From PyPI

```bash
pip install flow-state
```

## From Source

Clone the repository and install in editable mode:

```bash
git clone https://github.com/uahypersonics/flow_state.git
cd flow_state
pip install -e .
```

## Development Installation

For development, install with the `dev` extras:

```bash
pip install -e ".[dev]"
```

This includes:

- [pytest](https://docs.pytest.org/) and [pytest-cov](https://pytest-cov.readthedocs.io/) for testing
- [ruff](https://docs.astral.sh/ruff/) for linting
- [mkdocs-material](https://squidfunk.github.io/mkdocs-material/) for documentation

## Verify Installation

```python
import flow_state
print(flow_state.__version__)
```

Or from the command line:

```bash
flow-state --version
```
