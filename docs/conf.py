# BSD-3-Clause License - see LICENSE file
"""
Sphinx configuration for flow_state documentation.
"""

import os
import sys

# Add the src directory to the path
sys.path.insert(0, os.path.abspath("../src"))

# -- Project information -----------------------------------------------------
project = "flow_state"
copyright = "2026, UAH Hypersonics"
author = "UAH Hypersonics"
release = "0.1.0"

# -- General configuration ---------------------------------------------------
extensions = [
    "sphinx.ext.autodoc",
    "sphinx.ext.napoleon",
    "sphinx.ext.viewcode",
]

templates_path = ["_templates"]
exclude_patterns = ["_build", "Thumbs.db", ".DS_Store"]

# -- Options for HTML output -------------------------------------------------
html_theme = "sphinx_rtd_theme"
html_static_path = ["_static"]

# -- Options for autodoc -----------------------------------------------------
autodoc_member_order = "bysource"
autodoc_typehints = "description"

# -- Options for Napoleon (Google/NumPy docstrings) --------------------------
napoleon_google_docstring = True
napoleon_numpy_docstring = True
napoleon_include_init_with_doc = True
