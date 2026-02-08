# Streamlit Webapp for flow_state

This is a minimal web interface for computing flow states using Streamlit.

## Installation

```bash
cd apps/webapp
pip install -r requirements.txt
```

**Note:** You must also have `flow_state` installed:
```bash
pip install -e ../..
```

## Running the App

```bash
streamlit run app.py
```

The app will open in your browser at http://localhost:8501.

## Features

- Select gas model (air, nitrogen, or custom)
- Input pressure, temperature, and Mach number
- View computed flow properties
- Download results as TOML file
