# flow_state Webapp

Web interface for computing compressible flow states using Streamlit.

## Local Development

```bash
cd apps/webapp
pip install -r requirements.txt
streamlit run app.py
```

Opens at http://localhost:8501

## Features

**Input Modes:**

- Static conditions (M, p, T)
- Stagnation conditions (M, p₀, T₀)
- Altitude-based (M, h) with USSA76/CIRA86

**Gas Models:**

- Air
- Nitrogen
- Custom (γ, R)

**Output:**

- Static and stagnation conditions
- Transport properties
- Reynolds numbers
- Download as JSON or TOML

## Deployment

Deploy to [Streamlit Cloud](https://streamlit.io/cloud) by connecting your GitHub repo and pointing to `apps/webapp/app.py`.

