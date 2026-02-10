# Examples

Practical examples of using `flow_state` for common tasks.

## Python API

### Wind Tunnel Conditions

=== "BAM6QT"

    The Boeing/AFOSR Mach 6 Quiet Tunnel at Purdue:

    ```python
    from flow_state import solve
    from flow_state.io import summary

    # BAM6QT typical quiet conditions
    # p0 = 140 psi, T0 = 420 K
    state = solve(
        mach=6.0,
        pres_stag=(140.0, "psi"),
        temp_stag=420.0,
    )

    print(summary(state))
    ```

=== "AEDC Tunnel 9"

    Arnold Engineering Development Complex Tunnel 9 (White Oak):

    ```python
    from flow_state import solve
    from flow_state.io import summary

    # AEDC T9 high-enthalpy conditions
    # Mach 10, p0 = 1800 psi, T0 = 1000 K, nitrogen
    state = solve(
        mach=10.0,
        pres_stag=(1800.0, "psi"),
        temp_stag=1000.0,
        gas="n2",
    )

    print(summary(state))
    ```

=== "UA LT5"

    University of Alabama Ludwieg Tube 5:

    ```python
    from flow_state import solve
    from flow_state.io import summary

    # UA LT5 typical conditions
    # Mach 5, p0 = 100 psi, T0 = 375 K
    state = solve(
        mach=5.0,
        pres_stag=(100.0, "psi"),
        temp_stag=375.0,
    )

    print(summary(state))
    ```

### Flight Conditions

=== "STORT (USSA76)"

    Sounding Rocket Transition (STORT) trajectory point using US Standard Atmosphere:

    ```python
    from flow_state import solve
    from flow_state.io import summary

    # STORT trajectory point
    # Mach 6, altitude 30 km
    state = solve(
        mach=6.0,
        altitude=30_000,
        atm="ussa76",
    )

    print(summary(state))
    ```

=== "STORT (CIRA86)"

    Same STORT trajectory point using CIRA-86 with latitude/month:

    ```python
    from flow_state import solve, atmosphere
    from flow_state.io import summary

    # STORT trajectory point with CIRA-86
    # Mach 6, altitude 30 km, mid-latitude summer
    cira = atmosphere.CIRA86(latitude=35, month=7)
    state = solve(
        mach=6.0,
        altitude=30_000,
        atm=cira,
    )

    print(summary(state))
    ```

=== "HIFiRE-1"

    Hypersonic International Flight Research Experimentation (HIFiRE-1):

    ```python
    from flow_state import solve
    from flow_state.io import summary

    # HIFiRE-1 trajectory point
    # Mach 7, altitude 25 km
    state = solve(
        mach=7.0,
        altitude=25_000,
    )

    print(summary(state))
    ```

## CLI

### Direct Options

=== "BAM6QT"

    ```bash
    flow-state solve --mach 6 --pres-stag 140 --pres-stag-unit psi --temp-stag 420
    ```

=== "AEDC Tunnel 9"

    ```bash
    flow-state solve --mach 10 --pres-stag 1800 --pres-stag-unit psi --temp-stag 1000 --gas n2
    ```

=== "HIFiRE-1"

    ```bash
    flow-state solve --mach 7 --altitude 25000
    ```

### Config Files

=== "BAM6QT"

    ```toml title="bam6qt.toml"
    mach = 6.0
    pres_stag = [140, "psi"]
    temp_stag = 420
    ```

    [:material-download: Download](configs/bam6qt.toml){ .md-button .md-button--primary download="bam6qt.toml" }

=== "AEDC Tunnel 9"

    ```toml title="aedc_t9.toml"
    mach = 10.0
    pres_stag = [1800, "psi"]
    temp_stag = 1000
    gas = "n2"
    ```

    [:material-download: Download](configs/aedc_t9.toml){ .md-button .md-button--primary download="aedc_t9.toml" }

=== "HIFiRE-1"

    ```toml title="hifire1.toml"
    mach = 7.0
    altitude = 25000
    ```

    [:material-download: Download](configs/hifire1.toml){ .md-button .md-button--primary download="hifire1.toml" }

=== "STORT (CIRA86)"

    ```toml title="stort_cira.toml"
    mach = 6.0
    altitude = 30000

    [atmosphere]
    model = "cira86"
    latitude = 35
    month = 7
    ```

    [:material-download: Download](configs/stort_cira.toml){ .md-button .md-button--primary download="stort_cira.toml" }

Run with:

```bash
flow-state solve --config <filename>.toml
```

See [CLI Usage](user-guide/cli.md) and [Config Files](user-guide/config-files.md) for more details.

