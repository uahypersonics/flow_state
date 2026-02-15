# API Reference

Technical reference for the `flow_state` package.

## Package Architecture

``` mermaid
graph LR

    %% ---- leaf modules (no internal deps) ----
    gas["gas<br/><small>perfect · equilibrium · park</small>"]
    transport["transport<br/><small>sutherland · keyes · power_law</small>"]
    atmosphere["atmosphere<br/><small>ussa76 · cira86</small>"]
    isentropic["isentropic"]
    turbulence["turbulence"]
    units["units"]
    math_utils["math_utils"]

    %% ---- core ----
    core["core<br/><small>FlowState</small>"]
    core --> turbulence

    %% ---- solvers (central hub) ----
    solvers["solvers<br/><small>solve()</small>"]
    solvers --> core
    solvers --> gas
    solvers --> transport
    solvers --> atmosphere
    solvers --> isentropic
    solvers --> turbulence
    solvers --> units
    solvers --> math_utils

    %% ---- io ----
    io["io<br/><small>read_config · write_data · print_summary</small>"]
    io --> core
    io -.-> atmosphere

    %% ---- cli ----
    cli["cli"]
    cli --> solvers
    cli --> io

    %% ---- styling ----
    classDef hub fill:#4a86c8,stroke:#2d5a8c,color:#fff
    classDef leaf fill:#e8e8e8,stroke:#999,color:#333
    classDef mid fill:#7ab648,stroke:#4a7a2a,color:#fff

    class solvers hub
    class cli mid
    class core mid
    class io mid
    class gas,transport,atmosphere,isentropic,turbulence,units,math_utils leaf
```

[Download full-resolution module graph (SVG)](../assets/deps.svg){ .md-button download="flow_state_deps.svg" }

## Modules

- [FlowState](core.md): The main dataclass containing flow properties
- [Solvers](solvers.md): The `solve()` function and input combinations
- [Atmosphere](atmosphere.md): Atmosphere models and `AtmosphereState`
- [Transport](transport.md): Viscosity models (Sutherland, Keyes, etc.)
