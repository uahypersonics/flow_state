#!/usr/bin/env python3
"""
Generate atmosphere model comparison plots for documentation

Usage:
    python scripts/generate_atmosphere_plots.py
"""

# --------------------------------------------------
# import modules
# --------------------------------------------------
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.lines import Line2D

from flow_state.atmosphere import CIRA86, USSA76


# --------------------------------------------------
# main function
# --------------------------------------------------
def main() -> None:
    """Generate and save atmosphere model comparison plots"""

    # --------------------------------------------------
    # discretize altitude ranges for USSA76 and CIRA86
    # --------------------------------------------------
    alt_ussa76 = np.linspace(0, 86_000, 500)
    alt_cira86 = np.linspace(0, 120_000, 500)

    # --------------------------------------------------
    # configure models
    # --------------------------------------------------

    # USSA76 model instance
    ussa76 = USSA76()

    # CIRA86 configurations: (latitude, month, label)
    cira_configs = [
        # Northern Hemisphere
        (0,   1, "0° Jan"),
        (0,   7, "0° Jul"),
        (40,  1, "40°N Jan"),
        (40,  7, "40°N Jul"),
        (80,  1, "80°N Jan"),
        (80,  7, "80°N Jul"),
        # Southern Hemisphere
        (-40, 1, "40°S Jan"),
        (-40, 7, "40°S Jul"),
        (-80, 1, "80°S Jan"),
        (-80, 7, "80°S Jul"),
    ]

    # --------------------------------------------------
    # compute profiles for each model
    # --------------------------------------------------

    # USSA76 profiles
    temp_ussa = np.array([ussa76(h).temp for h in alt_ussa76])
    pres_ussa = np.array([ussa76(h).pres for h in alt_ussa76])
    dens_ussa = np.array([ussa76(h).dens for h in alt_ussa76])

    # CIRA86 profiles
    cira_profiles = {}
    for lat, month, label in cira_configs:
        model = CIRA86(latitude=lat, month=month)
        temps = np.array([model(h).temp for h in alt_cira86])
        press = np.array([model(h).pres for h in alt_cira86])
        denss = np.array([model(h).dens for h in alt_cira86])
        cira_profiles[label] = (temps, press, denss)

    # --------------------------------------------------
    # plot results
    # --------------------------------------------------
    # latitude -> color, month -> linestyle
    lat_colors = {
        "0°":   "#1976D2",   # blue (equator)
        "40°N": "#388E3C",   # green (NH mid)
        "80°N": "#D32F2F",   # red (NH polar)
        "40°S": "#7B1FA2",   # purple (SH mid)
        "80°S": "#E65100",   # orange (SH polar)
    }
    month_styles = {
        "Jan": "-",     # solid
        "Jul": "--",    # dashed
    }

    def _get_style(label: str) -> tuple[str, str]:
        """Extract color key and month key from label like '40°N Jan'."""
        parts = label.split()
        return parts[0], parts[1]

    # --------------------------------------------------
    # plot
    # --------------------------------------------------
    fig, axes = plt.subplots(1, 3, figsize=(14, 6), sharey=True)
    alt_ussa_km = alt_ussa76 / 1000
    alt_cira_km = alt_cira86 / 1000

    # --- temperature ---
    ax = axes[0]
    ax.plot(temp_ussa, alt_ussa_km, color="k", linewidth=2, label="USSA76")
    for lat, month, label in cira_configs:
        lat_key, mon_key = _get_style(label)
        ax.plot(cira_profiles[label][0], alt_cira_km,
                color=lat_colors[lat_key], linestyle=month_styles[mon_key],
                linewidth=1.2)
    ax.set_xlabel("Temperature [K]")
    ax.set_ylabel("Altitude [km]")
    ax.set_title("Temperature")
    ax.set_xlim(left=140)
    ax.grid(True, alpha=0.3)

    # --- pressure (log scale) ---
    ax = axes[1]
    ax.plot(pres_ussa, alt_ussa_km, color="k", linewidth=2, label="USSA76")
    for lat, month, label in cira_configs:
        lat_key, mon_key = _get_style(label)
        ax.plot(cira_profiles[label][1], alt_cira_km,
                color=lat_colors[lat_key], linestyle=month_styles[mon_key],
                linewidth=1.2)
    ax.set_xscale("log")
    ax.set_xlabel("Pressure [Pa]")
    ax.set_title("Pressure")
    ax.grid(True, alpha=0.3, which="both")

    # --- density (log scale) ---
    ax = axes[2]
    ax.plot(dens_ussa, alt_ussa_km, color="k", linewidth=2, label="USSA76")
    for lat, month, label in cira_configs:
        lat_key, mon_key = _get_style(label)
        ax.plot(cira_profiles[label][2], alt_cira_km,
                color=lat_colors[lat_key], linestyle=month_styles[mon_key],
                linewidth=1.2)
    ax.set_xscale("log")
    ax.set_xlabel("Density [kg/m³]")
    ax.set_title("Density")
    ax.grid(True, alpha=0.3, which="both")

    # --------------------------------------------------
    # unified legend
    # --------------------------------------------------
    legend_elements = [
        Line2D([0], [0], color="k", linewidth=2, label="USSA76"),
        Line2D([0], [0], color=lat_colors["0°"],   linewidth=1.2, label="CIRA86  0°"),
        Line2D([0], [0], color=lat_colors["40°N"],  linewidth=1.2, label="CIRA86  40°N"),
        Line2D([0], [0], color=lat_colors["80°N"],  linewidth=1.2, label="CIRA86  80°N"),
        Line2D([0], [0], color=lat_colors["40°S"],  linewidth=1.2, label="CIRA86  40°S"),
        Line2D([0], [0], color=lat_colors["80°S"],  linewidth=1.2, label="CIRA86  80°S"),
        Line2D([0], [0], color="grey", linestyle="-",  linewidth=1.2, label="January"),
        Line2D([0], [0], color="grey", linestyle="--", linewidth=1.2, label="July"),
    ]
    fig.legend(
        handles=legend_elements,
        loc="lower center",
        ncol=4,
        frameon=False,
        fontsize=8,
        bbox_to_anchor=(0.5, -0.05),
    )

    fig.suptitle("Atmosphere Model Comparison", fontsize=14, fontweight="bold")
    fig.tight_layout(rect=[0, 0.05, 1, 0.95])

    # --------------------------------------------------
    # save
    # --------------------------------------------------
    repo_root = Path(__file__).resolve().parent.parent
    out = repo_root / "docs" / "assets" / "atmosphere_comparison.png"
    fig.savefig(out, dpi=200, bbox_inches="tight", facecolor="white")
    print(f"saved: {out}")
    plt.close(fig)

# --------------------------------------------------
# main entry point when run as script
# --------------------------------------------------
if __name__ == "__main__":
    main()
