#!/usr/bin/env python3
"""
Generate a globe schematic showing the CIRA86 latitude grid for documentation.

Draws a sphere with latitude lines at the CIRA86 10° increments and labels
each with representative geographic locations.

Usage:
    python scripts/generate_latitude_grid.py
"""

# --------------------------------------------------
# import modules
# --------------------------------------------------
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np


def main() -> None:
    """Generate and save the CIRA86 latitude grid globe schematic."""

    fig, ax = plt.subplots(figsize=(8, 7))

    R = 1.0  # globe radius

    # --------------------------------------------------
    # draw globe outline (full circle)
    # --------------------------------------------------
    theta_full = np.linspace(0, 2 * np.pi, 300)
    ax.plot(R * np.cos(theta_full), R * np.sin(theta_full),
            color="#455A64", linewidth=1.5, zorder=3)

    # light fill for northern hemisphere
    theta_upper = np.linspace(0, np.pi, 200)
    ax.fill_between(
        R * np.cos(theta_upper), 0, R * np.sin(theta_upper),
        color="#B3E5FC", alpha=0.15, edgecolor="none",
    )
    # lighter fill for southern hemisphere
    theta_lower = np.linspace(np.pi, 2 * np.pi, 200)
    ax.fill_between(
        R * np.cos(theta_lower), 0, R * np.sin(theta_lower),
        color="#FFCCBC", alpha=0.10, edgecolor="none",
    )

    # equator line (thicker)
    ax.plot([-R, R], [0, 0], color="#455A64", linewidth=1.2, zorder=2)

    # polar axis (vertical)
    ax.plot([0, 0], [-R, R], color="#B0BEC5", linewidth=0.6,
            linestyle=":", zorder=1)

    # --------------------------------------------------
    # CIRA86 latitude grid: -80° to 80° in 20° increments
    # drawn as horizontal lines across the globe cross-section
    # --------------------------------------------------
    cira_lats = list(range(-80, 100, 20))  # -80, -60, -40, -20, 0, 20, 40, 60, 80

    # location annotations: (latitude, label)
    annotations = {
        80:  "High Arctic",
        60:  "Alaska, Scandinavia",
        40:  "Central US, Southern Europe",
        20:  "Hawaii, Northern India",
        0:   "Equator",
        -20: "Northern Australia, Brazil",
        -40: "New Zealand, Patagonia",
        -60: "Southern Ocean",
        -80: "Antarctica",
    }

    for lat_deg in cira_lats:
        lat_rad = np.radians(lat_deg)
        y = R * np.sin(lat_rad)
        half_width = R * np.cos(lat_rad)

        # draw latitude line (skip equator, already drawn)
        if lat_deg != 0:
            ax.plot(
                [-half_width, half_width], [y, y],
                color="#90A4AE", linewidth=0.8, linestyle="--", zorder=2,
            )

        # tick mark on right edge
        ax.plot(half_width, y, "o", color="#455A64", markersize=4, zorder=5)

    # --------------------------------------------------
    # location annotations with arrows (right side)
    # --------------------------------------------------
    for lat_deg in cira_lats:
        lat_rad = np.radians(lat_deg)
        y = R * np.sin(lat_rad)
        half_width = R * np.cos(lat_rad)

        label = annotations.get(lat_deg, "")
        if not label:
            continue

        # format: "Location  (lat°N/S)"
        if lat_deg > 0:
            full_label = f"{label}  ({lat_deg}°N)"
        elif lat_deg < 0:
            full_label = f"{label}  ({abs(lat_deg)}°S)"
        else:
            full_label = label

        x_text = 1.35
        y_text = y

        ax.annotate(
            full_label,
            xy=(half_width, y),
            xytext=(x_text, y_text),
            fontsize=8,
            color="#37474F",
            ha="left",
            va="center",
            arrowprops=dict(
                arrowstyle="-",
                color="#B0BEC5",
                linewidth=0.8,
            ),
        )

    # --------------------------------------------------
    # hemisphere labels
    # --------------------------------------------------
    ax.text(0, 0.55, "Northern\nHemisphere", ha="center", va="center",
            fontsize=9, color="#546E7A", fontstyle="italic", alpha=0.7)
    ax.text(0, -0.55, "Southern\nHemisphere", ha="center", va="center",
            fontsize=9, color="#BF360C", fontstyle="italic", alpha=0.5)

    # pole labels
    ax.text(0, R + 0.08, "N", fontsize=10, ha="center", va="bottom",
            fontweight="bold", color="#455A64")
    ax.text(0, -R - 0.08, "S", fontsize=10, ha="center", va="top",
            fontweight="bold", color="#BF360C")

    # --------------------------------------------------
    # title and formatting
    # --------------------------------------------------
    ax.set_title("CIRA86 Latitude Grid", fontsize=13, fontweight="bold", pad=15)
    ax.set_xlim(-1.4, 3.6)
    ax.set_ylim(-1.35, 1.35)
    ax.set_aspect("equal")
    ax.axis("off")
    fig.tight_layout()

    # --------------------------------------------------
    # save
    # --------------------------------------------------
    repo_root = Path(__file__).resolve().parent.parent
    out = repo_root / "docs" / "assets" / "cira86_latitude_grid.png"
    fig.savefig(out, dpi=200, bbox_inches="tight", facecolor="white")
    print(f"saved: {out}")
    plt.close(fig)


if __name__ == "__main__":
    main()
