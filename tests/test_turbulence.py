"""
tests for turbulence module (Kolmogorov scales)
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------

import pytest

from flow_state.turbulence import (
    kolmogorov_scales,
)

# --------------------------------------------------
# tests for kolmogorov scale calculations
# --------------------------------------------------


class TestKolmogorovScales:
    """test Kolmogorov scale calculations"""

    def test_basic_calculation(self) -> None:
        """basic Kolmogorov scale calculation"""
        nu = 1.5e-5  # kinematic viscosity [m²/s]
        U = 100.0    # velocity [m/s]
        L = 1.0      # length [m]

        scales = kolmogorov_scales(nu, U, L)

        assert scales.eta > 0
        assert scales.tau > 0
        assert scales.vel > 0

    def test_eta_scaling(self) -> None:
        """η/L ~ Re^(-3/4)"""
        nu = 1.5e-5
        U = 100.0
        L = 1.0

        scales = kolmogorov_scales(nu, U, L)
        Re = U * L / nu

        # η/L should scale as Re^(-3/4)
        expected_ratio = Re ** (-0.75)
        actual_ratio = scales.eta / L

        assert actual_ratio == pytest.approx(expected_ratio, rel=1e-6)

    def test_tau_scaling(self) -> None:
        """τ/(L/U) ~ Re^(-1/2)"""
        nu = 1.5e-5
        U = 100.0
        L = 1.0

        scales = kolmogorov_scales(nu, U, L)
        Re = U * L / nu

        # τ/(L/U) should scale as Re^(-1/2)
        T_large = L / U
        expected_ratio = Re ** (-0.5)
        actual_ratio = scales.tau / T_large

        assert actual_ratio == pytest.approx(expected_ratio, rel=1e-6)

    def test_velocity_scaling(self) -> None:
        """u_η/U ~ Re^(-1/4)"""
        nu = 1.5e-5
        U = 100.0
        L = 1.0

        scales = kolmogorov_scales(nu, U, L)
        Re = U * L / nu

        # u_η/U should scale as Re^(-1/4)
        expected_ratio = Re ** (-0.25)
        actual_ratio = scales.vel / U

        assert actual_ratio == pytest.approx(expected_ratio, rel=1e-6)

    def test_eta_tau_ueta_consistency(self) -> None:
        """u_η = η / τ"""
        nu = 1.5e-5
        U = 100.0
        L = 1.0

        scales = kolmogorov_scales(nu, U, L)

        assert scales.vel == pytest.approx(scales.eta / scales.tau, rel=1e-6)

    def test_higher_re_smaller_scales(self) -> None:
        """higher Reynolds number gives smaller Kolmogorov scales"""
        nu = 1.5e-5
        L = 1.0

        scales_low_re = kolmogorov_scales(nu, uvel=10.0, lref=L)
        scales_high_re = kolmogorov_scales(nu, uvel=100.0, lref=L)

        # higher Re should have smaller η
        assert scales_high_re.eta < scales_low_re.eta
        assert scales_high_re.tau < scales_low_re.tau

    def test_negative_viscosity_raises(self) -> None:
        """negative viscosity raises error"""
        with pytest.raises(ValueError):
            kolmogorov_scales(visc_kin=-1e-5, uvel=100, lref=1.0)

    def test_negative_velocity_raises(self) -> None:
        """negative velocity raises error"""
        with pytest.raises(ValueError):
            kolmogorov_scales(visc_kin=1e-5, uvel=-100, lref=1.0)

    def test_negative_length_raises(self) -> None:
        """negative length raises error"""
        with pytest.raises(ValueError):
            kolmogorov_scales(visc_kin=1e-5, uvel=100, lref=-1.0)
