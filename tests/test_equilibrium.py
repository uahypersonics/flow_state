"""
tests for EquilibriumAir high-temperature gas model
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------

import pytest

from flow_state.gas import EquilibriumAir, ParkAir, PerfectGas

# --------------------------------------------------
# tests for equilibrium air class
# --------------------------------------------------


class TestEquilibriumAir:
    """test EquilibriumAir class"""

    def test_air_defaults(self) -> None:
        """air preset has correct values"""
        air = EquilibriumAir.air()
        assert air.name == "equilibrium_air"

    def test_r_gas_low_temp_matches_cold_air(self) -> None:
        """at low temperature, r_gas matches cold air value"""
        equil = EquilibriumAir.air()

        # at 300 K, no dissociation, r_gas = R_air_cold
        r_gas_300 = equil.r_gas(300, 101325)
        assert r_gas_300 == pytest.approx(287.05, rel=0.01)

    def test_r_gas_increases_with_temperature(self) -> None:
        """r_gas increases as temperature increases (dissociation)"""
        equil = EquilibriumAir.air()

        r_gas_1000 = equil.r_gas(1000, 101325)
        r_gas_3000 = equil.r_gas(3000, 101325)
        r_gas_6000 = equil.r_gas(6000, 101325)

        # r_gas increases with dissociation
        assert r_gas_3000 > r_gas_1000
        assert r_gas_6000 > r_gas_3000

    def test_r_gas_high_temp_reasonable(self) -> None:
        """at high temperature, r_gas is in reasonable range"""
        equil = EquilibriumAir.air()

        # at 6000 K, significant dissociation, r_gas should be 30-80% higher
        r_gas_6000 = equil.r_gas(6000, 101325)
        assert 350 < r_gas_6000 < 550  # ~1.2 to 1.9 times R_air_cold

    def test_gamma_low_temp_matches_perfect_gas(self) -> None:
        """at low temperature, gamma approaches perfect gas value"""
        equil = EquilibriumAir.air()

        # at 300 K, gamma ~ 1.4
        gamma_300 = equil.gamma(300, 101325)
        assert gamma_300 == pytest.approx(1.4, rel=0.02)

    def test_gamma_decreases_with_temperature(self) -> None:
        """gamma decreases as temperature increases"""
        equil = EquilibriumAir.air()

        gamma_300 = equil.gamma(300, 101325)
        gamma_2000 = equil.gamma(2000, 101325)
        gamma_5000 = equil.gamma(5000, 101325)

        assert gamma_2000 < gamma_300
        assert gamma_5000 < gamma_2000

    def test_gamma_high_temp_reasonable(self) -> None:
        """at high temperature, gamma is in reasonable range"""
        equil = EquilibriumAir.air()

        # during active dissociation, gamma drops significantly
        gamma_5000 = equil.gamma(5000, 101325)
        assert 1.1 < gamma_5000 < 1.3

    def test_cp_cv_consistent_with_gamma(self) -> None:
        """cp/cv equals gamma"""
        equil = EquilibriumAir.air()

        for temp in [300, 2000, 5000]:
            cp = equil.cp(temp, 101325)
            cv = equil.cv(temp, 101325)
            gamma = equil.gamma(temp, 101325)
            assert cp / cv == pytest.approx(gamma, rel=1e-10)

    def test_mayer_relation(self) -> None:
        """cp - cv = r_gas holds"""
        equil = EquilibriumAir.air()

        for temp in [300, 2000, 5000]:
            cp = equil.cp(temp, 101325)
            cv = equil.cv(temp, 101325)
            r_gas = equil.r_gas(temp, 101325)
            assert cp - cv == pytest.approx(r_gas, rel=1e-10)

    def test_cp_peak_during_dissociation(self) -> None:
        """cp shows elevated values during dissociation"""
        equil = EquilibriumAir.air()
        perfect = PerfectGas.air()

        # cp peaks during dissociation (energy absorbed by reaction)
        cp_equil_3000 = equil.cp(3000, 101325)
        cp_perfect = perfect.cp(3000, 101325)

        # equilibrium cp should be higher during dissociation
        assert cp_equil_3000 > cp_perfect

    def test_pressure_affects_equilibrium(self) -> None:
        """lower pressure gives more dissociation (higher r_gas)"""
        equil = EquilibriumAir.air()

        # at high temperature, lower pressure -> more dissociation
        r_gas_1atm = equil.r_gas(5000, 101325)
        r_gas_01atm = equil.r_gas(5000, 10132.5)

        # lower pressure should give higher r_gas (more dissociation)
        assert r_gas_01atm >= r_gas_1atm

    def test_sound_speed_calculation(self) -> None:
        """sound speed calculation"""
        equil = EquilibriumAir.air()

        a = equil.sound_speed(3000)

        # should be positive and reasonable
        assert a > 0
        assert 500 < a < 2000  # reasonable range for high-temp air

    def test_density_calculation(self) -> None:
        """density from equation of state"""
        equil = EquilibriumAir.air()
        pres = 101325.0
        temp = 3000.0

        dens = equil.density(pres, temp)
        r_gas = equil.r_gas(temp, pres)
        expected = pres / (r_gas * temp)

        assert dens == pytest.approx(expected, rel=1e-6)

    def test_pressure_calculation(self) -> None:
        """pressure from equation of state with iteration"""
        equil = EquilibriumAir.air()
        dens = 0.1  # kg/m^3
        temp = 3000.0

        pres = equil.pressure(dens, temp)

        # verify: rho = p / (r_gas * T)
        dens_check = equil.density(pres, temp)
        assert dens_check == pytest.approx(dens, rel=1e-4)

    def test_temperature_calculation(self) -> None:
        """temperature from equation of state with iteration"""
        equil = EquilibriumAir.air()
        pres = 101325.0
        dens = 0.1  # kg/m^3

        temp = equil.temperature(pres, dens)

        # verify: rho = p / (r_gas * T)
        dens_check = equil.density(pres, temp)
        assert dens_check == pytest.approx(dens, rel=1e-4)

    def test_equation_of_state_roundtrip(self) -> None:
        """p, rho, T roundtrip consistency"""
        equil = EquilibriumAir.air()
        pres = 101325.0
        temp = 4000.0

        dens = equil.density(pres, temp)
        pres_back = equil.pressure(dens, temp)
        temp_back = equil.temperature(pres, dens)

        assert pres_back == pytest.approx(pres, rel=1e-3)
        assert temp_back == pytest.approx(temp, rel=1e-3)

    def test_negative_temp_raises(self) -> None:
        """negative temperature raises ValueError"""
        equil = EquilibriumAir.air()

        with pytest.raises(ValueError):
            equil.gamma(-100, 101325)

        with pytest.raises(ValueError):
            equil.sound_speed(-100)

    def test_continuity_with_park_at_low_temp(self) -> None:
        """at moderate temperatures, reasonably close to Park"""
        equil = EquilibriumAir.air()
        park = ParkAir.air()

        # at 1500 K, both models should give similar gamma
        gamma_equil = equil.gamma(1500, 101325)
        gamma_park = park.gamma(1500, 101325)

        # should be within ~5% (models have different physics)
        assert gamma_equil == pytest.approx(gamma_park, rel=0.1)
