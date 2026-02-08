# BSD-3-Clause License - see LICENSE file
"""
Minimal Streamlit webapp for flow_state.

This app provides a web interface for computing compressible flow states
using the same builders as the CLI. No physics logic is duplicated here;
all calculations are delegated to the flow_state library.

Run with:
    streamlit run app.py
"""

import streamlit as st

# Import from the flow_state library - no duplicated physics
from flow_state import from_M_p_T
from flow_state._version import __version__
from flow_state.gas import PerfectGas
from flow_state.transport import Sutherland


def main() -> None:
    """Main Streamlit application."""
    st.set_page_config(
        page_title="flow_state",
        page_icon="🌊",
        layout="centered",
    )

    st.title("🌊 flow_state")
    st.markdown(f"*Compressible flow state calculator* — v{__version__}")

    st.divider()

    # Gas model selection
    st.subheader("Gas Model")

    gas_choice = st.selectbox(
        "Select gas model",
        options=["Air", "Nitrogen", "Custom"],
        index=0,
    )

    if gas_choice == "Custom":
        col1, col2 = st.columns(2)
        with col1:
            gamma = st.number_input(
                "Specific heat ratio (γ)",
                min_value=1.01,
                max_value=2.0,
                value=1.4,
                step=0.01,
                format="%.4f",
            )
        with col2:
            R = st.number_input(
                "Gas constant R [J/(kg·K)]",
                min_value=100.0,
                max_value=1000.0,
                value=287.05,
                step=1.0,
                format="%.2f",
            )
        gas = PerfectGas.custom(gamma=gamma, R=R, name="custom")
        transport = Sutherland.air()  # Use air as default for custom
    elif gas_choice == "Nitrogen":
        gas = PerfectGas.nitrogen()
        transport = Sutherland.nitrogen()
    else:
        gas = PerfectGas.air()
        transport = Sutherland.air()

    st.divider()

    # Flow conditions input
    st.subheader("Flow Conditions")

    col1, col2, col3 = st.columns(3)

    with col1:
        p = st.number_input(
            "Pressure [Pa]",
            min_value=1.0,
            max_value=1e8,
            value=101325.0,
            step=1000.0,
            format="%.2f",
        )

    with col2:
        T = st.number_input(
            "Temperature [K]",
            min_value=50.0,
            max_value=5000.0,
            value=300.0,
            step=10.0,
            format="%.2f",
        )

    with col3:
        M = st.number_input(
            "Mach number",
            min_value=0.0,
            max_value=50.0,
            value=2.0,
            step=0.1,
            format="%.4f",
        )

    st.divider()

    # Compute flow state
    if st.button("🧮 Compute Flow State", type="primary", use_container_width=True):
        try:
            # Use the same builder as the CLI - no duplicated logic
            state = from_M_p_T(
                M=M,
                p=p,
                T=T,
                gas=gas,
                transport=transport,
            )

            st.success("Flow state computed successfully!")

            # Display results
            st.subheader("Results")

            # Thermodynamic properties
            st.markdown("**Thermodynamic Properties**")
            col1, col2, col3 = st.columns(3)
            with col1:
                st.metric("Density", f"{state.dens:.6f} kg/m³")
            with col2:
                st.metric("γ", f"{state.gamma:.4f}")
            with col3:
                st.metric("r_gas", f"{state.r_gas:.2f} J/(kg·K)")

            # Kinematic properties
            st.markdown("**Kinematic Properties**")
            col1, col2, col3 = st.columns(3)
            with col1:
                st.metric("Speed of Sound", f"{state.a:.2f} m/s")
            with col2:
                st.metric("Mach", f"{state.M:.4f}")
            with col3:
                st.metric("uvel", f"{state.uvel:.2f} m/s")

            # Transport properties
            if state.visc_dyn is not None:
                st.markdown("**Transport Properties**")
                col1, col2, col3 = st.columns(3)
                with col1:
                    st.metric("visc_dyn", f"{state.visc_dyn:.4e} Pa·s")
                with col2:
                    if state.visc_kin is not None:
                        st.metric("visc_kin", f"{state.visc_kin:.4e} m²/s")
                with col3:
                    if state.re1 is not None:
                        st.metric("re1", f"{state.re1:.4e} 1/m")

            st.divider()

            # Download button for TOML
            toml_content = state.to_toml_string()
            st.download_button(
                label="📥 Download flow_state.toml",
                data=toml_content,
                file_name="flow_state.toml",
                mime="application/toml",
                use_container_width=True,
            )

            # Show raw TOML in expander
            with st.expander("📄 View TOML Output"):
                st.code(toml_content, language="toml")

        except Exception as e:
            st.error(f"Error computing flow state: {e}")

    # Footer
    st.divider()
    st.markdown(
        """
        <div style='text-align: center; color: gray; font-size: 0.8em;'>
        flow_state — BSD-3-Clause License<br>
        <a href="https://github.com/uahypersonics/flow_state">GitHub</a>
        </div>
        """,
        unsafe_allow_html=True,
    )


if __name__ == "__main__":
    main()
