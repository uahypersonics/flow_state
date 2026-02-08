"""
bisection root finding to avoid scipy dependency.
"""

# --------------------------------------------------
# load necessary modules
# --------------------------------------------------
from collections.abc import Callable


# --------------------------------------------------
# main code: bisection root finder (avoids scipy dependency)
# --------------------------------------------------
def bisect(
    f: Callable[[float], float],
    lo: float,
    hi: float,
    rel_tol: float = 1e-10,
    abs_tol: float | None = None,
    max_iter: int = 100,
) -> float:
    """
    Find x in [lo, hi] such that f(x) ≈ 0 using bisection.

    Args:
        f: Function to find root of (should change sign over [lo, hi])
        lo: Lower bound
        hi: Upper bound
        rel_tol (optional): Relative tolerance for interval convergence (hi-lo)/|hi|. Defaults to 1e-10.
        abs_tol (optional): Absolute tolerance for x interval. If provided, uses this instead of rel_tol. Defaults to None.
        max_iter (optional): Maximum iterations. Defaults to 100.

    Returns:
        x such that f(x) ≈ 0

    Raises:
        ValueError: If no sign change detected or no convergence.

    Example:
        >>> from flow_state.math_utils import bisect
        >>> root = bisect(lambda x: x**2 - 2, 0, 2)  # sqrt(2)
        >>> abs(root - 1.41421356) < 1e-6
        True
    """

    # --------------------------------------------------
    # compute function values for initial bounds
    # --------------------------------------------------
    f_lo, f_hi = f(lo), f(hi)

    # --------------------------------------------------
    # check for sign change over interval (if none raise error, no root on interval)
    # --------------------------------------------------
    if f_lo * f_hi > 0:
        raise ValueError(
            f"No sign change: f({lo})={f_lo:.3e}, f({hi})={f_hi:.3e}. "
            "Check bounds or ensure solution exists in range."
        )

    # --------------------------------------------------
    # main bisection loop: loop until convergence or max iterations reached
    # --------------------------------------------------
    for _ in range(max_iter):

        # compute midpoint and corresponding function value
        mid = 0.5 * (lo + hi)
        f_mid = f(mid)

        # --------------------------------------------------
        # check convergence based on x-interval width (not f(x) value)
        # rationale: tolerance on x is more predictable than on f(x) because:
        #   - near a root, f(x) can be tiny but x still imprecise (shallow slope)
        #   - or f(x) can be "large" but x very accurate (steep slope)
        # this behavior matches the scipy brentq/bisect approach
        # --------------------------------------------------
        if f_mid == 0.0:
            # exact root found, nothing else to be done
            return mid
        if abs_tol is not None:
            # absolute tolerance: stop when interval width < abs_tol
            if (hi - lo) < abs_tol:
                return mid
        else:
            # relative tolerance: stop when interval width < rel_tol * |hi|
            if (hi - lo) / max(abs(hi), 1e-10) < rel_tol:
                return mid

        # update bounds based on sign of f at midpoint
        if f_mid * f_lo < 0:
            hi = mid
            f_hi = f_mid
        else:
            lo = mid
            f_lo = f_mid

    # if we reach here, max iterations were exceeded without convergence
    raise ValueError(f"Bisection did not converge after {max_iter} iterations")
