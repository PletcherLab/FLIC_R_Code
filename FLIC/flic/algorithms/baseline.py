from __future__ import annotations

import numpy as np
import pandas as pd


def running_median(x: np.ndarray, window: int) -> np.ndarray:
    """
    Approximation of R `stats::runmed(x, k)` used in `CalculateBaseline()`.

    Behavior: centered window; at edges use the available samples (min_periods=1).
    Window is forced to odd.

    Uses pandas rolling median (C-level, GIL-releasing) for O(n log window)
    performance and true thread-level parallelism when multiple DFMs are loaded
    concurrently.
    """

    x = np.asarray(x, dtype=float)
    if x.size == 0:
        return x.copy()
    if window < 1:
        raise ValueError("window must be >= 1")
    if window % 2 == 0:
        window += 1

    return (
        pd.Series(x)
        .rolling(window, center=True, min_periods=1)
        .median()
        .to_numpy(dtype=float)
    )


def baseline_subtract(raw: np.ndarray, window_samples: int) -> np.ndarray:
    """
    Baseline subtraction used by `CalculateBaseline()`:
    subtract running median from the raw signal.
    """

    raw = np.asarray(raw, dtype=float)
    med = running_median(raw, window_samples)
    return raw - med

