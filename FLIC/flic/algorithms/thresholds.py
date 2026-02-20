from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd

from ..parameters import Parameters


@dataclass(frozen=True, slots=True)
class WellThresholds:
    feeding_max: np.ndarray
    feeding_min: np.ndarray
    tasting_max: np.ndarray
    tasting_min: np.ndarray


def fixed_thresholds_for_well(
    baselined_well: np.ndarray, *, n: int, params: Parameters
) -> WellThresholds:
    """
    Port of `Set.Fixed.Threshold.Well()` from `PrivateFunctions.R`.

    If feeding_threshold is negative, per-well thresholds are computed as max(signal) * abs(multiplier).
    """

    if params.feeding_threshold < 0:
        mx = float(np.nanmax(baselined_well))
        feeding_max = round(mx * abs(params.feeding_threshold), 0)
        feeding_min = round(mx * abs(params.feeding_minimum), 0)
        tasting_min = round(mx * abs(params.tasting_minimum), 0)
        tasting_max = round(mx * abs(params.tasting_maximum), 0)
    else:
        feeding_max = params.feeding_threshold
        feeding_min = params.feeding_minimum
        tasting_min = params.tasting_minimum
        tasting_max = params.tasting_maximum

    return WellThresholds(
        feeding_max=np.full(n, feeding_max, dtype=float),
        feeding_min=np.full(n, feeding_min, dtype=float),
        tasting_max=np.full(n, tasting_max, dtype=float),
        tasting_min=np.full(n, tasting_min, dtype=float),
    )


def build_thresholds_table(baselined: pd.DataFrame, params: Parameters) -> dict[str, pd.DataFrame]:
    """
    Port of `Set.Fixed.Threshold()` + `Thresholds.Well()`.

    Returns dict mapping 'W1'..'W12' to a DataFrame with columns:
    FeedingMax, FeedingMin, TastingMax, TastingMin
    """

    n = len(baselined)
    out: dict[str, pd.DataFrame] = {}
    for well in range(1, 13):
        cname = f"W{well}"
        thr = fixed_thresholds_for_well(baselined[cname].to_numpy(), n=n, params=params)
        df = pd.DataFrame(
            {
                "FeedingMax": thr.feeding_max,
                "FeedingMin": thr.feeding_min,
                "TastingMax": thr.tasting_max,
                "TastingMin": thr.tasting_min,
            }
        )
        out[cname] = df
    return out

