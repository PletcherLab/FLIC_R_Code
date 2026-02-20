from __future__ import annotations

import numpy as np
import pandas as pd

from .events import expand_events, get_events, get_intervals
from ..parameters import Parameters


def set_tasting_data(
    baselined: pd.DataFrame,
    thresholds: dict[str, pd.DataFrame],
    feeding_lick_df: pd.DataFrame,
    *,
    params: Parameters,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """
    Port of `Set.Tasting.Data()` and `Set.Tasting.Data.Well()`.
    """

    tasting_df = baselined.copy()
    tasting_event_df = baselined.copy()

    for well in range(1, 13):
        cname = f"W{well}"
        thr = thresholds[cname]
        data = baselined[cname].to_numpy()

        licks = (data > thr["TastingMin"].to_numpy()) & (data < thr["TastingMax"].to_numpy())

        feeding_licks = feeding_lick_df[cname].to_numpy(dtype=bool)
        licks[feeding_licks] = False

        events = get_events(licks)
        events[events < params.tasting_minevents] = 0

        tasting_df[cname] = licks
        tasting_event_df[cname] = events

    return tasting_df, tasting_event_df


def tasting_bout_durations_and_intervals(
    baselined: pd.DataFrame,
    tasting_event_df: pd.DataFrame,
    *,
    params: Parameters,
    interval_source_lick_df: pd.DataFrame,
) -> tuple[dict[str, pd.DataFrame | int], dict[str, pd.DataFrame | int]]:
    """
    Port of `Set.Tasting.Durations.And.Intervals()`.

    Note: the R code computes "tasting" intervals from `FeedingData.Well.Licks(...)` (likely a bug),
    so this function requires the caller to pass the lick-data source explicitly.
    """

    spm = float(params.samples_per_second)
    durations: dict[str, pd.DataFrame | int] = {}
    intervals: dict[str, pd.DataFrame | int] = {}

    for well in range(1, 13):
        cname = f"W{well}"
        data = baselined[cname].to_numpy()
        events = tasting_event_df[cname].to_numpy()

        starts = np.flatnonzero(events > 0)
        bout_durs = events[starts].astype(int)

        if starts.size == 0:
            durations[cname] = 0
        else:
            mins = baselined.loc[starts, "Minutes"].to_numpy()
            max_int = np.zeros_like(starts, dtype=float)
            min_int = np.zeros_like(starts, dtype=float)
            sum_int = np.zeros_like(starts, dtype=float)
            avg_int = np.zeros_like(starts, dtype=float)
            var_int = np.zeros_like(starts, dtype=float)

            n = len(data)
            for i, (idx, L) in enumerate(zip(starts, bout_durs, strict=False)):
                lo = int(idx)
                hi = min(n, lo + int(L))
                seg = data[lo:hi]
                max_int[i] = float(np.max(seg))
                min_int[i] = float(np.min(seg))
                sum_int[i] = float(np.sum(seg))
                avg_int[i] = float(np.mean(seg))
                var_int[i] = float(np.var(seg, ddof=1)) if seg.size > 1 else float("nan")

            dur_df = pd.DataFrame(
                {
                    "Minutes": mins,
                    "Licks": bout_durs.astype(int),
                    "Duration": bout_durs.astype(float) / spm,
                    "TotalIntensity": sum_int,
                    "AvgIntensity": avg_int,
                    "MinIntensity": min_int,
                    "MaxIntensity": max_int,
                    "VarIntensity": var_int,
                }
            )
            durations[cname] = dur_df

        # R uses `Get.Intervals(FeedingData.Well.Licks(...))` even for tasting.
        licks_src = interval_source_lick_df[cname].to_numpy(dtype=bool)
        bout_int = get_intervals(licks_src)
        idxs = np.flatnonzero(bout_int > 0)
        if idxs.size == 0:
            intervals[cname] = 0
        else:
            tmp = baselined.iloc[idxs]
            ints_df = pd.DataFrame(
                {
                    "Minutes": tmp["Minutes"].to_numpy(),
                    "Sample": tmp["Sample"].to_numpy() if "Sample" in tmp.columns else idxs + 1,
                    "IntervalSec": bout_int[idxs].astype(float) / spm,
                }
            )
            intervals[cname] = ints_df

    return durations, intervals

