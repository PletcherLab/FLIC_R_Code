from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd

from .events import expand_events, get_events, get_surviving_events, link_events
from ..parameters import Parameters


@dataclass(slots=True)
class FeedingDerived:
    lick_data: pd.DataFrame
    event_data: pd.DataFrame
    durations: dict[str, pd.DataFrame | int]
    intervals: dict[str, pd.DataFrame | int]
    dual_feeding_data: pd.DataFrame | None = None
    lick_matrix: np.ndarray | None = None


def _well_col(well: int) -> str:
    return f"W{well}"


def set_feeding_data(
    baselined: pd.DataFrame,
    thresholds: dict[str, pd.DataFrame],
    *,
    params: Parameters,
    correct_for_dual_feeding: bool,
    check_simultaneous_feeding_fn,
    adjust_dual_feeding_fn,
) -> tuple[pd.DataFrame, pd.DataFrame, np.ndarray | None, pd.DataFrame | None]:
    """
    Port of `Set.Feeding.Data()` including optional dual-feeding correction.
    """

    lick_df = baselined.copy()
    event_df = baselined.copy()

    for well in range(1, 13):
        cname = _well_col(well)
        thr = thresholds[cname]
        data = baselined[cname].to_numpy()

        feeding_min = data > thr["FeedingMin"].to_numpy()
        feeding_max = data > thr["FeedingMax"].to_numpy()

        events = get_surviving_events(feeding_min, feeding_max)
        events[events < params.feeding_minevents] = 0

        licks = expand_events(events)
        bridged = link_events(licks, int(params.feeding_event_link_gap))
        events = get_events(bridged)

        lick_df[cname] = licks
        event_df[cname] = events

    lick_matrix = None
    dual_feeding_data = None

    if params.chamber_size == 2:
        # `CheckForSimultaneousFeeding.DFM` works on dfm$LickData + dfm$BaselineData.
        lick_matrix = check_simultaneous_feeding_fn(lick_df, baselined, params)
        if correct_for_dual_feeding:
            adj = adjust_dual_feeding_fn(lick_df, baselined, params)
            adj_baselined = adj["baselined"]
            dual_feeding_data = adj["dual_feeding_data"]

            # Recompute per-well events using the adjusted baselined data.
            lick_df2 = adj_baselined.copy()
            event_df2 = adj_baselined.copy()
            for well in range(1, 13):
                cname = _well_col(well)
                thr = thresholds[cname]
                data = adj_baselined[cname].to_numpy()

                feeding_min = data > thr["FeedingMin"].to_numpy()
                feeding_max = data > thr["FeedingMax"].to_numpy()

                events = get_surviving_events(feeding_min, feeding_max)
                events[events < params.feeding_minevents] = 0

                licks = expand_events(events)
                bridged = link_events(licks, int(params.feeding_event_link_gap))
                events = get_events(bridged)

                lick_df2[cname] = licks
                event_df2[cname] = events

            # Keep original baselined in the returned DFM, like the R code does.
            lick_df = lick_df2
            event_df = event_df2

    return lick_df, event_df, lick_matrix, dual_feeding_data


def bout_durations_and_intervals(
    baselined: pd.DataFrame,
    event_df: pd.DataFrame,
    *,
    params: Parameters,
) -> tuple[dict[str, pd.DataFrame | int], dict[str, pd.DataFrame | int]]:
    """
    Port of `Set.Durations.And.Intervals()` returning dicts of per-well DataFrames.
    """

    durations: dict[str, pd.DataFrame | int] = {}
    intervals: dict[str, pd.DataFrame | int] = {}

    spm = float(params.samples_per_second)
    for well in range(1, 13):
        cname = _well_col(well)
        data = baselined[cname].to_numpy()
        events = event_df[cname].to_numpy()

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

        # Intervals: derived from licks boolean (Expand.Events(events) in R is not used here in final).
        licks_bool = expand_events(events)
        # Get intervals of FALSE-runs between lick episodes.
        from .events import get_intervals

        bout_int = get_intervals(licks_bool)
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

