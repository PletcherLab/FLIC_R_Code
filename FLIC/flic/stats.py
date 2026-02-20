from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, Sequence

import numpy as np
import pandas as pd

from .lights import get_lights_info
from .parameters import Parameters
from .utils import range_is_specified


def _apply_range_mask(df: pd.DataFrame, range_minutes: Sequence[float]) -> pd.DataFrame:
    if not range_is_specified(range_minutes):
        return df
    a, b = float(range_minutes[0]), float(range_minutes[1])
    return df[(df["Minutes"] > a) & (df["Minutes"] <= b)]


def _chamber_from_well(well: int, params: Parameters) -> int:
    if params.chamber_size == 1:
        return well
    # Mirrors `GetChamberFromWell` mapping by well number.
    return (well - 1) // 2 + 1


def _tcwell_from_well(well: int, params: Parameters) -> str | None:
    if params.chamber_size != 2:
        return None
    even = (well % 2 == 0)
    # `pi_direction` defines whether WellA is the left or right well in each two-well chamber.
    # With default chamber_sets (1,2),(3,4),... odd wells are "left" and even wells are "right".
    if params.pi_direction == "left":
        return "WellA" if not even else "WellB"
    if params.pi_direction == "right":
        return "WellB" if not even else "WellA"
    raise ValueError(f"Invalid pi_direction: {params.pi_direction!r}")


def _interval_summary(interval_df: pd.DataFrame | int, range_minutes: Sequence[float]) -> tuple[float, float]:
    if not isinstance(interval_df, pd.DataFrame) or interval_df.empty:
        return 0.0, 0.0
    tmp = _apply_range_mask(interval_df, range_minutes)
    if tmp.empty:
        return 0.0, 0.0
    return float(tmp["IntervalSec"].mean()), float(tmp["IntervalSec"].median())


def _duration_summary(dur_df: pd.DataFrame | int, range_minutes: Sequence[float]) -> tuple[float, float]:
    if not isinstance(dur_df, pd.DataFrame) or dur_df.empty:
        return np.nan, np.nan
    tmp = _apply_range_mask(dur_df, range_minutes)
    if tmp.empty:
        return np.nan, np.nan
    return float(tmp["Duration"].mean()), float(tmp["Duration"].median())


def _intensity_summary(
    baselined_well: np.ndarray,
    event_vec: np.ndarray,
    *,
    range_mask: np.ndarray | None = None,
) -> tuple[float, float, float, float]:
    # R: da <- d[l] where l = Expand.Events(events)
    from .algorithms.events import expand_events

    licks = expand_events(event_vec.astype(int))
    if range_mask is not None:
        licks = licks & range_mask
    da = baselined_well[licks]
    if da.size == 0:
        return np.nan, np.nan, np.nan, np.nan
    return float(np.mean(da)), float(np.median(da)), float(np.min(da)), float(np.max(da))


def feeding_summary_one_well(
    *,
    dfm_id: int,
    raw: pd.DataFrame,
    baselined: pd.DataFrame,
    lick_df: pd.DataFrame,
    event_df: pd.DataFrame,
    durations: dict[str, pd.DataFrame | int],
    intervals: dict[str, pd.DataFrame | int],
    params: Parameters,
    range_minutes: Sequence[float] = (0, 0),
    transform_licks: bool = True,
) -> pd.DataFrame:
    if params.chamber_size != 1:
        raise ValueError("feeding_summary_one_well requires chamber_size==1")

    lights = get_lights_info(raw if not range_is_specified(range_minutes) else _apply_range_mask(raw, range_minutes))
    if "W1" in lights.columns:
        lights_sec = lights[[f"W{i}" for i in range(1, 13)]].sum(axis=0).to_numpy() / float(
            params.samples_per_second
        )
    else:
        lights_sec = np.zeros(12, dtype=float)

    rows: list[dict[str, object]] = []
    for well in range(1, 13):
        cname = f"W{well}"
        mean_dur, med_dur = _duration_summary(durations[cname], range_minutes)
        mean_intv, med_intv = _interval_summary(intervals[cname], range_minutes)

        base = baselined[cname].to_numpy(dtype=float)
        ev = event_df[cname].to_numpy(dtype=int)
        mask = None
        if range_is_specified(range_minutes):
            a, b = float(range_minutes[0]), float(range_minutes[1])
            mins = baselined["Minutes"].to_numpy(dtype=float)
            mask = (mins > a) & (mins <= b)
        mean_int, med_int, min_int, max_int = _intensity_summary(base, ev, range_mask=mask)

        licks = int(_apply_range_mask(lick_df, range_minutes)[cname].sum())
        events = int((_apply_range_mask(event_df, range_minutes)[cname].to_numpy(dtype=int) > 0).sum())

        rows.append(
            {
                "DFM": dfm_id,
                "Chamber": well,
                "Licks": float(licks),
                "Events": float(events),
                "MeanDuration": mean_dur,
                "MedDuration": med_dur,
                "MeanTimeBtw": mean_intv,
                "MedTimeBtw": med_intv,
                "MeanInt": mean_int,
                "MedianInt": med_int,
                "MinInt": min_int,
                "MaxInt": max_int,
                "OptoOn_sec": float(lights_sec[well - 1]) if len(lights_sec) == 12 else 0.0,
                "StartMin": float(range_minutes[0]),
                "EndMin": float(range_minutes[1]),
            }
        )

    out = pd.DataFrame(rows)
    if transform_licks:
        out["Licks"] = np.power(out["Licks"], 0.25)
    return out


def feeding_summary_two_well(
    *,
    dfm_id: int,
    raw: pd.DataFrame,
    baselined: pd.DataFrame,
    lick_df: pd.DataFrame,
    event_df: pd.DataFrame,
    durations: dict[str, pd.DataFrame | int],
    intervals: dict[str, pd.DataFrame | int],
    params: Parameters,
    range_minutes: Sequence[float] = (0, 0),
    transform_licks: bool = True,
) -> pd.DataFrame:
    if params.chamber_size != 2:
        raise ValueError("feeding_summary_two_well requires chamber_size==2")

    lights = get_lights_info(raw if not range_is_specified(range_minutes) else _apply_range_mask(raw, range_minutes))
    lights_sec = lights[[f"W{i}" for i in range(1, 13)]].sum(axis=0).to_numpy() / float(params.samples_per_second)

    rows: list[dict[str, object]] = []

    for chamber in range(params.chamber_sets.shape[0]):
        w1, w2 = int(params.chamber_sets[chamber, 0]), int(params.chamber_sets[chamber, 1])
        if params.pi_direction == "left":
            well_a, well_b = w1, w2
        elif params.pi_direction == "right":
            well_a, well_b = w2, w1
        else:
            raise ValueError(f"Invalid pi_direction: {params.pi_direction!r}")

        ca, cb = f"W{well_a}", f"W{well_b}"

        licks_a = float(_apply_range_mask(lick_df, range_minutes)[ca].sum())
        licks_b = float(_apply_range_mask(lick_df, range_minutes)[cb].sum())
        events_a = float((_apply_range_mask(event_df, range_minutes)[ca].to_numpy(dtype=int) > 0).sum())
        events_b = float((_apply_range_mask(event_df, range_minutes)[cb].to_numpy(dtype=int) > 0).sum())

        # Summaries per well
        mean_dur_a, med_dur_a = _duration_summary(durations[ca], range_minutes)
        mean_dur_b, med_dur_b = _duration_summary(durations[cb], range_minutes)
        mean_intv_a, med_intv_a = _interval_summary(intervals[ca], range_minutes)
        mean_intv_b, med_intv_b = _interval_summary(intervals[cb], range_minutes)

        base_a = baselined[ca].to_numpy(dtype=float)
        base_b = baselined[cb].to_numpy(dtype=float)
        ev_a = event_df[ca].to_numpy(dtype=int)
        ev_b = event_df[cb].to_numpy(dtype=int)

        mask = None
        if range_is_specified(range_minutes):
            a, b = float(range_minutes[0]), float(range_minutes[1])
            mins = baselined["Minutes"].to_numpy(dtype=float)
            mask = (mins > a) & (mins <= b)
        mean_int_a, med_int_a, min_int_a, max_int_a = _intensity_summary(base_a, ev_a, range_mask=mask)
        mean_int_b, med_int_b, min_int_b, max_int_b = _intensity_summary(base_b, ev_b, range_mask=mask)

        pi = (licks_a - licks_b) / (licks_a + licks_b) if (licks_a + licks_b) > 0 else np.nan
        event_pi = (events_a - events_b) / (events_a + events_b) if (events_a + events_b) > 0 else np.nan

        rows.append(
            {
                "DFM": dfm_id,
                "Chamber": chamber + 1,
                "PI": pi,
                "EventPI": event_pi,
                "LicksA": licks_a,
                "LicksB": licks_b,
                "EventsA": events_a,
                "EventsB": events_b,
                "MeanDurationA": mean_dur_a,
                "MedDurationA": med_dur_a,
                "MeanDurationB": mean_dur_b,
                "MedDurationB": med_dur_b,
                "MeanTimeBtwA": mean_intv_a,
                "MedTimeBtwA": med_intv_a,
                "MeanTimeBtwB": mean_intv_b,
                "MedTimeBtwB": med_intv_b,
                "MeanIntA": mean_int_a,
                "MedianIntA": med_int_a,
                "MinIntA": min_int_a,
                "MaxIntA": max_int_a,
                "MeanIntB": mean_int_b,
                "MedianIntB": med_int_b,
                "MinIntB": min_int_b,
                "MaxIntB": max_int_b,
                "OptoOn_sec_A": float(lights_sec[well_a - 1]),
                "OptoOn_sec_B": float(lights_sec[well_b - 1]),
                "StartMin": float(range_minutes[0]),
                "EndMin": float(range_minutes[1]),
            }
        )

    out = pd.DataFrame(rows)
    if transform_licks:
        out["LicksA"] = np.power(out["LicksA"], 0.25)
        out["LicksB"] = np.power(out["LicksB"], 0.25)
    return out


def binned_feeding_summary(
    dfm,
    *,
    binsize_min: float = 30.0,
    range_minutes: Sequence[float] = (0, 0),
    transform_licks: bool = True,
) -> pd.DataFrame:
    """
    Port of `BinnedFeeding.Summary.DFM()`.
    """

    if range_is_specified(range_minutes):
        m_min, m_max = float(range_minutes[0]), float(range_minutes[1])
    else:
        m_min, m_max = 0.0, float(dfm.raw_df["Minutes"].max())
    if m_min > m_max:
        m_max = m_min + 1.0

    y = np.arange(m_min, m_max + 1e-9, binsize_min, dtype=float)
    if y.size == 0 or y[-1] < m_max:
        y = np.append(y, m_max)

    # Build bin ranges as in R: cbind(y[-length(y)], y[-1])
    bins = np.column_stack([y[:-1], y[1:]])
    labels = [f"({a},{b}]" for a, b in bins]

    parts: list[pd.DataFrame] = []
    for (a, b), label in zip(bins, labels, strict=False):
        summ = dfm.feeding_summary(range_minutes=(float(a), float(b)), transform_licks=transform_licks)
        summ.insert(0, "Minutes", float((a + b) / 2.0))
        summ.insert(0, "Interval", label)
        parts.append(summ)
    return pd.concat(parts, ignore_index=True)


def interval_data_all_wells(dfm, *, range_minutes: Sequence[float] = (0, 0)) -> pd.DataFrame:
    """
    Port of `GetIntervalData.DFM()` (simplified but preserves key columns).
    """

    frames: list[pd.DataFrame] = []
    for well in range(1, 13):
        cname = f"W{well}"
        the_data = dfm.intervals.get(cname, 0)
        if not isinstance(the_data, pd.DataFrame) or the_data.empty:
            continue
        tmp = the_data.copy()
        if range_is_specified(range_minutes):
            tmp = _apply_range_mask(tmp, range_minutes)
        if tmp.empty:
            continue

        tmp.insert(0, "Well", well)
        tmp.insert(0, "TCWell", _tcwell_from_well(well, dfm.params) or "")
        tmp.insert(0, "Chamber", _chamber_from_well(well, dfm.params))
        tmp.insert(0, "DFM", dfm.id)
        frames.append(tmp)
    return pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()


def duration_data_all_wells(dfm, *, range_minutes: Sequence[float] = (0, 0)) -> pd.DataFrame:
    """
    Port of `GetDurationData.DFM()` (simplified but preserves key columns).
    """

    frames: list[pd.DataFrame] = []
    for well in range(1, 13):
        cname = f"W{well}"
        the_data = dfm.durations.get(cname, 0)
        if not isinstance(the_data, pd.DataFrame) or the_data.empty:
            continue
        tmp = the_data.copy()
        if range_is_specified(range_minutes):
            tmp = _apply_range_mask(tmp, range_minutes)
        if tmp.empty:
            continue

        tmp.insert(0, "Well", well)
        tmp.insert(0, "TCWell", _tcwell_from_well(well, dfm.params) or "")
        tmp.insert(0, "Chamber", _chamber_from_well(well, dfm.params))
        tmp.insert(0, "DFM", dfm.id)
        frames.append(tmp)
    return pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()

