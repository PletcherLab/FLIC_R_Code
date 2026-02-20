from __future__ import annotations

from typing import Sequence

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from .utils import range_is_specified


def _apply_range(df: pd.DataFrame, range_minutes: Sequence[float]) -> pd.DataFrame:
    if not range_is_specified(range_minutes):
        return df
    a, b = float(range_minutes[0]), float(range_minutes[1])
    return df[(df["Minutes"] > a) & (df["Minutes"] <= b)]


def plot_raw(dfm, *, range_minutes: Sequence[float] = (0, 0)):
    df = _apply_range(dfm.raw_df, range_minutes)
    wcols = [f"W{i}" for i in range(1, 13) if f"W{i}" in df.columns]
    n = len(wcols)
    fig, axes = plt.subplots(n, 1, sharex=True, figsize=(10, max(6, n * 1.3)))
    if n == 1:
        axes = [axes]
    for ax, col in zip(axes, wcols, strict=False):
        ax.plot(df["Minutes"], df[col], linewidth=0.8)
        ax.set_ylabel(col)
    axes[-1].set_xlabel("Minutes")
    fig.suptitle(f"DFM {dfm.id} Raw")
    fig.tight_layout()
    return fig


def plot_baselined(dfm, *, range_minutes: Sequence[float] = (0, 0), include_thresholds: bool = False):
    df = _apply_range(dfm.baseline_df, range_minutes)
    wcols = [f"W{i}" for i in range(1, 13) if f"W{i}" in df.columns]
    n = len(wcols)
    fig, axes = plt.subplots(n, 1, sharex=True, figsize=(10, max(6, n * 1.3)))
    if n == 1:
        axes = [axes]
    for ax, col in zip(axes, wcols, strict=False):
        ax.plot(df["Minutes"], df[col], linewidth=0.8)
        if include_thresholds:
            thr = dfm.thresholds[col]
            ax.axhline(float(thr["FeedingMax"].iloc[0]), linestyle="--", color="red", linewidth=0.6)
            ax.axhline(float(thr["FeedingMin"].iloc[0]), linestyle="--", color="red", linewidth=0.6)
        ax.set_ylabel(col)
    axes[-1].set_xlabel("Minutes")
    fig.suptitle(f"DFM {dfm.id} Baselined")
    fig.tight_layout()
    return fig


def plot_binned_licks(dfm, *, binsize_min: float = 30.0, range_minutes: Sequence[float] = (0, 0), transform_licks: bool = True):
    binned = dfm.binned_feeding_summary(binsize_min=binsize_min, range_minutes=range_minutes, transform_licks=transform_licks)
    if binned.empty:
        fig, ax = plt.subplots(figsize=(8, 3))
        ax.set_title(f"DFM {dfm.id} (no data)")
        return fig

    fig, ax = plt.subplots(figsize=(10, 4))
    if "LicksA" in binned.columns:
        # Two-well: plot total per chamber (A+B) as bars by bin.
        binned = binned.copy()
        binned["LicksTotal"] = binned["LicksA"] + binned["LicksB"]
        for chamber, grp in binned.groupby("Chamber"):
            ax.plot(grp["Minutes"], grp["LicksTotal"], marker="o", linewidth=1.0, label=f"Ch{int(chamber)}")
        ax.set_ylabel("Transformed Licks" if transform_licks else "Licks")
        ax.legend(title="Chamber", ncol=3, fontsize=8)
    else:
        for chamber, grp in binned.groupby("Chamber"):
            ax.plot(grp["Minutes"], grp["Licks"], marker="o", linewidth=1.0, label=f"Ch{int(chamber)}")
        ax.set_ylabel("Transformed Licks" if transform_licks else "Licks")
        ax.legend(title="Chamber", ncol=3, fontsize=8)

    ax.set_xlabel("Minutes")
    ax.set_title(f"DFM {dfm.id} binned feeding")
    fig.tight_layout()
    return fig


def cumulative_pi(dfm, *, range_minutes: Sequence[float] = (0, 0)) -> pd.DataFrame:
    """
    Port of `CumulativePI.DFM` (data part).
    """

    if dfm.params.chamber_size != 2:
        raise ValueError("cumulative_pi is only defined for two-well chambers")

    licks = _apply_range(dfm.lick_df, range_minutes)
    minutes = _apply_range(dfm.baseline_df, range_minutes)["Minutes"].to_numpy(dtype=float)

    parts: list[pd.DataFrame] = []
    chambers = getattr(dfm, "chambers", None)
    if chambers is None:
        chambers = []
        for chamber in range(dfm.params.chamber_sets.shape[0]):
            w1, w2 = dfm.params.chamber_sets[chamber, 0], dfm.params.chamber_sets[chamber, 1]
            chambers.append(type("Tmp", (), {"index": chamber + 1, "wells": (int(w1), int(w2))})())

    for ch in chambers:
        if hasattr(ch, "well_a") and hasattr(ch, "well_b"):
            well_a, well_b = int(ch.well_a), int(ch.well_b)
        else:
            w1, w2 = ch.wells
            if dfm.params.pi_direction == "left":
                well_a, well_b = int(w1), int(w2)
            elif dfm.params.pi_direction == "right":
                well_a, well_b = int(w2), int(w1)
            else:
                raise ValueError(f"Invalid pi_direction: {dfm.params.pi_direction!r}")

        a = licks[f"W{well_a}"].to_numpy(dtype=bool)
        b = licks[f"W{well_b}"].to_numpy(dtype=bool)
        mask = a | b
        if not np.any(mask):
            continue

        ca = np.cumsum(a.astype(int))
        cb = np.cumsum(b.astype(int))
        denom = ca + cb
        pi = np.full(denom.shape, np.nan, dtype=float)
        nz = denom > 0
        pi[nz] = (ca[nz] - cb[nz]) / denom[nz]
        out = pd.DataFrame(
            {
                "Minutes": minutes[mask],
                "PI": pi[mask],
                "Licks": denom[mask].astype(int),
                "Chamber": int(getattr(ch, "index", 1)),
            }
        )
        parts.append(out)

    return pd.concat(parts, ignore_index=True) if parts else pd.DataFrame(columns=["Minutes", "PI", "Licks", "Chamber"])


def cumulative_event_pi(
    dfm, *, events_limit: int | None = None, range_minutes: Sequence[float] = (0, 0)
) -> pd.DataFrame:
    """
    Port of `CumulativeEventPI.DFM` (data part).
    """

    if dfm.params.chamber_size != 2:
        raise ValueError("cumulative_event_pi is only defined for two-well chambers")

    events = _apply_range(dfm.event_df, range_minutes)
    minutes = _apply_range(dfm.baseline_df, range_minutes)["Minutes"].to_numpy(dtype=float)

    parts: list[pd.DataFrame] = []
    chambers = getattr(dfm, "chambers", None)
    if chambers is None:
        chambers = []
        for chamber in range(dfm.params.chamber_sets.shape[0]):
            w1, w2 = dfm.params.chamber_sets[chamber, 0], dfm.params.chamber_sets[chamber, 1]
            chambers.append(type("Tmp", (), {"index": chamber + 1, "wells": (int(w1), int(w2))})())

    for ch in chambers:
        if hasattr(ch, "well_a") and hasattr(ch, "well_b"):
            well_a, well_b = int(ch.well_a), int(ch.well_b)
        else:
            w1, w2 = ch.wells
            if dfm.params.pi_direction == "left":
                well_a, well_b = int(w1), int(w2)
            elif dfm.params.pi_direction == "right":
                well_a, well_b = int(w2), int(w1)
            else:
                raise ValueError(f"Invalid pi_direction: {dfm.params.pi_direction!r}")

        a = (events[f"W{well_a}"].to_numpy(dtype=int) > 0).astype(int)
        b = (events[f"W{well_b}"].to_numpy(dtype=int) > 0).astype(int)
        mask = (a + b) > 0
        if not np.any(mask):
            continue

        ca = np.cumsum(a)
        cb = np.cumsum(b)
        denom = ca + cb
        pi = np.full(denom.shape, np.nan, dtype=float)
        nz = denom > 0
        pi[nz] = (ca[nz] - cb[nz]) / denom[nz]

        out = pd.DataFrame(
            {
                "Minutes": minutes[mask],
                "PI": pi[mask],
                "Chamber": int(getattr(ch, "index", 1)),
            }
        )
        out["EventNum"] = np.arange(1, len(out) + 1, dtype=int)
        if events_limit is not None and len(out) > int(events_limit):
            out = out.iloc[: int(events_limit)].reset_index(drop=True)
        parts.append(out)

    cols = ["Minutes", "PI", "Chamber", "EventNum"]
    return pd.concat(parts, ignore_index=True)[cols] if parts else pd.DataFrame(columns=cols)


def plot_cumulative_pi(dfm, *, range_minutes: Sequence[float] = (0, 0), single_plot: bool = False):
    data = cumulative_pi(dfm, range_minutes=range_minutes)
    if data.empty:
        fig, ax = plt.subplots(figsize=(8, 3))
        ax.set_title(f"DFM {dfm.id} cumulative PI (no data)")
        return fig

    chambers = sorted(data["Chamber"].unique().tolist())
    if single_plot:
        fig, ax = plt.subplots(figsize=(10, 4))
        for ch in chambers:
            tmp = data[data["Chamber"] == ch]
            ax.plot(tmp["Minutes"], tmp["PI"], marker=".", linewidth=1.0, label=f"Ch{int(ch)}")
        ax.set_ylim(-1, 1)
        ax.set_xlabel("Minutes")
        ax.set_ylabel("PI (Licks)")
        ax.legend(ncol=3, fontsize=8)
        ax.set_title(f"DFM {dfm.id} cumulative PI")
        fig.tight_layout()
        return fig

    fig, axes = plt.subplots(len(chambers), 1, sharex=True, figsize=(10, max(4, len(chambers) * 1.6)))
    if len(chambers) == 1:
        axes = [axes]
    for ax, ch in zip(axes, chambers, strict=False):
        tmp = data[data["Chamber"] == ch]
        ax.plot(tmp["Minutes"], tmp["PI"], marker=".", linewidth=1.0)
        ax.set_ylim(-1, 1)
        ax.set_ylabel(f"Ch{int(ch)}")
    axes[-1].set_xlabel("Minutes")
    fig.suptitle(f"DFM {dfm.id} cumulative PI (Licks)")
    fig.tight_layout()
    return fig


def plot_cumulative_event_pi(
    dfm,
    *,
    events_limit: int | None = None,
    range_minutes: Sequence[float] = (0, 0),
    single_plot: bool = False,
    by_bout: bool = False,
):
    data = cumulative_event_pi(dfm, events_limit=events_limit, range_minutes=range_minutes)
    if data.empty:
        fig, ax = plt.subplots(figsize=(8, 3))
        ax.set_title(f"DFM {dfm.id} cumulative event PI (no data)")
        return fig

    chambers = sorted(data["Chamber"].unique().tolist())
    xcol = "EventNum" if by_bout else "Minutes"
    xlabel = "Event Number" if by_bout else "Minutes"

    if single_plot:
        fig, ax = plt.subplots(figsize=(10, 4))
        for ch in chambers:
            tmp = data[data["Chamber"] == ch]
            ax.plot(tmp[xcol], tmp["PI"], marker=".", linewidth=1.0, label=f"Ch{int(ch)}")
        ax.set_ylim(-1, 1)
        ax.set_xlabel(xlabel)
        ax.set_ylabel("PI (Events)")
        ax.legend(ncol=3, fontsize=8)
        ax.set_title(f"DFM {dfm.id} cumulative EventPI")
        fig.tight_layout()
        return fig

    fig, axes = plt.subplots(len(chambers), 1, sharex=True, figsize=(10, max(4, len(chambers) * 1.6)))
    if len(chambers) == 1:
        axes = [axes]
    for ax, ch in zip(axes, chambers, strict=False):
        tmp = data[data["Chamber"] == ch]
        ax.plot(tmp[xcol], tmp["PI"], marker=".", linewidth=1.0)
        ax.set_ylim(-1, 1)
        ax.set_ylabel(f"Ch{int(ch)}")
    axes[-1].set_xlabel(xlabel)
    fig.suptitle(f"DFM {dfm.id} cumulative EventPI")
    fig.tight_layout()
    return fig


def plot_cumulative_licks(dfm, *, single_plot: bool = False, transform_licks: bool = True):
    """
    Port of `CumulativeLicksPlots.DFM` (matplotlib version).
    """

    # Construct per-well bout table from dfm.durations; if no bouts, show a flat line.
    wcols = [f"W{i}" for i in range(1, 13)]
    first_min = float(dfm.baseline_df["Minutes"].iloc[0]) if len(dfm.baseline_df) else 0.0
    last_min = float(dfm.baseline_df["Minutes"].iloc[-1]) if len(dfm.baseline_df) else 0.0

    def cumulative_for_well(well: int) -> tuple[np.ndarray, np.ndarray]:
        d = dfm.durations.get(f"W{well}", 0)
        if not isinstance(d, pd.DataFrame) or d.empty:
            x = np.array([first_min, last_min], dtype=float)
            y = np.array([0.0, 0.0], dtype=float)
        else:
            x = d["Minutes"].to_numpy(dtype=float)
            y = np.cumsum(d["Licks"].to_numpy(dtype=float))
        if transform_licks:
            y = np.power(y, 0.25)
        return x, y

    if single_plot:
        fig, ax = plt.subplots(figsize=(10, 5))
        for well in range(1, 13):
            x, y = cumulative_for_well(well)
            ax.plot(x, y, linewidth=1.0, label=f"W{well}")
        ax.set_xlabel("Minutes")
        ax.set_ylabel("Transformed Cumulative Licks" if transform_licks else "Cumulative Licks")
        ax.set_title(f"DFM {dfm.id} cumulative licks")
        ax.legend(ncol=6, fontsize=7)
        fig.tight_layout()
        return fig

    if dfm.params.chamber_size == 1:
        fig, axes = plt.subplots(6, 2, sharex=True, figsize=(12, 10))
        for well in range(1, 13):
            r = (well - 1) // 2
            c = (well - 1) % 2
            ax = axes[r][c]
            x, y = cumulative_for_well(well)
            ax.plot(x, y, linewidth=1.0)
            ax.set_title(f"W{well}", fontsize=9)
        for ax in axes[-1]:
            ax.set_xlabel("Minutes")
        fig.suptitle(f"DFM {dfm.id} cumulative licks")
        fig.tight_layout()
        return fig

    chambers = getattr(dfm, "chambers", None)
    if not chambers:
        chambers = []
        for i in range(dfm.params.chamber_sets.shape[0]):
            w1, w2 = dfm.params.chamber_sets[i, 0], dfm.params.chamber_sets[i, 1]
            chambers.append(type("Tmp", (), {"index": i + 1, "wells": (int(w1), int(w2))})())

    fig, axes = plt.subplots(len(chambers), 2, sharex=True, figsize=(12, max(8, len(chambers) * 1.4)))
    if len(chambers) == 1:
        axes = [axes]
    for row_idx, ch in enumerate(chambers):
        if hasattr(ch, "well_a") and hasattr(ch, "well_b"):
            well_a, well_b = int(ch.well_a), int(ch.well_b)
        else:
            w1, w2 = ch.wells
            if dfm.params.pi_direction == "left":
                well_a, well_b = int(w1), int(w2)
            elif dfm.params.pi_direction == "right":
                well_a, well_b = int(w2), int(w1)
            else:
                raise ValueError(f"Invalid pi_direction: {dfm.params.pi_direction!r}")
        for col_idx, well in enumerate([well_a, well_b]):
            ax = axes[row_idx][col_idx]
            x, y = cumulative_for_well(well)
            ax.plot(x, y, linewidth=1.0)
            ax.set_title(
                f"Ch{int(getattr(ch, 'index', row_idx+1))} {'WellA' if col_idx==0 else 'WellB'} (W{well})",
                fontsize=9,
            )
    for ax in axes[-1]:
        ax.set_xlabel("Minutes")
    fig.suptitle(f"DFM {dfm.id} cumulative licks")
    fig.tight_layout()
    return fig


def plot_raw_well(dfm, well: int, *, range_minutes: Sequence[float] = (0, 0)):
    """
    Plot the raw signal for a single well.

    Single-well counterpart to R's `RawDataPlot.DFM`.
    Use this instead of `plot_raw` when you want to inspect one well closely —
    per-DFM plots compress thousands of points into a small panel, making
    individual features hard to see.

    Parameters
    ----------
    dfm : DFM
        Loaded DFM object.
    well : int
        1-based well number (1–12).
    range_minutes : (float, float)
        Restrict to this time window; (0, 0) means use all data.
    """
    col = f"W{well}"
    df = _apply_range(dfm.raw_df, range_minutes)
    if col not in df.columns:
        raise ValueError(
            f"Well {well} not found in DFM {dfm.id} raw data (expected column '{col}')."
        )

    fig, ax = plt.subplots(figsize=(10, 3))
    ax.plot(df["Minutes"], df[col], linewidth=0.7, color="steelblue")
    ax.set_xlabel("Minutes")
    ax.set_ylabel("Signal")
    ax.set_title(f"DFM {dfm.id}  W{well}  Raw")
    fig.tight_layout()
    return fig


def plot_baselined_well(
    dfm,
    well: int,
    *,
    range_minutes: Sequence[float] = (0, 0),
    include_thresholds: bool = False,
):
    """
    Plot the baseline-subtracted signal for a single well.

    Single-well counterpart to R's `BaselinedDataPlot.DFM`.

    Parameters
    ----------
    dfm : DFM
        Loaded DFM object.
    well : int
        1-based well number (1–12).
    range_minutes : (float, float)
        Restrict to this time window; (0, 0) means use all data.
    include_thresholds : bool
        If True, overlay FeedingMax and FeedingMin as dashed red lines,
        matching R's ``IncludeThresholds=TRUE`` option.
    """
    col = f"W{well}"
    df = _apply_range(dfm.baseline_df, range_minutes)
    if col not in df.columns:
        raise ValueError(
            f"Well {well} not found in DFM {dfm.id} baselined data (expected column '{col}')."
        )

    fig, ax = plt.subplots(figsize=(10, 3))
    ax.plot(df["Minutes"], df[col], linewidth=0.7, color="steelblue")

    if include_thresholds and hasattr(dfm, "thresholds") and col in dfm.thresholds:
        thr = dfm.thresholds[col]
        feed_max = float(thr["FeedingMax"].iloc[0])
        feed_min = float(thr["FeedingMin"].iloc[0])
        ax.axhline(
            feed_max,
            linestyle="--",
            color="red",
            linewidth=0.8,
            label=f"FeedingMax ({feed_max:.0f})",
        )
        ax.axhline(
            feed_min,
            linestyle="--",
            color="red",
            linewidth=0.8,
            label=f"FeedingMin ({feed_min:.0f})",
        )
        ax.legend(fontsize=8, loc="upper right")

    ax.set_xlabel("Minutes")
    ax.set_ylabel("Baselined Signal")
    ax.set_title(f"DFM {dfm.id}  W{well}  Baselined")
    fig.tight_layout()
    return fig

