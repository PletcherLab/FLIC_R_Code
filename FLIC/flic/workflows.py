from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Sequence

import numpy as np
import pandas as pd

from .dfm import DFM
from .parameters import Parameters


def _as_params_list(monitors: Sequence[int], parameters: Parameters | Sequence[Parameters]) -> list[Parameters]:
    if isinstance(parameters, Parameters):
        return [parameters] * len(monitors)
    params_list = list(parameters)
    if len(params_list) != len(monitors):
        raise ValueError("If a list of parameter objects is specified, there must be one for each DFM.")
    return params_list


def append_treatment(results: pd.DataFrame, exp_design: pd.DataFrame) -> pd.DataFrame:
    """
    Port of `AppendTreatmentonResultsFrame()` using a merge on (DFM, Chamber).

    `exp_design` should contain columns: DFM, Chamber, Treatment.
    """

    if "Chamber" not in results.columns:
        raise ValueError("Need a Chamber column to assign treatment.")
    for col in ("DFM", "Chamber", "Treatment"):
        if col not in exp_design.columns:
            raise ValueError(f"exp_design missing required column: {col}")

    merged = results.merge(
        exp_design[["DFM", "Chamber", "Treatment"]],
        how="left",
        on=["DFM", "Chamber"],
    )
    merged["Treatment"] = merged["Treatment"].fillna("None")
    # Put Treatment first, like the R function.
    cols = ["Treatment", *[c for c in merged.columns if c != "Treatment"]]
    return merged[cols]


def _sem(x: pd.Series) -> float:
    x = pd.to_numeric(x, errors="coerce").dropna()
    if len(x) == 0:
        return np.nan
    if len(x) == 1:
        return 0.0
    return float(x.std(ddof=1) / np.sqrt(len(x)))


def aggregate_treatments(results: pd.DataFrame) -> pd.DataFrame:
    """
    Port of `AggregateTreatments()` (mean + SEM by Treatment).
    """

    if "Treatment" not in results.columns:
        raise ValueError("results must have a Treatment column")

    drop_cols = {"DFM", "Chamber"}
    numeric_cols = [c for c in results.columns if c not in ({"Treatment"} | drop_cols)]

    grouped = results.groupby("Treatment", dropna=False)
    mean_df = grouped[numeric_cols].mean(numeric_only=True)
    sem_df = grouped[numeric_cols].agg(_sem)

    out = mean_df.reset_index()
    for c in numeric_cols:
        out[f"{c}SEM"] = sem_df[c].to_numpy()
    return out


def aggregate_treatments_binned(results: pd.DataFrame) -> pd.DataFrame:
    """
    Port of `AggregateTreatmentsBinnedData()` (mean + SEM by Interval and Treatment).
    """

    if "Treatment" not in results.columns or "Interval" not in results.columns:
        raise ValueError("results must have Interval and Treatment columns")

    drop_cols = {"DFM", "Chamber"}
    numeric_cols = [c for c in results.columns if c not in ({"Treatment", "Interval"} | drop_cols)]

    grouped = results.groupby(["Interval", "Treatment"], dropna=False)
    mean_df = grouped[numeric_cols].mean(numeric_only=True).reset_index()
    sem_df = grouped[numeric_cols].agg(_sem).reset_index()

    out = mean_df.copy()
    for c in numeric_cols:
        out[f"{c}SEM"] = sem_df[c].to_numpy()
    return out


def feeding_summary_monitors(
    monitors: Sequence[int],
    parameters: Parameters | Sequence[Parameters],
    *,
    exp_design: pd.DataFrame | None = None,
    range_minutes: Sequence[float] = (0, 0),
    transform_licks: bool = True,
    save_to_file: bool = False,
    filename: str = "FeedingSummary",
    data_dir: str | Path = ".",
) -> dict[str, pd.DataFrame]:
    """
    Port of `Feeding.Summary.Monitors()`.
    """

    params_list = _as_params_list(monitors, parameters)
    frames: list[pd.DataFrame] = []
    for mid, p in zip(monitors, params_list, strict=False):
        dfm = DFM.load(int(mid), p, data_dir=data_dir, range_minutes=(0, 0))
        frames.append(dfm.feeding_summary(range_minutes=range_minutes, transform_licks=transform_licks))
    results = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()

    out: dict[str, pd.DataFrame] = {"Results": results}
    if exp_design is not None and isinstance(exp_design, pd.DataFrame):
        results_t = append_treatment(results, exp_design)
        out["Results"] = results_t
        out["Stats"] = aggregate_treatments(results_t)

    if save_to_file:
        base = Path(f"{filename}")
        if "Stats" in out:
            out["Stats"].to_csv(base.with_name(base.name + "_Stats.csv"), index=False)
            out["Results"].to_csv(base.with_name(base.name + "_Data.csv"), index=False)
        else:
            out["Results"].to_csv(base.with_suffix(".csv"), index=False)
    return out


def binned_feeding_summary_monitors(
    monitors: Sequence[int],
    parameters: Parameters | Sequence[Parameters],
    *,
    binsize_min: float = 30.0,
    exp_design: pd.DataFrame | None = None,
    range_minutes: Sequence[float] = (0, 0),
    transform_licks: bool = True,
    save_to_file: bool = False,
    filename: str = "BinnedSummary",
    data_dir: str | Path = ".",
) -> dict[str, pd.DataFrame]:
    """
    Port of `BinnedFeeding.Summary.Monitors()`.
    """

    params_list = _as_params_list(monitors, parameters)
    frames: list[pd.DataFrame] = []
    for mid, p in zip(monitors, params_list, strict=False):
        dfm = DFM.load(int(mid), p, data_dir=data_dir, range_minutes=(0, 0))
        frames.append(
            dfm.binned_feeding_summary(
                binsize_min=binsize_min, range_minutes=range_minutes, transform_licks=transform_licks
            )
        )
    results = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()

    out: dict[str, pd.DataFrame] = {"Results": results}
    if exp_design is not None and isinstance(exp_design, pd.DataFrame):
        results_t = append_treatment(results, exp_design)
        out["Results"] = results_t
        out["Stats"] = aggregate_treatments_binned(results_t)

    if save_to_file:
        base = Path(f"{filename}")
        if "Stats" in out:
            out["Stats"].to_csv(base.with_name(base.name + "_Stats.csv"), index=False)
            out["Results"].to_csv(base.with_name(base.name + "_Data.csv"), index=False)
        else:
            out["Results"].to_csv(base.with_suffix(".csv"), index=False)
    return out


def output_baselined_data_monitors(
    monitors: Sequence[int],
    parameters: Parameters | Sequence[Parameters],
    *,
    range_minutes: Sequence[float] = (0, 0),
    filename_prefix: str = "Baselined",
    data_dir: str | Path = ".",
) -> list[Path]:
    """
    Port of `OutputBaselinedData.Monitors()` / `OutputBaselinedData.DFM()` (Python-flavored).

    Writes one CSV per DFM with a name like: `{prefix}_DFM{id}.csv`.
    Returns the written paths.
    """

    params_list = _as_params_list(monitors, parameters)
    written: list[Path] = []
    for mid, p in zip(monitors, params_list, strict=False):
        dfm = DFM.load(int(mid), p, data_dir=data_dir, range_minutes=(0, 0))
        out_df = dfm.baselined(range_minutes=range_minutes)
        out_path = Path(f"{filename_prefix}_DFM{dfm.id}.csv")
        out_df.to_csv(out_path, index=False)
        written.append(out_path)
    return written


def output_interval_data_monitors(
    monitors: Sequence[int],
    parameters: Parameters | Sequence[Parameters],
    *,
    exp_design: pd.DataFrame | None = None,
    range_minutes: Sequence[float] = (0, 0),
    filename: str = "IntervalData",
    data_dir: str | Path = ".",
) -> Path:
    """
    Port of `OutputIntervalData.Monitors()` (writes a single combined CSV).
    """

    params_list = _as_params_list(monitors, parameters)
    frames: list[pd.DataFrame] = []
    for mid, p in zip(monitors, params_list, strict=False):
        dfm = DFM.load(int(mid), p, data_dir=data_dir, range_minutes=(0, 0))
        frames.append(dfm.interval_data(range_minutes=range_minutes))
    result = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()
    if exp_design is not None and not result.empty:
        result = append_treatment(result, exp_design)
    out_path = Path(f"{filename}.csv")
    result.to_csv(out_path, index=False)
    return out_path


def output_duration_data_monitors(
    monitors: Sequence[int],
    parameters: Parameters | Sequence[Parameters],
    *,
    exp_design: pd.DataFrame | None = None,
    range_minutes: Sequence[float] = (0, 0),
    filename: str = "DurationsData",
    data_dir: str | Path = ".",
) -> Path:
    """
    Port of `OutputDurationData.Monitors()` (writes a single combined CSV).
    """

    params_list = _as_params_list(monitors, parameters)
    frames: list[pd.DataFrame] = []
    for mid, p in zip(monitors, params_list, strict=False):
        dfm = DFM.load(int(mid), p, data_dir=data_dir, range_minutes=(0, 0))
        frames.append(dfm.duration_data(range_minutes=range_minutes))
    result = pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()
    if exp_design is not None and not result.empty:
        result = append_treatment(result, exp_design)
    out_path = Path(f"{filename}.csv")
    result.to_csv(out_path, index=False)
    return out_path

