from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Sequence

import numpy as np
import pandas as pd

from .utils import natural_sorted, range_is_specified


@dataclass(frozen=True, slots=True)
class LoadedDFM:
    df: pd.DataFrame
    version: int  # 2 or 3 (based on filename pattern)
    source_files: list[Path]


def _elapsed_seconds_from_date_time(df: pd.DataFrame) -> np.ndarray:
    # Port of `GetElapsedSeconds()` from `PrivateFunctions.R`.
    dates = df["Date"].astype(str)
    times = df["Time"].astype(str)
    ms = pd.to_numeric(df["MSec"], errors="coerce").fillna(0.0).to_numpy()

    # Detect AM/PM timestamps by searching for 'AM'/'PM' or a trailing 'M' as in R code.
    first = times.iloc[0]
    is_ampm = ("AM" in first) or ("PM" in first) or (first.upper().endswith("M"))
    if is_ampm:
        ts = pd.to_datetime(dates + " " + times, format="%m/%d/%Y %I:%M:%S %p", utc=True)
    else:
        ts = pd.to_datetime(dates + " " + times, format="%m/%d/%Y %H:%M:%S", utc=True)

    diffs = (ts - ts.iloc[0]).dt.total_seconds().to_numpy(dtype=float)
    return diffs + (ms / 1000.0)


def _ensure_minutes_seconds(df: pd.DataFrame) -> pd.DataFrame:
    if "Seconds" in df.columns:
        seconds = pd.to_numeric(df["Seconds"], errors="coerce").to_numpy(dtype=float)
        minutes = seconds / 60.0
        # Mirror R: prepend Minutes (and Seconds already exists).
        out = df.copy()
        out.insert(0, "Minutes", minutes)
        return out

    has_dt = all(c in df.columns for c in ("Date", "Time", "MSec"))
    if has_dt:
        seconds = _elapsed_seconds_from_date_time(df)
        minutes = seconds / 60.0
        out = df.copy()
        out.insert(0, "Seconds", seconds)
        out.insert(0, "Minutes", minutes)
        return out

    raise ValueError("Time information missing from DFM data (need Seconds or Date/Time/MSec).")


def load_dfm_csvs(dfm_id: int, data_dir: str | Path = ".", range_minutes: Sequence[float] = (0, 0)) -> LoadedDFM:
    """
    Implements `DFMClass()` selection logic from `DFM.R`.

    V3: files matching `DFM{id}_*.csv`
    V2: file `DFM_{id}.csv`
    V2 link-files: files matching `DFM_{id}_*.csv`
    """

    data_dir = Path(data_dir)
    if not data_dir.exists():
        raise FileNotFoundError(f"data_dir does not exist: {data_dir}")

    v3 = natural_sorted(data_dir.glob(f"DFM{dfm_id}_*.csv"))
    if v3:
        files = v3
        version = 3
    else:
        v2_single = data_dir / f"DFM_{dfm_id}.csv"
        if v2_single.exists():
            files = [v2_single]
            version = 2
        else:
            v2_link = natural_sorted(data_dir.glob(f"DFM_{dfm_id}_*.csv"))
            if v2_link:
                files = v2_link
                version = 2
            else:
                raise FileNotFoundError(f"DFM data file(s) not found for id={dfm_id} in {data_dir}")

    frames: list[pd.DataFrame] = []
    for f in files:
        frames.append(pd.read_csv(f))
    df = pd.concat(frames, ignore_index=True) if len(frames) > 1 else frames[0]

    df = _ensure_minutes_seconds(df)

    if range_is_specified(range_minutes):
        a, b = float(range_minutes[0]), float(range_minutes[1])
        df = df[(df["Minutes"] > a) & (df["Minutes"] < b)]

    return LoadedDFM(df=df.reset_index(drop=True), version=version, source_files=files)

