from __future__ import annotations

from dataclasses import dataclass
import re
from typing import Sequence

import numpy as np
import pandas as pd

from .parameters import Parameters


def find_data_breaks(raw: pd.DataFrame, params: Parameters, multiplier: float = 4.0) -> pd.DataFrame | None:
    """
    Port of `FindDataBreaks()` from `QC.R`.

    Returns a DataFrame of breaks or None if none found.
    """

    if "Seconds" not in raw.columns:
        return None
    seconds = pd.to_numeric(raw["Seconds"], errors="coerce").to_numpy(dtype=float)
    interval = np.diff(seconds, prepend=seconds[0])
    thresh = (1.0 / float(params.samples_per_second)) * float(multiplier)
    idx = np.flatnonzero(interval > thresh)
    if idx.size == 0:
        return None
    out = raw.iloc[idx].copy()
    out.insert(0, "Interval", interval[idx])
    out.insert(0, "Index", idx + 1)
    return out.reset_index(drop=True)


def check_for_simultaneous_feeding(
    lick_df: pd.DataFrame, baselined: pd.DataFrame, params: Parameters
) -> np.ndarray:
    """
    Port of `CheckForSimultaneousFeeding.DFM()` from `QC.R`.

    Returns a 6x5 matrix for the 6 two-well chambers:
    [sum(l1), sum(l2), sum(both), max(min(sig1, sig2)) for both, count(sig1>sig2) among both]
    """

    if params.chamber_size != 2:
        raise ValueError("simultaneous feeding check is for chamber_size==2 only")

    n = int(params.chamber_sets.shape[0])
    mat = np.full((n, 5), np.nan, dtype=float)
    for i in range(n):
        w1, w2 = params.chamber_sets[i, 0], params.chamber_sets[i, 1]
        c1, c2 = f"W{w1}", f"W{w2}"
        l1 = lick_df[c1].to_numpy(dtype=bool)
        l2 = lick_df[c2].to_numpy(dtype=bool)
        both = l1 & l2
        if np.sum(both) > 0:
            sig1 = baselined[c1].to_numpy(dtype=float)
            sig2 = baselined[c2].to_numpy(dtype=float)
            higher_in_left = float(np.sum(sig1[both] > sig2[both]))
            max_min = float(np.max(np.minimum(sig1[both], sig2[both])))
            mat[i, :] = [float(np.sum(l1)), float(np.sum(l2)), float(np.sum(both)), max_min, higher_in_left]
        else:
            mat[i, :] = [float(np.sum(l1)), float(np.sum(l2)), 0.0, 0.0, 0.0]
    return mat


def adjust_baseline_for_dual_feeding(
    lick_df: pd.DataFrame, baselined: pd.DataFrame, params: Parameters, dfm_id: int | None = None
) -> dict[str, object]:
    """
    Port of `Adjust.Baseline.For.Dual.Feeding.DFM()` from `PrivateFunctions.R`.

    Returns dict with keys:
    - baselined: adjusted baselined DataFrame (W columns possibly zeroed for dual-feeding samples)
    - dual_feeding_data: DataFrame of rows where both wells were feeding, with a Chamber column
    """

    if params.chamber_size != 2:
        raise ValueError("dual-feeding adjustment is for chamber_size==2 only")

    wcols = [f"W{i}" for i in range(1, 13)]
    rd = lick_df[wcols].to_numpy(dtype=bool)
    rd2 = baselined[wcols].to_numpy(dtype=float).copy()

    dual_rows: list[pd.DataFrame] = []
    n = int(params.chamber_sets.shape[0])
    for i in range(n):
        w1, w2 = params.chamber_sets[i, 0], params.chamber_sets[i, 1]
        l1 = rd[:, w1 - 1]
        l2 = rd[:, w2 - 1]
        both = l1 & l2
        if not np.any(both):
            continue

        sig1 = rd2[:, w1 - 1]
        sig2 = rd2[:, w2 - 1]

        smaller_1 = both & (sig1 < sig2)
        smaller_2 = both & (sig2 < sig1)
        same = both & (sig2 == sig1)

        rd2[smaller_1, w1 - 1] = 0.0
        rd2[smaller_2, w2 - 1] = 0.0
        rd2[same, w1 - 1] = 0.0
        rd2[same, w2 - 1] = 0.0

        tmp = baselined.loc[both].copy()
        tmp.insert(0, "Chamber", i + 1)
        dual_rows.append(tmp)

    dual_feeding_data = pd.concat(dual_rows, ignore_index=True) if dual_rows else pd.DataFrame()
    adjusted = baselined.copy()
    adjusted[wcols] = rd2
    return {"baselined": adjusted, "dual_feeding_data": dual_feeding_data}


def check_for_bleeding(baselined: pd.DataFrame, cutoff: float) -> dict[str, object]:
    """
    Port of `CheckForBleeding.DFM()` from `QC.R` (data return only; no contour plot).

    For each signal well i, take all rows where W_i > cutoff and compute the mean response
    across all wells. Returns:
    - Matrix: DataFrame indexed by W?Sig and columns W?Resp
    - AllData: Series of overall mean per well across all rows
    """

    wcols = [f"W{i}" for i in range(1, 13)]
    missing = [c for c in wcols if c not in baselined.columns]
    if missing:
        raise ValueError(f"baselined missing required well columns: {missing}")

    rd = baselined[wcols].apply(pd.to_numeric, errors="coerce")
    mat = np.full((12, 12), np.nan, dtype=float)
    for i in range(12):
        tmp = rd.loc[rd.iloc[:, i] > float(cutoff)]
        if len(tmp) == 0:
            mat[i, :] = 0.0
        else:
            mat[i, :] = tmp.mean(axis=0, skipna=True).to_numpy(dtype=float)

    mat = np.nan_to_num(mat, nan=0.0)
    matrix_df = pd.DataFrame(
        mat,
        index=[f"W{i}Sig" for i in range(1, 13)],
        columns=[f"W{i}Resp" for i in range(1, 13)],
    )
    all_data = rd.mean(axis=0, skipna=True)
    all_data.index = wcols

    return {"Matrix": matrix_df, "AllData": all_data}


_ERROR_BIT_NAMES = [
    "I2C Error",
    "ID Error",
    "PacketType Error",
    "DMA_TX Error",
    "DMA_RX Error",
    "PacketSize Error",
    "AIInterrupt Error",
    "OInterrupt Error",
]


def report_dfm_integrity(dfm) -> dict[str, object]:
    """
    Port of `ReportDFMIntegrity()` from `QC.R`.

    Prints an integrity report and returns a dict of the main findings.
    """

    if dfm is None:
        raise ValueError("dfm must be a DFM object")
    raw = getattr(dfm, "raw_df", None)
    if raw is None or not isinstance(raw, pd.DataFrame):
        raise ValueError("DFM object does not contain a raw_df DataFrame")

    n = len(raw)
    print("\n==============================")
    print("DFM Integrity Report")
    print(f"DFM ID: {getattr(dfm, 'id', None)}")
    print(f"Rows in RawData: {n}")
    print("==============================\n")

    # ---- Start/end time + elapsed minutes ----
    start_time = None
    end_time = None
    elapsed_minutes = None
    elapsed_minutes_from_minutes_col = None

    if all(c in raw.columns for c in ("Date", "Time")) and n > 0:
        tmp = str(raw["Time"].iloc[0])
        is_ampm = ("AM" in tmp.upper()) or ("PM" in tmp.upper()) or (re.search(r".M$", tmp) is not None)
        if is_ampm:
            ts = pd.to_datetime(
                raw["Date"].astype(str) + " " + raw["Time"].astype(str),
                format="%m/%d/%Y %I:%M:%S %p",
                utc=True,
                errors="coerce",
            )
        else:
            ts = pd.to_datetime(
                raw["Date"].astype(str) + " " + raw["Time"].astype(str),
                format="%m/%d/%Y %H:%M:%S",
                utc=True,
                errors="coerce",
            )
        if "MSec" in raw.columns:
            ms = pd.to_numeric(raw["MSec"], errors="coerce").fillna(0.0) / 1000.0
            ts = ts + pd.to_timedelta(ms, unit="s")
        if ts.notna().any():
            start_time = ts.iloc[0]
            end_time = ts.iloc[-1]
            elapsed_minutes = float((end_time - start_time).total_seconds() / 60.0)

    if "Minutes" in raw.columns and n > 1:
        mins = pd.to_numeric(raw["Minutes"], errors="coerce")
        if mins.notna().any():
            elapsed_minutes_from_minutes_col = float(mins.max() - mins.min())

    print("## Experiment timing")
    if start_time is not None and end_time is not None and pd.notna(start_time) and pd.notna(end_time):
        print(f"Start time: {start_time}")
        print(f"End time:   {end_time}")
        print(f"Elapsed:    {elapsed_minutes:.3f} minutes")
    else:
        print("Start/End time: (Date/Time/MSec not found or not parseable)")
    if elapsed_minutes_from_minutes_col is not None:
        print(f"Elapsed (from Minutes column): {elapsed_minutes_from_minutes_col:.3f} minutes")
    print()

    # ---- Error column(s) ----
    err_cols = [c for c in raw.columns if re.search("error", c, flags=re.IGNORECASE)]
    print("## Error column details")
    if not err_cols:
        print("No column with name matching /error/i found in RawData.\n")
    else:
        print(f"Found error-like column(s): {', '.join(err_cols)}\n")
        for ec in err_cols:
            v = raw[ec]
            flagged = None
            if pd.api.types.is_numeric_dtype(v) or pd.api.types.is_bool_dtype(v):
                vv = pd.to_numeric(v, errors="coerce")
                flagged = vv.notna() & (vv != 0)
            else:
                vs = v.astype(str).fillna("")
                flagged = (vs != "") & (vs != "0") & (vs.str.lower() != "na")

            print(f"Column: {ec}")
            print(f"  Non-NA: {int(v.notna().sum())}  NA: {int(v.isna().sum())}")
            print(f"  Flagged entries: {int(flagged.sum())}")

            if int(flagged.sum()) > 0:
                codes = pd.to_numeric(v, errors="coerce").astype("Int64")
                codes = codes[flagged].dropna().astype(int)
                if len(codes) > 0:
                    bit_mat = np.vstack(
                        [((codes.to_numpy() & (1 << i)) > 0) for i in range(8)]
                    ).T
                    type_counts = bit_mat.sum(axis=0)
                    print("  Error-type counts (among flagged rows):")
                    for name, cnt in zip(_ERROR_BIT_NAMES, type_counts, strict=False):
                        print(f"    {name}: {int(cnt)}")
            print()

    # ---- Index increment check ----
    print("## Index increment check")
    if "Index" not in raw.columns:
        print("No `Index` column found in RawData.")
        print("Skipping increment-by-one analysis.\n")
        index_ok = None
    else:
        ix = pd.to_numeric(raw["Index"], errors="coerce")
        if ix.notna().sum() < 2:
            print("Index column has <2 valid entries; cannot evaluate increments.\n")
            index_ok = None
        else:
            d = np.diff(ix.to_numpy(dtype=float))
            index_ok = bool(np.all(d == 1))
            if index_ok:
                print("Index increments by exactly 1 for all consecutive rows.\n")
            else:
                print("Index does NOT always increment by 1.\n")

    return {
        "dfm_id": getattr(dfm, "id", None),
        "n_rawdata": n,
        "start_time": start_time,
        "end_time": end_time,
        "elapsed_minutes": elapsed_minutes,
        "elapsed_minutes_from_minutes_col": elapsed_minutes_from_minutes_col,
        "error_columns": err_cols,
        "index_increments_by_one": index_ok,
    }

