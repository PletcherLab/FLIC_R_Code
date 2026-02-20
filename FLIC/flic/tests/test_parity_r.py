from __future__ import annotations

import subprocess
from pathlib import Path

import numpy as np
import pandas as pd

from flic import DFM, Parameters


def _repo_root() -> Path:
    # tests/ is at <repo>/tests
    return Path(__file__).resolve().parents[1]


def test_r_parity_feeding_summary_dfm5(tmp_path: Path):
    repo = _repo_root()

    # Generate golden using the R implementation.
    subprocess.run(
        ["Rscript", "scripts/generate_golden.R", "5", str(tmp_path)],
        cwd=str(repo),
        check=True,
        capture_output=True,
        text=True,
    )

    r_path = tmp_path / "feeding_summary_DFM5.csv"
    r_df = pd.read_csv(r_path)

    # Python output
    dfm = DFM.load(5, Parameters.two_well(), data_dir=str(repo), range_minutes=(0, 0))
    py_df = dfm.feeding_summary(transform_licks=True)

    # Align columns
    common = [c for c in r_df.columns if c in py_df.columns]
    assert common, "No common columns between R and Python summaries"
    r = r_df[common].copy()
    p = py_df[common].copy()

    # Sort deterministically
    if "Chamber" in common:
        r = r.sort_values(["Chamber"]).reset_index(drop=True)
        p = p.sort_values(["Chamber"]).reset_index(drop=True)

    # Compare numeric columns with tolerances.
    num_cols = [c for c in common if pd.api.types.is_numeric_dtype(r[c])]
    for c in num_cols:
        rv = r[c].to_numpy(dtype=float)
        pv = p[c].to_numpy(dtype=float)
        assert rv.shape == pv.shape
        # Allow small numerical differences due to median/filter implementations.
        ok = np.allclose(rv, pv, rtol=1e-6, atol=1e-6, equal_nan=True)
        assert ok, f"Mismatch in column {c}"


def test_r_parity_binned_feeding_summary_dfm5(tmp_path: Path):
    repo = _repo_root()
    subprocess.run(
        ["Rscript", "scripts/generate_golden.R", "5", str(tmp_path)],
        cwd=str(repo),
        check=True,
        capture_output=True,
        text=True,
    )

    r_path = tmp_path / "binned_feeding_summary_60min_DFM5.csv"
    r_df = pd.read_csv(r_path)

    dfm = DFM.load(5, Parameters.two_well(), data_dir=str(repo), range_minutes=(0, 0))
    py_df = dfm.binned_feeding_summary(binsize_min=60, transform_licks=True)

    # Drop Interval string formatting differences; compare numeric columns + Chamber.
    drop = [c for c in ["Interval"] if c in r_df.columns and c in py_df.columns]
    r_df = r_df.drop(columns=drop)
    py_df = py_df.drop(columns=drop)

    common = [c for c in r_df.columns if c in py_df.columns]
    r = r_df[common].copy()
    p = py_df[common].copy()

    sort_cols = [c for c in ["Minutes", "Chamber"] if c in common]
    if sort_cols:
        r = r.sort_values(sort_cols).reset_index(drop=True)
        p = p.sort_values(sort_cols).reset_index(drop=True)

    num_cols = [c for c in common if pd.api.types.is_numeric_dtype(r[c])]
    for c in num_cols:
        rv = r[c].to_numpy(dtype=float)
        pv = p[c].to_numpy(dtype=float)
        assert rv.shape == pv.shape
        ok = np.allclose(rv, pv, rtol=1e-6, atol=1e-6, equal_nan=True)
        assert ok, f"Mismatch in column {c}"

