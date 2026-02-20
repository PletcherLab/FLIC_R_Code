from __future__ import annotations

import numpy as np
import pandas as pd


def _light_status_v3(opto_col1: int) -> np.ndarray:
    # Port of `LightStatus` for DFMVERSION >= 3 (uses only OptoCol1 bits 0..11).
    lights = np.zeros(12, dtype=bool)
    for i in range(12):
        if (opto_col1 & (1 << i)) > 0:
            lights[i] = True
    return lights


def get_lights_info(raw: pd.DataFrame) -> pd.DataFrame:
    """
    Port of `GetLightsInfo()` from `MiscFunctions.R` (DFM3 only).

    Returns DataFrame with columns:
    Minutes, W1..W12, OptoCol1, OptoCol2 (OptoCol2 may be absent in newer data)
    """

    if len(raw) < 1:
        cols = ["Minutes", *[f"W{i}" for i in range(1, 13)], "OptoCol1", "OptoCol2"]
        return pd.DataFrame([[np.nan] * len(cols)], columns=cols)

    if "OptoCol1" not in raw.columns:
        cols = ["Minutes", *[f"W{i}" for i in range(1, 13)], "OptoCol1", "OptoCol2"]
        out = pd.DataFrame(0, index=raw.index, columns=cols)
        out["Minutes"] = raw["Minutes"].to_numpy()
        out["OptoCol1"] = np.nan
        out["OptoCol2"] = np.nan
        return out

    opto1 = pd.to_numeric(raw["OptoCol1"], errors="coerce").fillna(0).astype(int).to_numpy()
    opto2 = (
        pd.to_numeric(raw["OptoCol2"], errors="coerce").fillna(0).astype(int).to_numpy()
        if "OptoCol2" in raw.columns
        else np.zeros(len(raw), dtype=int)
    )

    lights = np.vstack([_light_status_v3(v) for v in opto1])
    out = pd.DataFrame(lights, columns=[f"W{i}" for i in range(1, 13)])
    out.insert(0, "Minutes", raw["Minutes"].to_numpy())
    out["OptoCol1"] = opto1
    out["OptoCol2"] = opto2
    return out

