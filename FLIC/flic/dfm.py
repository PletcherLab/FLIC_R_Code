from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Sequence

import numpy as np
import pandas as pd

from .algorithms.baseline import baseline_subtract
from .algorithms.feeding import bout_durations_and_intervals
from .algorithms.tasting import tasting_bout_durations_and_intervals
from .algorithms.thresholds import build_thresholds_table
from .chamber import OneWellChamber, TwoWellChamber, compute_feeding_for_well, compute_tasting_for_well
from .io import load_dfm_csvs
from .parameters import Parameters
from .qc import (
    adjust_baseline_for_dual_feeding,
    check_for_bleeding,
    check_for_simultaneous_feeding,
    find_data_breaks,
    report_dfm_integrity,
)
from .stats import (
    binned_feeding_summary,
    duration_data_all_wells,
    feeding_summary_one_well,
    feeding_summary_two_well,
    interval_data_all_wells,
)
from .utils import range_is_specified


@dataclass(slots=True)
class DFM:
    id: int
    params: Parameters
    raw_df: pd.DataFrame
    version: int = 2
    source_files: list[Path] | None = None

    # Computed
    baseline_df: pd.DataFrame | None = None
    thresholds: dict[str, pd.DataFrame] | None = None
    lick_df: pd.DataFrame | None = None
    event_df: pd.DataFrame | None = None
    tasting_df: pd.DataFrame | None = None
    tasting_event_df: pd.DataFrame | None = None
    durations: dict[str, pd.DataFrame | int] | None = None
    intervals: dict[str, pd.DataFrame | int] | None = None
    tasting_durations: dict[str, pd.DataFrame | int] | None = None
    tasting_intervals: dict[str, pd.DataFrame | int] | None = None
    lick_matrix: np.ndarray | None = None
    dual_feeding_data: pd.DataFrame | None = None
    in_training_data: pd.DataFrame | None = None
    chambers: list[OneWellChamber | TwoWellChamber] | None = None

    @classmethod
    def load(
        cls,
        dfm_id: int,
        params: Parameters,
        *,
        data_dir: str | Path = ".",
        range_minutes: Sequence[float] = (0, 0),
        correct_for_dual_feeding: bool | None = None,
    ) -> DFM:
        loaded = load_dfm_csvs(dfm_id, data_dir=data_dir, range_minutes=range_minutes)
        df = loaded.df

        obj = cls(
            id=int(dfm_id),
            params=params,
            raw_df=df,
            version=int(loaded.version),
            source_files=loaded.source_files,
        )

        if correct_for_dual_feeding is None:
            correct_for_dual_feeding = bool(params.correct_for_dual_feeding)

        if obj.version == 3:
            obj._calculate_progressive_ratio_training()

        obj.recompute_all(correct_for_dual_feeding=bool(correct_for_dual_feeding))
        return obj

    def with_params(self, new_params: Parameters) -> DFM:
        """
        Equivalent of R's `ChangeParameterObject()` but returns a new object (no hidden global state).
        """

        new = DFM(
            id=self.id,
            params=new_params,
            raw_df=self.raw_df.copy(),
            version=self.version,
            source_files=self.source_files,
        )
        if new.version == 3:
            new._calculate_progressive_ratio_training()
        new.recompute_all(correct_for_dual_feeding=bool(new_params.correct_for_dual_feeding))
        return new

    def recompute_all(self, *, correct_for_dual_feeding: bool = False) -> None:
        self._calculate_baseline()
        self.thresholds = build_thresholds_table(self.baseline_df, self.params)
        self._build_chambers()
        self._calculate_feeding(correct_for_dual_feeding=correct_for_dual_feeding)
        self._calculate_tasting()

    def _build_chambers(self) -> None:
        """
        Build the chamber objects that compose this DFM.

        - chamber_size==1: 12 OneWellChamber instances (one per well)
        - chamber_size==2: one TwoWellChamber per row of params.chamber_sets
        """

        if self.params.chamber_size == 1:
            self.chambers = [
                OneWellChamber(index=i, dfm_id=self.id, params=self.params, well=i) for i in range(1, 13)
            ]
        elif self.params.chamber_size == 2:
            chambers: list[TwoWellChamber] = []
            for i, (w1, w2) in enumerate(self.params.chamber_sets.tolist()):
                left = int(w1)
                right = int(w2)
                if self.params.pi_direction == "left":
                    well_a, well_b = left, right
                elif self.params.pi_direction == "right":
                    well_a, well_b = right, left
                else:
                    raise ValueError(f"Invalid pi_direction: {self.params.pi_direction!r}")
                chambers.append(
                    TwoWellChamber(
                        index=i + 1,
                        dfm_id=self.id,
                        params=self.params,
                        left_well=left,
                        right_well=right,
                        well_a=well_a,
                        well_b=well_b,
                    )
                )
            self.chambers = chambers
        else:
            raise NotImplementedError("Unsupported chamber_size")

    def _calculate_progressive_ratio_training(self) -> None:
        # Port of `CalculateProgressiveRatioTraining()`.
        df = self.raw_df
        wcols = [f"W{i}" for i in range(1, 13) if f"W{i}" in df.columns]
        in_training_df = df[["Minutes", "Sample", *wcols]].copy() if "Sample" in df.columns else df[
            ["Minutes", *wcols]
        ].copy()
        for col in wcols:
            intraining = pd.to_numeric(df[col], errors="coerce").fillna(0) > 40000
            in_training_df[col] = intraining.to_numpy(dtype=bool)
            df.loc[intraining, col] = pd.to_numeric(df.loc[intraining, col], errors="coerce").fillna(0) - 65536
        self.raw_df = df

        # Port of `GetDoneTrainingInfo()`
        rows = []
        for i in range(1, 13):
            col = f"W{i}"
            if col not in in_training_df.columns:
                continue
            mask = in_training_df[col].to_numpy(dtype=bool)
            if not np.any(mask):
                rows.append({"well": col, "Minutes": np.nan, "Sample": np.nan})
            else:
                rows.append(
                    {
                        "well": col,
                        "Minutes": float(in_training_df.loc[mask, "Minutes"].max()),
                        "Sample": float(in_training_df.loc[mask, "Sample"].max())
                        if "Sample" in in_training_df.columns
                        else float(np.flatnonzero(mask).max() + 1),
                    }
                )
        self.in_training_data = pd.DataFrame(rows)

    def _calculate_baseline(self) -> None:
        # Port of `CalculateBaseline()` with the same window logic.
        df = self.raw_df.copy()
        window_min = float(self.params.baseline_window_minutes)
        # R code uses a fixed 5 samples/sec here.
        window = int(round(window_min * 60 * 5))
        if window % 2 == 0:
            window += 1

        for i in range(1, 13):
            col = f"W{i}"
            if col not in df.columns:
                continue
            df[col] = baseline_subtract(pd.to_numeric(df[col], errors="coerce").to_numpy(dtype=float), window)

        self.baseline_df = df

    def _calculate_feeding(self, *, correct_for_dual_feeding: bool) -> None:
        if self.chambers is None:
            self._build_chambers()

        lick_df = self.baseline_df.copy()
        event_df = self.baseline_df.copy()

        for chamber in self.chambers:
            for well in chamber.wells:
                cname = f"W{well}"
                res = compute_feeding_for_well(
                    baselined_well=lick_df[cname].to_numpy(dtype=float),
                    thresholds_well=self.thresholds[cname],
                    params=self.params,
                )
                lick_df[cname] = res.licks
                event_df[cname] = res.events

        lick_matrix = None
        dual_feeding_data = None

        if self.params.chamber_size == 2:
            lick_matrix = check_for_simultaneous_feeding(lick_df, self.baseline_df, self.params)
            if correct_for_dual_feeding:
                adj = adjust_baseline_for_dual_feeding(lick_df, self.baseline_df, self.params, dfm_id=self.id)
                dual_feeding_data = adj["dual_feeding_data"]
                adj_baselined = adj["baselined"]

                lick_df2 = adj_baselined.copy()
                event_df2 = adj_baselined.copy()
                for chamber in self.chambers:
                    for well in chamber.wells:
                        cname = f"W{well}"
                        res = compute_feeding_for_well(
                            baselined_well=adj_baselined[cname].to_numpy(dtype=float),
                            thresholds_well=self.thresholds[cname],
                            params=self.params,
                        )
                        lick_df2[cname] = res.licks
                        event_df2[cname] = res.events

                lick_df = lick_df2
                event_df = event_df2

        self.lick_df = lick_df
        self.event_df = event_df
        self.lick_matrix = lick_matrix
        self.dual_feeding_data = dual_feeding_data

        self.durations, self.intervals = bout_durations_and_intervals(
            self.baseline_df, self.event_df, params=self.params
        )

    def _calculate_tasting(self) -> None:
        if self.chambers is None:
            self._build_chambers()

        tasting_df = self.baseline_df.copy()
        tasting_event_df = self.baseline_df.copy()

        for chamber in self.chambers:
            for well in chamber.wells:
                cname = f"W{well}"
                res = compute_tasting_for_well(
                    baselined_well=self.baseline_df[cname].to_numpy(dtype=float),
                    thresholds_well=self.thresholds[cname],
                    feeding_licks_well=self.lick_df[cname].to_numpy(dtype=bool),
                    params=self.params,
                )
                tasting_df[cname] = res.licks
                tasting_event_df[cname] = res.events

        self.tasting_df = tasting_df
        self.tasting_event_df = tasting_event_df
        self.tasting_durations, self.tasting_intervals = tasting_bout_durations_and_intervals(
            self.baseline_df,
            self.tasting_event_df,
            params=self.params,
            interval_source_lick_df=self.lick_df,  # matches R behavior
        )

    # ---- Accessors ----
    def raw(self, *, range_minutes: Sequence[float] = (0, 0)) -> pd.DataFrame:
        if not range_is_specified(range_minutes):
            return self.raw_df
        a, b = float(range_minutes[0]), float(range_minutes[1])
        return self.raw_df[(self.raw_df["Minutes"] > a) & (self.raw_df["Minutes"] <= b)]

    def baselined(self, *, range_minutes: Sequence[float] = (0, 0)) -> pd.DataFrame:
        if not range_is_specified(range_minutes):
            return self.baseline_df
        a, b = float(range_minutes[0]), float(range_minutes[1])
        df = self.baseline_df
        return df[(df["Minutes"] > a) & (df["Minutes"] <= b)]

    # ---- QC ----
    def data_breaks(self, *, multiplier: float = 4.0) -> pd.DataFrame | None:
        return find_data_breaks(self.raw_df, self.params, multiplier=multiplier)

    def integrity_report(self) -> dict[str, object]:
        return report_dfm_integrity(self)

    def simultaneous_feeding_matrix(self) -> np.ndarray:
        """
        QC equivalent of `CheckForSimultaneousFeeding.DFM` (two-well only).
        """

        if self.params.chamber_size != 2:
            raise ValueError("simultaneous_feeding_matrix is for chamber_size==2 only")
        if self.lick_df is None or self.baseline_df is None:
            raise ValueError("DFM must have computed lick/baseline data first")
        return check_for_simultaneous_feeding(self.lick_df, self.baseline_df, self.params)

    def bleeding_check(self, cutoff: float) -> dict[str, object]:
        """
        QC equivalent of `CheckForBleeding.DFM` (data return only; no plot).
        """

        if self.baseline_df is None:
            raise ValueError("DFM must have baseline computed first")
        return check_for_bleeding(self.baseline_df, cutoff=cutoff)

    # ---- Stats ----
    def feeding_summary(
        self,
        *,
        range_minutes: Sequence[float] = (0, 0),
        transform_licks: bool = True,
    ) -> pd.DataFrame:
        if self.params.chamber_size == 1:
            return feeding_summary_one_well(
                dfm_id=self.id,
                raw=self.raw_df,
                baselined=self.baseline_df,
                lick_df=self.lick_df,
                event_df=self.event_df,
                durations=self.durations,
                intervals=self.intervals,
                params=self.params,
                range_minutes=range_minutes,
                transform_licks=transform_licks,
            )
        if self.params.chamber_size == 2:
            return feeding_summary_two_well(
                dfm_id=self.id,
                raw=self.raw_df,
                baselined=self.baseline_df,
                lick_df=self.lick_df,
                event_df=self.event_df,
                durations=self.durations,
                intervals=self.intervals,
                params=self.params,
                range_minutes=range_minutes,
                transform_licks=transform_licks,
            )
        raise NotImplementedError("Feeding summary not implemented for this DFM type.")

    def binned_feeding_summary(
        self,
        *,
        binsize_min: float = 30.0,
        range_minutes: Sequence[float] = (0, 0),
        transform_licks: bool = True,
    ) -> pd.DataFrame:
        return binned_feeding_summary(
            self, binsize_min=binsize_min, range_minutes=range_minutes, transform_licks=transform_licks
        )

    def interval_data(self, *, range_minutes: Sequence[float] = (0, 0)) -> pd.DataFrame:
        return interval_data_all_wells(self, range_minutes=range_minutes)

    def duration_data(self, *, range_minutes: Sequence[float] = (0, 0)) -> pd.DataFrame:
        return duration_data_all_wells(self, range_minutes=range_minutes)

    # ---- Plots ----
    def plot_raw(self, *, range_minutes: Sequence[float] = (0, 0)):
        from .plots import plot_raw

        return plot_raw(self, range_minutes=range_minutes)

    def plot_baselined(
        self, *, range_minutes: Sequence[float] = (0, 0), include_thresholds: bool = False
    ):
        from .plots import plot_baselined

        return plot_baselined(self, range_minutes=range_minutes, include_thresholds=include_thresholds)

    def plot_binned_licks(
        self,
        *,
        binsize_min: float = 30.0,
        range_minutes: Sequence[float] = (0, 0),
        transform_licks: bool = True,
    ):
        from .plots import plot_binned_licks

        return plot_binned_licks(
            self, binsize_min=binsize_min, range_minutes=range_minutes, transform_licks=transform_licks
        )

    def plot_cumulative_licks(self, *, single_plot: bool = False, transform_licks: bool = True):
        from .plots import plot_cumulative_licks

        return plot_cumulative_licks(self, single_plot=single_plot, transform_licks=transform_licks)

    def plot_cumulative_pi(self, *, range_minutes: Sequence[float] = (0, 0), single_plot: bool = False):
        from .plots import plot_cumulative_pi

        return plot_cumulative_pi(self, range_minutes=range_minutes, single_plot=single_plot)

    def plot_cumulative_event_pi(
        self,
        *,
        events_limit: int | None = None,
        range_minutes: Sequence[float] = (0, 0),
        single_plot: bool = False,
        by_bout: bool = False,
    ):
        from .plots import plot_cumulative_event_pi

        return plot_cumulative_event_pi(
            self,
            events_limit=events_limit,
            range_minutes=range_minutes,
            single_plot=single_plot,
            by_bout=by_bout,
        )

