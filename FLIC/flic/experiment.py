from __future__ import annotations

from contextlib import redirect_stdout
from dataclasses import dataclass
from io import StringIO
from pathlib import Path
from typing import Any, Literal, Sequence

import pandas as pd

from .dfm import DFM
from .experiment_design import ExperimentDesign


@dataclass(slots=True)
class Experiment:
    """
    Concrete experiment instance created from configuration.
    """

    dfms: dict[int, DFM]
    design: ExperimentDesign
    global_config: dict
    config_path: Path | None = None
    data_dir: Path | None = None
    range_minutes: tuple[float, float] | None = None
    parallel: bool | None = None
    executor: Literal["threads", "processes"] | None = None
    max_workers: int | None = None
    qc_report_dir: Path | None = None
    qc_results: dict[int, dict[str, Any]] | None = None

    @classmethod
    def load(
        cls,
        config_path: str | Path = "flic_config.yaml",
        *,
        data_dir: str | Path | None = None,
        range_minutes: Sequence[float] = (0, 0),
        parallel: bool = True,
        max_workers: int | None = None,
        executor: Literal["threads", "processes"] = "threads",
    ) -> Experiment:
        """
        Load an experiment from a YAML config.

        Defaults:
        - config_path: `flic_config.yaml`
        - relative to the current working directory
        """

        from .yaml_config import load_experiment_yaml

        return load_experiment_yaml(
            config_path,
            data_dir=data_dir,
            range_minutes=range_minutes,
            parallel=parallel,
            max_workers=max_workers,
            executor=executor,
        )

    def _simultaneous_feeding_matrix_df(self, dfm: DFM) -> pd.DataFrame:
        mat = dfm.simultaneous_feeding_matrix()
        n = int(dfm.params.chamber_sets.shape[0])
        return pd.DataFrame(
            mat,
            index=[f"Chamber{i}" for i in range(1, n + 1)],
            columns=["Licks1", "Licks2", "Both", "MaxMinSignalAtBoth", "HigherInCol1AtBoth"],
        )

    def _dfm_actual_range_minutes(self, dfm: DFM) -> tuple[float, float] | None:
        """
        Best-effort actual time range in minutes, based on the loaded raw data.
        """

        for col in ("Minutes", "minutes", "Minute", "minute"):
            if col in dfm.raw_df.columns:
                s = pd.to_numeric(dfm.raw_df[col], errors="coerce").dropna()
                if s.empty:
                    return None
                return float(s.min()), float(s.max())
        return None

    def compute_qc_results(
        self,
        *,
        data_breaks_multiplier: float = 4.0,
        bleeding_cutoff: float = 50.0,
        include_integrity_text: bool = False,
    ) -> dict[int, dict[str, Any]]:
        """
        Compute QC results for each DFM (does not write any files).
        """

        results: dict[int, dict[str, Any]] = {}
        for dfm_id in sorted(int(k) for k in self.dfms.keys()):
            dfm = self.dfms[dfm_id]

            # Integrity report prints; optionally capture it.
            integrity_text = None
            if include_integrity_text:
                buf = StringIO()
                with redirect_stdout(buf):
                    integrity = dfm.integrity_report()
                integrity_text = buf.getvalue()
            else:
                integrity = dfm.integrity_report()

            breaks = dfm.data_breaks(multiplier=float(data_breaks_multiplier))
            breaks_count = 0 if breaks is None else int(len(breaks))

            sim_df = None
            bleed = None
            if dfm.params.chamber_size == 2:
                sim_df = self._simultaneous_feeding_matrix_df(dfm)
                bleed = dfm.bleeding_check(cutoff=float(bleeding_cutoff))

            results[dfm_id] = {
                "integrity": integrity,
                "integrity_text": integrity_text,
                "data_breaks_multiplier": float(data_breaks_multiplier),
                "data_breaks_count": breaks_count,
                "data_breaks_head": None if breaks is None else breaks.head(10),
                "simultaneous_feeding_matrix": sim_df,
                "bleeding_cutoff": float(bleeding_cutoff),
                "bleeding": bleed,
            }

        self.qc_results = results
        return results

    def write_qc_reports(
        self,
        out_dir: str | Path,
        *,
        data_breaks_multiplier: float = 4.0,
        bleeding_cutoff: float = 50.0,
    ) -> Path:
        """
        Write per-DFM QC reports (CSV/TXT) to `out_dir` and return the resolved directory path.
        """

        out = Path(out_dir).expanduser().resolve()
        out.mkdir(parents=True, exist_ok=True)

        qc = self.compute_qc_results(
            data_breaks_multiplier=data_breaks_multiplier,
            bleeding_cutoff=bleeding_cutoff,
            include_integrity_text=True,
        )

        for dfm_id, r in qc.items():
            integrity = r["integrity"]
            pd.DataFrame([integrity]).to_csv(out / f"DFM{dfm_id}_integrity_report.csv", index=False)
            if r.get("integrity_text"):
                (out / f"DFM{dfm_id}_integrity_report.txt").write_text(
                    str(r["integrity_text"]), encoding="utf-8"
                )

            # Data breaks
            breaks_head = r.get("data_breaks_head")
            breaks_count = int(r.get("data_breaks_count", 0))
            if breaks_count > 0 and isinstance(breaks_head, pd.DataFrame):
                # Recompute full breaks for saving (head may be truncated).
                dfm = self.dfms[dfm_id]
                breaks = dfm.data_breaks(multiplier=float(data_breaks_multiplier))
                if breaks is not None:
                    breaks.to_csv(out / f"DFM{dfm_id}_data_breaks.csv", index=False)

            # Two-well only
            sim_df = r.get("simultaneous_feeding_matrix")
            if isinstance(sim_df, pd.DataFrame):
                sim_df.to_csv(out / f"DFM{dfm_id}_simultaneous_feeding_matrix.csv")

            bleed = r.get("bleeding")
            if isinstance(bleed, dict) and "Matrix" in bleed and "AllData" in bleed:
                bleed["Matrix"].to_csv(out / f"DFM{dfm_id}_bleeding_matrix.csv")
                bleed["AllData"].to_csv(out / f"DFM{dfm_id}_bleeding_alldata.csv", header=True)

        self.qc_report_dir = out
        return out

    def summary_text(
        self,
        *,
        include_qc: bool = True,
        qc_data_breaks_multiplier: float = 4.0,
        qc_bleeding_cutoff: float = 50.0,
    ) -> str:
        """
        Return a human-readable summary of what was loaded and how it was configured.
        """

        buf = StringIO()
        buf.write("FLIC experiment summary\n")
        buf.write("======================\n\n")

        if self.config_path is not None:
            buf.write(f"Config: {self.config_path}\n")
        if self.data_dir is not None:
            buf.write(f"Data dir: {self.data_dir}\n")
        if self.range_minutes is not None:
            buf.write(f"Requested range minutes: {self.range_minutes}\n")
        if self.parallel is not None:
            buf.write(f"Parallel: {self.parallel}\n")
        if self.executor is not None:
            buf.write(f"Executor: {self.executor}\n")
        if self.max_workers is not None:
            buf.write(f"Max workers: {self.max_workers}\n")
        if self.qc_report_dir is not None:
            buf.write(f"QC report dir: {self.qc_report_dir}\n")

        dfm_ids = sorted(int(k) for k in self.dfms.keys())
        buf.write(f"Loaded DFMs: {dfm_ids}\n")
        buf.write(f"Treatments: {sorted(self.design.treatments.keys())}\n\n")

        # Global params (if any)
        global_params = None
        if isinstance(self.global_config, dict):
            global_params = self.global_config.get("params", self.global_config.get("parameters", None))
        if global_params is not None:
            buf.write("Global params\n")
            buf.write("------------\n")
            if isinstance(global_params, dict) and global_params:
                for k in sorted(global_params.keys()):
                    buf.write(f"{k}: {global_params[k]!r}\n")
            else:
                buf.write(repr(global_params) + "\n")
            buf.write("\n")

        # Experimental design
        design_df = self.design.design_table()
        if not design_df.empty and all(c in design_df.columns for c in ("Treatment", "DFM", "Chamber")):
            design_df = design_df.sort_values(["Treatment", "DFM", "Chamber"])

        buf.write("Experimental design (DFM/Chamber -> Treatment)\n")
        buf.write("--------------------------------------------\n")
        if design_df.empty:
            buf.write("(empty)\n\n")
        else:
            buf.write(design_df.to_string(index=False))
            buf.write("\n\n")

        # Per-DFM details
        overall_min: float | None = None
        overall_max: float | None = None
        buf.write("DFM details\n")
        buf.write("-----------\n")
        for dfm_id in dfm_ids:
            dfm = self.dfms[dfm_id]
            buf.write(f"DFM {dfm_id}\n")
            buf.write(f"- version: {dfm.version}\n")
            buf.write(f"- raw rows: {len(dfm.raw_df)}\n")
            if dfm.source_files:
                buf.write("- source_files:\n")
                for p in dfm.source_files:
                    buf.write(f"  - {p}\n")
            actual = self._dfm_actual_range_minutes(dfm)
            if actual is not None:
                buf.write(f"- actual range minutes: ({actual[0]}, {actual[1]})\n")
                overall_min = actual[0] if overall_min is None else min(overall_min, actual[0])
                overall_max = actual[1] if overall_max is None else max(overall_max, actual[1])

            params = dfm.params
            buf.write("- params:\n")
            buf.write(f"  - chamber_size: {params.chamber_size!r}\n")
            buf.write(f"  - pi_direction: {params.pi_direction!r}\n")
            buf.write(f"  - correct_for_dual_feeding: {params.correct_for_dual_feeding!r}\n")
            buf.write(f"  - baseline_window_minutes: {params.baseline_window_minutes!r}\n")
            buf.write(f"  - feeding_threshold: {params.feeding_threshold!r}\n")
            buf.write(f"  - feeding_minimum: {params.feeding_minimum!r}\n")
            buf.write(f"  - tasting_minimum: {params.tasting_minimum!r}\n")
            buf.write(f"  - tasting_maximum: {params.tasting_maximum!r}\n")
            buf.write(f"  - feeding_minevents: {params.feeding_minevents!r}\n")
            buf.write(f"  - tasting_minevents: {params.tasting_minevents!r}\n")
            buf.write(f"  - samples_per_second: {params.samples_per_second!r}\n")
            buf.write(f"  - feeding_event_link_gap: {params.feeding_event_link_gap!r}\n")
            buf.write(f"  - chamber_sets: {params.chamber_sets.tolist()!r}\n")
            buf.write("\n")

        if overall_min is not None and overall_max is not None:
            buf.write(f"Overall actual loaded range (minutes): ({overall_min}, {overall_max})\n")

        if include_qc:
            buf.write("\nQC summary\n")
            buf.write("----------\n")
            qc = self.qc_results
            if qc is None:
                qc = self.compute_qc_results(
                    data_breaks_multiplier=qc_data_breaks_multiplier,
                    bleeding_cutoff=qc_bleeding_cutoff,
                    include_integrity_text=False,
                )

            for dfm_id in dfm_ids:
                r = qc.get(dfm_id, {})
                integrity = r.get("integrity", {})
                buf.write(f"DFM {dfm_id}\n")
                if isinstance(integrity, dict) and integrity:
                    buf.write(
                        f"- integrity: rows={integrity.get('n_rawdata')}, "
                        f"elapsed_min={integrity.get('elapsed_minutes_from_minutes_col')}, "
                        f"index_increments_by_one={integrity.get('index_increments_by_one')}\n"
                    )

                buf.write(
                    f"- data_breaks: count={int(r.get('data_breaks_count', 0))} "
                    f"(multiplier={r.get('data_breaks_multiplier')})\n"
                )
                head = r.get("data_breaks_head")
                if isinstance(head, pd.DataFrame) and not head.empty:
                    buf.write("  head:\n")
                    for line in head.to_string(index=False).splitlines():
                        buf.write(f"  {line}\n")

                sim_df = r.get("simultaneous_feeding_matrix")
                if isinstance(sim_df, pd.DataFrame):
                    buf.write("- simultaneous feeding (two-well):\n")
                    for line in sim_df.to_string().splitlines():
                        buf.write(f"  {line}\n")

                bleed = r.get("bleeding")
                if isinstance(bleed, dict) and "Matrix" in bleed and "AllData" in bleed:
                    mat_df = bleed["Matrix"]
                    all_data = bleed["AllData"]
                    max_resp = float(getattr(mat_df.to_numpy(), "max", lambda: 0.0)())
                    buf.write(f"- bleeding check (cutoff={r.get('bleeding_cutoff')}): max_matrix={max_resp}\n")
                    buf.write("  all_data (mean signal per well):\n")
                    for line in all_data.to_string().splitlines():
                        buf.write(f"  {line}\n")

                buf.write("\n")

        return buf.getvalue()

    def plot_feeding_summary(
        self,
        *,
        range_minutes: Sequence[float] = (0, 0),
        transform_licks: bool = True,
        ncols: int = 2,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """
        Produce dot/box plots of feeding summary metrics grouped by treatment.

        Each subplot shows one metric with individual chamber data points (jittered dots)
        overlaid on a box plot, grouped by treatment on the x-axis.

        For chamber_size=2, includes PI, EventPI, and per-well (A/B) metrics.
        For chamber_size=1, includes Licks, Events, Duration, TimeBtw, and Interval metrics.

        Returns a matplotlib Figure.
        """
        import math

        import matplotlib.pyplot as plt
        import numpy as np

        df = self.design.feeding_summary(
            range_minutes=range_minutes, transform_licks=transform_licks
        )
        if df.empty:
            fig, ax = plt.subplots(figsize=(6, 3))
            ax.set_title("No feeding summary data")
            return fig

        chamber_size = next(iter(self.dfms.values())).params.chamber_size
        treatments = sorted(df["Treatment"].unique().tolist())
        colors = [plt.cm.tab10(i % 10) for i in range(len(treatments))]

        one_well_metrics = [
            ("Licks", "Licks"),
            ("Events", "Events"),
            ("MeanDuration", "Mean Duration (s)"),
            ("MedDuration", "Median Duration (s)"),
            ("MeanTimeBtw", "Mean Time Btw (s)"),
            ("MedTimeBtw", "Median Time Btw (s)"),
            ("MeanInt", "Mean Interval (s)"),
            ("MedianInt", "Median Interval (s)"),
        ]

        two_well_metrics = [
            ("PI", "PI"),
            ("EventPI", "Event PI"),
            ("LicksA", "Licks (Well A)"),
            ("LicksB", "Licks (Well B)"),
            ("EventsA", "Events (Well A)"),
            ("EventsB", "Events (Well B)"),
            ("MeanDurationA", "Mean Duration A (s)"),
            ("MeanDurationB", "Mean Duration B (s)"),
            ("MedDurationA", "Median Duration A (s)"),
            ("MedDurationB", "Median Duration B (s)"),
            ("MeanTimeBtwA", "Mean Time Btw A (s)"),
            ("MeanTimeBtwB", "Mean Time Btw B (s)"),
            ("MedTimeBtwA", "Median Time Btw A (s)"),
            ("MedTimeBtwB", "Median Time Btw B (s)"),
            ("MeanIntA", "Mean Interval A (s)"),
            ("MeanIntB", "Mean Interval B (s)"),
            ("MedianIntA", "Median Interval A (s)"),
            ("MedianIntB", "Median Interval B (s)"),
        ]

        all_metrics = two_well_metrics if chamber_size == 2 else one_well_metrics
        metrics = [(col, lbl) for col, lbl in all_metrics if col in df.columns]

        if not metrics:
            fig, ax = plt.subplots(figsize=(6, 3))
            ax.set_title("No matching columns in feeding summary")
            return fig

        nrows = math.ceil(len(metrics) / ncols)
        if figsize is None:
            figsize = (ncols * 5, nrows * 3.5)

        fig, axes = plt.subplots(nrows, ncols, figsize=figsize)
        axes_flat = np.array(axes).flatten()

        rng = np.random.default_rng(42)
        positions = list(range(len(treatments)))

        for idx, (col, label) in enumerate(metrics):
            ax = axes_flat[idx]
            groups = [
                df.loc[df["Treatment"] == t, col].dropna().to_numpy(dtype=float)
                for t in treatments
            ]

            # Boxplot — only pass non-empty groups to avoid matplotlib warnings.
            nonempty = [(p, g, c) for p, g, c in zip(positions, groups, colors) if len(g) > 0]
            if nonempty:
                ne_pos, ne_grp, ne_colors = zip(*nonempty)
                bp = ax.boxplot(
                    list(ne_grp),
                    positions=list(ne_pos),
                    widths=0.4,
                    patch_artist=True,
                    medianprops={"color": "black", "linewidth": 1.5},
                    whiskerprops={"linewidth": 1.0},
                    capprops={"linewidth": 1.0},
                    showfliers=False,
                )
                for patch, color in zip(bp["boxes"], ne_colors):
                    patch.set_facecolor(color)
                    patch.set_alpha(0.4)

            # Jittered dots overlaid on boxes.
            for pos, vals, color in zip(positions, groups, colors):
                if len(vals) == 0:
                    continue
                jitter = rng.uniform(-0.15, 0.15, len(vals))
                ax.scatter(pos + jitter, vals, color=color, s=30, zorder=3, alpha=0.85)

            ax.set_xticks(positions)
            ax.set_xticklabels(treatments, rotation=30, ha="right", fontsize=8)
            ax.set_xlim(-0.6, len(treatments) - 0.4)
            ax.set_ylabel(label, fontsize=9)
            ax.set_title(col, fontsize=9)

        for idx in range(len(metrics), len(axes_flat)):
            axes_flat[idx].set_visible(False)

        fig.suptitle("Feeding Summary by Treatment", fontsize=12)
        fig.tight_layout()
        return fig

    def _binned_licks_table_by_treatment(
        self,
        *,
        binsize_min: float = 30.0,
        range_minutes: Sequence[float] = (0, 0),
        transform_licks: bool = True,
    ) -> pd.DataFrame:
        """
        Build a long table of binned feeding-summary rows for chambers assigned to treatments.

        Output columns:
        - Treatment, DFM, Chamber, Interval, Minutes, ... (feeding summary metrics)
        """

        def range_is_specified_local(r: Sequence[float]) -> bool:
            try:
                return not (float(r[0]) == 0.0 and float(r[1]) == 0.0)
            except Exception:  # noqa: BLE001
                return False

        # If caller didn't specify a range but the experiment was loaded with one, use it.
        effective_range = range_minutes
        if (not range_is_specified_local(range_minutes)) and self.range_minutes is not None:
            if range_is_specified_local(self.range_minutes):
                effective_range = self.range_minutes

        # Cache binned summary per DFM so we don't recompute repeatedly per treatment.
        binned_by_dfm: dict[int, pd.DataFrame] = {}
        for dfm_id, dfm in self.dfms.items():
            binned_by_dfm[int(dfm_id)] = dfm.binned_feeding_summary(
                binsize_min=float(binsize_min),
                range_minutes=effective_range,
                transform_licks=bool(transform_licks),
            )

        parts: list[pd.DataFrame] = []
        for trt_name, trt in self.design.treatments.items():
            if not trt.chambers:
                continue

            # Group selection by DFM
            by_dfm: dict[int, set[int]] = {}
            for tc in trt.chambers:
                by_dfm.setdefault(int(tc.dfm_id), set()).add(int(tc.chamber_index))

            for dfm_id, chamber_idxs in by_dfm.items():
                binned = binned_by_dfm.get(int(dfm_id))
                if binned is None or binned.empty or "Chamber" not in binned.columns:
                    continue

                tmp = binned[binned["Chamber"].astype(int).isin(chamber_idxs)].copy()
                if tmp.empty:
                    continue

                tmp.insert(0, "Treatment", str(trt_name))

                # Ensure DFM exists for grouping even if upstream summary didn't include it.
                if "DFM" not in tmp.columns:
                    tmp.insert(tmp.columns.get_loc("Treatment") + 1, "DFM", int(dfm_id))
                parts.append(tmp)

        return pd.concat(parts, ignore_index=True) if parts else pd.DataFrame()

    def _metric_series_from_binned_rows(
        self,
        binned_rows: pd.DataFrame,
        *,
        metric: str,
        two_well_mode: Literal["total", "mean_ab", "A", "B"] = "total",
    ) -> pd.Series:
        """
        Given binned feeding-summary rows, compute a 1D series of metric values.

        `metric` can be:
        - a column name present in the binned feeding summary (e.g. 'PI', 'EventPI', 'LicksA', 'MedDurationB', ...)
        - one of the common base names: 'Licks', 'Events', 'MeanDuration', 'MedDuration', 'MeanTimeBtw', 'MedTimeBtw',
          'MeanInt', 'MedianInt'

        For two-well summaries and base names, `two_well_mode` controls how A/B are combined:
        - 'A' / 'B': use the A or B column
        - 'total': sum A+B (best for Licks/Events)
        - 'mean_ab': mean of A and B (best for duration/interval/time-between metrics)
        """

        m = str(metric).strip()
        if m in binned_rows.columns:
            return pd.to_numeric(binned_rows[m], errors="coerce")

        base = m
        base_map = {
            "Licks": ("LicksA", "LicksB", "Licks"),
            "Events": ("EventsA", "EventsB", "Events"),
            "MeanDuration": ("MeanDurationA", "MeanDurationB", "MeanDuration"),
            "MedDuration": ("MedDurationA", "MedDurationB", "MedDuration"),
            "MeanTimeBtw": ("MeanTimeBtwA", "MeanTimeBtwB", "MeanTimeBtw"),
            "MedTimeBtw": ("MedTimeBtwA", "MedTimeBtwB", "MedTimeBtw"),
            "MeanInt": ("MeanIntA", "MeanIntB", "MeanInt"),
            "MedianInt": ("MedianIntA", "MedianIntB", "MedianInt"),
        }
        if base not in base_map:
            raise ValueError(
                f"Unknown metric {metric!r}. Provide an existing column name, or one of: {sorted(base_map.keys())}"
            )

        col_a, col_b, col_single = base_map[base]
        if col_a in binned_rows.columns and col_b in binned_rows.columns:
            a = pd.to_numeric(binned_rows[col_a], errors="coerce")
            b = pd.to_numeric(binned_rows[col_b], errors="coerce")
            mode = str(two_well_mode)
            if mode == "A":
                return a
            if mode == "B":
                return b
            if mode == "mean_ab":
                return (a + b) / 2.0
            # total
            return a.fillna(0.0) + b.fillna(0.0)

        if col_single in binned_rows.columns:
            return pd.to_numeric(binned_rows[col_single], errors="coerce")

        raise ValueError(
            f"Metric {metric!r} not available in binned summary columns. "
            f"Missing expected columns: {col_single!r} or ({col_a!r},{col_b!r})."
        )

    def plot_binned_metric_by_treatment(
        self,
        *,
        metric: str = "Licks",
        two_well_mode: Literal["total", "mean_ab", "A", "B"] = "total",
        binsize_min: float = 30.0,
        range_minutes: Sequence[float] = (0, 0),
        transform_licks: bool = True,
        show_sem: bool = True,
        show_individual_chambers: bool = False,
        figsize: tuple[float, float] = (10, 4),
    ) -> Any:
        """
        Plot mean ± SEM of a binned feeding-summary metric for each treatment across the experiment.

        - Uses the chamber→treatment assignments in `Experiment.design`.
        - Uses per-DFM `binned_feeding_summary()` and aggregates across chambers for each treatment.

        Returns a matplotlib Figure.
        """

        import matplotlib.pyplot as plt
        import numpy as np

        df = self._binned_licks_table_by_treatment(
            binsize_min=binsize_min, range_minutes=range_minutes, transform_licks=transform_licks
        )
        if df.empty:
            fig, ax = plt.subplots(figsize=figsize)
            ax.set_title("Binned metric by treatment (no data)")
            return fig

        df = df.copy()
        df["MetricValue"] = self._metric_series_from_binned_rows(df, metric=metric, two_well_mode=two_well_mode)

        df = df.dropna(subset=["Minutes", "MetricValue"]).copy()
        df["Minutes"] = pd.to_numeric(df["Minutes"], errors="coerce")
        df["MetricValue"] = pd.to_numeric(df["MetricValue"], errors="coerce")
        df = df.dropna(subset=["Minutes", "MetricValue"])

        treatments = sorted(df["Treatment"].unique().tolist())

        agg = (
            df.groupby(["Treatment", "Minutes"], as_index=False)["MetricValue"]
            .agg(mean="mean", std="std", n="count")
            .rename(columns={"mean": "Mean", "std": "Std", "n": "N"})
        )
        agg["SEM"] = (agg["Std"] / np.sqrt(agg["N"].clip(lower=1))).fillna(0.0)

        fig, ax = plt.subplots(figsize=figsize)
        colors = [plt.cm.tab10(i % 10) for i in range(len(treatments))]

        if show_individual_chambers:
            for i, trt in enumerate(treatments):
                tmp = df[df["Treatment"] == trt]
                for (dfm_id, ch), g in tmp.groupby(["DFM", "Chamber"]):
                    g = g.sort_values("Minutes")
                    ax.plot(
                        g["Minutes"],
                        g["MetricValue"],
                        color=colors[i],
                        alpha=0.18,
                        linewidth=0.8,
                    )

        for i, trt in enumerate(treatments):
            tmp = agg[agg["Treatment"] == trt].sort_values("Minutes")
            x = tmp["Minutes"].to_numpy(dtype=float)
            y = tmp["Mean"].to_numpy(dtype=float)
            e = tmp["SEM"].to_numpy(dtype=float)

            ax.plot(x, y, color=colors[i], linewidth=2.0, label=f"{trt} (n={int(tmp['N'].max())})")
            if show_sem:
                ax.fill_between(x, y - e, y + e, color=colors[i], alpha=0.2, linewidth=0)

        ax.set_xlabel("Minutes")
        ylabel = metric
        if metric in ("Licks", "Events") and two_well_mode == "total":
            ylabel = f"{metric} (total)"
        ax.set_ylabel(("Transformed " if transform_licks and metric == "Licks" else "") + ylabel)
        ax.set_title(f"Binned {metric} by treatment (mean ± SEM)")
        ax.legend(fontsize=8, ncol=2)
        fig.tight_layout()
        return fig

    def plot_binned_metrics_by_treatment(
        self,
        *,
        metrics: Sequence[str] = ("Licks", "Events", "MedDuration"),
        two_well_mode: Literal["total", "mean_ab", "A", "B"] = "total",
        binsize_min: float = 30.0,
        range_minutes: Sequence[float] = (0, 0),
        transform_licks: bool = True,
        show_sem: bool = True,
        show_individual_chambers: bool = False,
        ncols: int = 2,
        figsize: tuple[float, float] | None = None,
    ) -> Any:
        """
        Plot multiple binned treatment timecourses (mean ± SEM), one subplot per metric.
        """

        import math

        import matplotlib.pyplot as plt
        import numpy as np

        metrics = [str(m).strip() for m in metrics if str(m).strip()]
        if not metrics:
            fig, ax = plt.subplots(figsize=(6, 3))
            ax.set_title("No metrics specified")
            return fig

        df = self._binned_licks_table_by_treatment(
            binsize_min=binsize_min, range_minutes=range_minutes, transform_licks=transform_licks
        )
        if df.empty:
            fig, ax = plt.subplots(figsize=(6, 3))
            ax.set_title("Binned metrics by treatment (no data)")
            return fig

        df = df.copy()
        df["Minutes"] = pd.to_numeric(df["Minutes"], errors="coerce")
        df = df.dropna(subset=["Minutes"])
        treatments = sorted(df["Treatment"].unique().tolist())
        colors = [plt.cm.tab10(i % 10) for i in range(len(treatments))]

        nrows = math.ceil(len(metrics) / int(ncols))
        if figsize is None:
            figsize = (ncols * 6, nrows * 3.8)
        fig, axes = plt.subplots(nrows, ncols, figsize=figsize, sharex=True)
        axes_flat = np.array(axes).flatten()

        for i, metric in enumerate(metrics):
            ax = axes_flat[i]
            tmp = df.copy()
            tmp["MetricValue"] = self._metric_series_from_binned_rows(
                tmp, metric=metric, two_well_mode=two_well_mode
            )
            tmp["MetricValue"] = pd.to_numeric(tmp["MetricValue"], errors="coerce")
            tmp = tmp.dropna(subset=["MetricValue"])

            agg = (
                tmp.groupby(["Treatment", "Minutes"], as_index=False)["MetricValue"]
                .agg(mean="mean", std="std", n="count")
                .rename(columns={"mean": "Mean", "std": "Std", "n": "N"})
            )
            agg["SEM"] = (agg["Std"] / np.sqrt(agg["N"].clip(lower=1))).fillna(0.0)

            if show_individual_chambers:
                for t_i, trt in enumerate(treatments):
                    chamber_rows = tmp[tmp["Treatment"] == trt]
                    for (_dfm_id, _ch), g in chamber_rows.groupby(["DFM", "Chamber"]):
                        g = g.sort_values("Minutes")
                        ax.plot(
                            g["Minutes"],
                            g["MetricValue"],
                            color=colors[t_i],
                            alpha=0.18,
                            linewidth=0.8,
                        )

            for t_i, trt in enumerate(treatments):
                g = agg[agg["Treatment"] == trt].sort_values("Minutes")
                x = g["Minutes"].to_numpy(dtype=float)
                y = g["Mean"].to_numpy(dtype=float)
                e = g["SEM"].to_numpy(dtype=float)
                ax.plot(x, y, color=colors[t_i], linewidth=2.0, label=trt)
                if show_sem:
                    ax.fill_between(x, y - e, y + e, color=colors[t_i], alpha=0.2, linewidth=0)

            ylabel = metric
            if metric in ("Licks", "Events") and two_well_mode == "total":
                ylabel = f"{metric} (total)"
            ax.set_ylabel(("Transformed " if transform_licks and metric == "Licks" else "") + ylabel)
            ax.set_title(metric)
            ax.legend(fontsize=7, ncol=1)

        for j in range(len(metrics), len(axes_flat)):
            axes_flat[j].set_visible(False)

        fig.suptitle("Binned metrics by treatment (mean ± SEM)", fontsize=12)
        fig.tight_layout()
        return fig

    def plot_binned_licks_by_treatment(
        self,
        *,
        binsize_min: float = 30.0,
        range_minutes: Sequence[float] = (0, 0),
        transform_licks: bool = True,
        show_sem: bool = True,
        show_individual_chambers: bool = False,
        figsize: tuple[float, float] = (10, 4),
    ) -> Any:
        """
        Backward-compatible wrapper: plot binned licks by treatment (mean ± SEM).
        """

        return self.plot_binned_metric_by_treatment(
            metric="Licks",
            two_well_mode="total",
            binsize_min=binsize_min,
            range_minutes=range_minutes,
            transform_licks=transform_licks,
            show_sem=show_sem,
            show_individual_chambers=show_individual_chambers,
            figsize=figsize,
        )

    def write_summary(
        self,
        path: str | Path,
        *,
        include_qc: bool = True,
        qc_data_breaks_multiplier: float = 4.0,
        qc_bleeding_cutoff: float = 50.0,
    ) -> Path:
        """
        Write `summary_text()` to disk and return the resolved output path.
        """

        out = Path(path).expanduser().resolve()
        out.parent.mkdir(parents=True, exist_ok=True)
        out.write_text(
            self.summary_text(
                include_qc=include_qc,
                qc_data_breaks_multiplier=qc_data_breaks_multiplier,
                qc_bleeding_cutoff=qc_bleeding_cutoff,
            ),
            encoding="utf-8",
        )
        return out

