#!/usr/bin/env python3
"""
Progressive Assay Two-Choice Analysis
======================================
Python port of RRmodified_ProgAssayTwoChoice_Jan25.R

Loads FLIC data from a YAML config, computes feeding summaries for different
time windows, produces QC plots, per-well lick plots, and analysis graphs
for a two-choice progressive assay experiment (e.g., sucrose vs. yeast).

Key mapping from R to Python:
  ParametersClass.TwoWell()     -> Parameters.two_well()
  SetParameter(p, ...)          -> p.with_updates(...)
  PI.Multiplier=-1              -> pi_direction="right"
  Feeding.Summary.Monitors(...) -> exp.design.feeding_summary(range_minutes=...)
  RawDataPlot.DFM(DFM1)        -> plot_raw(dfm)
  separate(fs6, "Treatment")   -> fs6["Treatment"].str.split("_", expand=True)
  ggplot jitter + stat_summary  -> jitter_stripplot() below
  aov(...) / summary(...)       -> statsmodels anova_lm

The YAML config (flic_config.yaml) controls:
  - Which DFMs to load and their pi_direction
  - Chamber-to-treatment assignments
  - Global parameters (feeding_threshold, feeding_minimum, etc.)

For a two-choice assay the LicksA / EventsA / MedDurationA columns always
refer to the "sucrose side" well (WellA as configured by pi_direction).
LicksB / EventsB / MedDurationB refer to the alternative food (e.g. yeast).

Usage:
    cd /path/to/experiment/data
    python -m flic.scripts.prog_assay_two_choice --config flic_config.yaml

    # Or from anywhere, pointing at a config:
    python -m flic.scripts.prog_assay_two_choice \\
        --config /data/exp/flic_config.yaml \\
        --out-dir /data/exp/results
"""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Sequence

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import yaml

from flic import Experiment
from flic.plots import plot_raw, plot_binned_licks

# ─── Colour palette (mirrors R script scale_color_manual) ────────────────────
_PALETTE = ["#C62121", "#292836", "#52A08D", "#C1CEDA", "#494856"]


# ─── Utility helpers ──────────────────────────────────────────────────────────

def _resolve_data_dir(config_path: Path, data_dir_value: str | None) -> Path | None:
    if not data_dir_value:
        return None
    p = Path(data_dir_value)
    if p.is_absolute():
        return p
    return (config_path.parent / p).resolve()


def separate_treatment(
    df: pd.DataFrame,
    col: str = "Treatment",
    sep: str = "_",
    into: list[str] | None = None,
    n: int = 2,
) -> pd.DataFrame:
    """
    Split a single Treatment column into multiple columns.

    Equivalent to R's tidyr::separate().

    Parameters
    ----------
    col  : column to split
    sep  : separator character
    into : list of new column names (length determines max splits + 1)
    n    : maximum number of splits if ``into`` is not provided
           (produces n+1 columns)
    """
    df = df.copy()
    if into is None:
        into = [f"Part{i}" for i in range(n + 1)]
    splits = df[col].str.split(sep, n=len(into) - 1, expand=True)
    for i, name in enumerate(into):
        df[name] = splits[i] if i < splits.shape[1] else pd.NA
    return df


# ─── Plotting helpers ─────────────────────────────────────────────────────────

def _jitter_stripplot(
    ax: plt.Axes,
    df: pd.DataFrame,
    x_col: str,
    y_col: str,
    *,
    palette: dict[str, str] | None = None,
    jitter_width: float = 0.25,
    dot_size: float = 40.0,
    rng: np.random.Generator | None = None,
) -> list[str]:
    """
    Jittered dot strip plot with mean ± SEM overlay.

    Mirrors ggplot2's geom_jitter + stat_summary(fun=mean) +
    stat_summary(fun.data=mean_se, geom="errorbar").

    Returns the ordered group labels (x-axis categories).
    """
    if rng is None:
        rng = np.random.default_rng(42)

    groups = sorted(df[x_col].dropna().unique().tolist())
    pal = palette or {}

    for i, grp in enumerate(groups):
        vals = df.loc[df[x_col] == grp, y_col].dropna().to_numpy(dtype=float)
        color = pal.get(str(grp), _PALETTE[i % len(_PALETTE)])
        if len(vals) == 0:
            continue
        jitter = rng.uniform(-jitter_width, jitter_width, len(vals))
        ax.scatter(np.full(len(vals), i) + jitter, vals,
                   color=color, s=dot_size, zorder=3, alpha=0.85)

        mean_val = float(np.mean(vals))
        sem_val = float(np.std(vals, ddof=1) / np.sqrt(len(vals))) if len(vals) > 1 else 0.0
        # Mean marker (equivalent to stat_summary shape=13 / ✳)
        ax.plot(i, mean_val, marker="*", color="#333333", ms=10, zorder=5)
        # Error bar (equivalent to stat_summary geom="errorbar")
        ax.errorbar(i, mean_val, yerr=sem_val,
                    color="#333333", capsize=4, linewidth=1.0, zorder=4)

    ax.set_xticks(range(len(groups)))
    ax.set_xticklabels(groups, rotation=30, ha="right", fontsize=9)
    ax.set_xlim(-0.6, len(groups) - 0.4)
    return groups


def plot_metric_by_treatment(
    df: pd.DataFrame,
    x_col: str,
    y_col: str,
    *,
    title: str = "",
    xlabel: str = "",
    ylabel: str = "",
    facet_col: str | None = None,
    figsize: tuple[float, float] = (10, 6),
) -> plt.Figure:
    """
    Jittered strip plot of one metric broken down by treatment (x-axis).

    Optional faceting by ``facet_col`` (e.g. "Genotype").

    Mirrors the series of ggplot calls in the R script:
        ggplot(fs6, aes(Treatment, MedDurationA)) +
          geom_jitter(...) + stat_summary(fun=mean) +
          stat_summary(fun.data=mean_se, geom="errorbar")
    """
    rng = np.random.default_rng(42)
    groups = sorted(df[x_col].dropna().unique().tolist())
    palette = {str(g): _PALETTE[i % len(_PALETTE)] for i, g in enumerate(groups)}

    if facet_col and facet_col in df.columns:
        facets = sorted(df[facet_col].dropna().unique().tolist())
        if not facets:
            facets = ["(all)"]
        fig, axes = plt.subplots(1, len(facets), figsize=figsize, sharey=True)
        if len(facets) == 1:
            axes = [axes]
        for ax, fval in zip(axes, facets):
            sub = df[df[facet_col] == fval] if fval != "(all)" else df
            _jitter_stripplot(ax, sub, x_col, y_col, palette=palette, rng=rng)
            ax.set_title(str(fval), fontsize=10)
            ax.set_xlabel(xlabel or x_col, fontsize=10)
            ax.set_ylabel(ylabel or y_col, fontsize=10)
    else:
        fig, ax = plt.subplots(figsize=figsize)
        _jitter_stripplot(ax, df, x_col, y_col, palette=palette, rng=rng)
        ax.set_xlabel(xlabel or x_col, fontsize=10)
        ax.set_ylabel(ylabel or y_col, fontsize=10)

    fig.suptitle(title, fontsize=12)
    fig.tight_layout()
    return fig


def plot_food_comparison(
    df: pd.DataFrame,
    col_a: str,
    col_b: str,
    *,
    label_a: str = "S2",
    label_b: str = "Yeast",
    value_label: str = "Events",
    title: str = "",
    facet_col: str = "Treatment",
    figsize: tuple[float, float] = (14, 5),
) -> plt.Figure:
    """
    Long-format jitter comparison plot of two food metrics (e.g. sucrose vs. yeast).

    Mirrors the R 'Homeostatic Feeding' section:
        tmp <- fs[, c("Treatment", "Group", "EventsB", "Experiment")]
        ...
        s2.data <- rbind(s2.data, tmp)
        ggplot(s2.data, aes(Food, Events)) + facet_wrap("Treatment") + ...

    col_a is the 'A' well metric (sucrose), col_b is the 'B' well metric (yeast).
    """
    # Build long-format DataFrame
    needed = [facet_col, col_a, col_b]
    missing = [c for c in needed if c not in df.columns]
    if missing:
        fig, ax = plt.subplots(figsize=(6, 3))
        ax.set_title(f"plot_food_comparison: missing columns {missing}")
        return fig

    rows_a = df[[facet_col, col_a]].copy()
    rows_a.columns = [facet_col, value_label]
    rows_a["Food"] = label_a

    rows_b = df[[facet_col, col_b]].copy()
    rows_b.columns = [facet_col, value_label]
    rows_b["Food"] = label_b

    combined = pd.concat([rows_a, rows_b], ignore_index=True)

    rng = np.random.default_rng(42)
    palette = {label_a: "#52A08D", label_b: "#292836"}

    facets = sorted(df[facet_col].dropna().unique().tolist())
    if not facets:
        facets = ["(all)"]

    fig, axes = plt.subplots(1, len(facets), figsize=figsize, sharey=True)
    if len(facets) == 1:
        axes = [axes]

    for ax, fval in zip(axes, facets):
        sub = combined[combined[facet_col] == fval] if fval != "(all)" else combined
        _jitter_stripplot(ax, sub, "Food", value_label, palette=palette, rng=rng)
        ax.set_title(str(fval), fontsize=10)
        ax.set_xlabel("Food", fontsize=10)
        ax.set_ylabel(value_label if ax is axes[0] else "", fontsize=10)

    fig.suptitle(title, fontsize=12)
    fig.tight_layout()
    return fig


# ─── Statistics ───────────────────────────────────────────────────────────────

def run_anova(
    df: pd.DataFrame,
    y_col: str,
    factors: Sequence[str],
    *,
    weights_col: str | None = None,
    label: str = "",
) -> None:
    """
    Fit an OLS model and print a Type-I ANOVA table.

    Mirrors R's aov() / summary.aov():
        aov(MedDurationA ~ Experiment + Treatment, data=fs6)
        aov(MedDurationA ~ Experiment + Treatment, weights=EventsA, data=fs6)
    """
    try:
        import statsmodels.formula.api as smf
        from statsmodels.stats.anova import anova_lm
    except ImportError:
        print("  [ANOVA skipped] statsmodels is not installed.")
        return

    cols_needed = [y_col] + list(factors)
    if weights_col:
        cols_needed.append(weights_col)
    sub = df[cols_needed].dropna()

    # Sanitize column names for the formula (replace spaces and dots)
    rename = {}
    for c in cols_needed:
        safe = c.replace(" ", "_").replace(".", "_")
        if safe != c:
            rename[c] = safe
    if rename:
        sub = sub.rename(columns=rename)
        y_col = rename.get(y_col, y_col)
        factors = [rename.get(f, f) for f in factors]
        if weights_col:
            weights_col = rename.get(weights_col, weights_col)

    formula = f"{y_col} ~ " + " + ".join(factors)
    if label:
        print(f"\n{'─'*60}")
        print(f"ANOVA: {label}")
        print(f"Formula: {formula}")
    else:
        print(f"\nANOVA: {formula}")

    try:
        if weights_col:
            model = smf.wls(formula, data=sub, weights=sub[weights_col]).fit()
        else:
            model = smf.ols(formula, data=sub).fit()
        table = anova_lm(model, typ=1)
        print(table.to_string())
    except Exception as exc:
        print(f"  [ANOVA error] {exc}")


# ─── Main ─────────────────────────────────────────────────────────────────────

def main() -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Progressive assay two-choice analysis.\n"
            "Python port of RRmodified_ProgAssayTwoChoice_Jan25.R"
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "--config",
        default="flic_config.yaml",
        help="Path to YAML config (default: flic_config.yaml in CWD).",
    )
    parser.add_argument(
        "--out-dir",
        default=".",
        help="Directory for all output files (default: CWD).",
    )
    parser.add_argument(
        "--hours-short",
        type=float,
        default=6.0,
        metavar="H",
        help="Duration of the 'short' analysis window in hours (default: 6).",
    )
    parser.add_argument(
        "--hours-long",
        type=float,
        default=24.0,
        metavar="H",
        help="Duration of the 'long' analysis window in hours (default: 24).",
    )
    parser.add_argument(
        "--treatment-sep",
        default="_",
        metavar="SEP",
        help="Separator used to split Treatment names (default: '_').",
    )
    parser.add_argument(
        "--treatment-parts",
        nargs="+",
        default=["Condition", "Genotype", "Group"],
        metavar="PART",
        help=(
            "Column names to split Treatment into (default: Condition Genotype Group). "
            "Adjust to match your naming scheme, e.g. --treatment-parts Condition Group."
        ),
    )
    parser.add_argument(
        "--sucrose-label",
        default="S2",
        metavar="LABEL",
        help="Label for the sucrose/A-well food (default: S2).",
    )
    parser.add_argument(
        "--yeast-label",
        default="Yeast",
        metavar="LABEL",
        help="Label for the yeast/B-well food (default: Yeast).",
    )
    parser.add_argument(
        "--no-parallel",
        action="store_true",
        help="Disable parallel DFM loading.",
    )
    parser.add_argument(
        "--no-stats",
        action="store_true",
        help="Skip ANOVA output.",
    )
    parser.add_argument(
        "--binsize-min",
        type=float,
        default=30.0,
        metavar="MIN",
        help="Bin size in minutes for binned feeding summaries (default: 30).",
    )
    args = parser.parse_args()

    config_path = Path(args.config).resolve()
    if not config_path.exists():
        parser.error(f"Config file not found: {config_path}")

    out_dir = Path(args.out_dir).resolve()
    out_dir.mkdir(parents=True, exist_ok=True)

    short_min = args.hours_short * 60.0
    long_min = args.hours_long * 60.0
    late_start = short_min  # e.g. 360 for hours 6-24

    # Resolve data_dir relative to config
    cfg = yaml.safe_load(config_path.read_text())
    if not isinstance(cfg, dict):
        raise ValueError("Config YAML root must be a mapping.")
    data_dir = _resolve_data_dir(config_path, cfg.get("data_dir"))

    # ── Load experiment ───────────────────────────────────────────────────────
    # Load the maximum range needed so we can compute all sub-windows from it.
    print(f"Loading experiment (0 – {args.hours_long:.0f} hrs)...")
    exp = Experiment.load(
        config_path,
        data_dir=data_dir,
        range_minutes=(0.0, long_min),
        parallel=not args.no_parallel,
        executor="threads",
    )
    print(f"  Loaded DFMs: {sorted(exp.dfms.keys())}")
    print(f"  Treatments : {sorted(exp.design.treatments.keys())}")

    # ── Feeding summaries for different windows ───────────────────────────────
    # Mirrors:
    #   fs6  <- Feeding.Summary.Monitors(monitors, p.list, ed, range=c(0,360), ...)
    #   fs18 <- Feeding.Summary.Monitors(monitors, p.list, ed, range=c(360,1440), ...)
    #   fs   <- Feeding.Summary.Monitors(monitors, p.list, ed, ...)
    print("Computing feeding summaries...")
    fs_short = exp.design.feeding_summary(range_minutes=(0.0, short_min), transform_licks=True)
    fs_late  = exp.design.feeding_summary(range_minutes=(late_start, long_min), transform_licks=True)
    fs_long  = exp.design.feeding_summary(range_minutes=(0.0, long_min), transform_licks=True)

    for df_out, name in [
        (fs_short, f"FeedingSummary_{args.hours_short:.0f}hrs_Tf_SP"),
        (fs_late,  f"FeedingSummary_L{args.hours_long - args.hours_short:.0f}hrs_Tf_SP"),
        (fs_long,  f"FeedingSummary_{args.hours_long:.0f}hrs_Tf_SP"),
    ]:
        if not df_out.empty:
            p = out_dir / f"{name}_Data.csv"
            df_out.to_csv(p, index=False)
            print(f"  Saved {p.name}  ({len(df_out)} rows)")

    # ── Raw data plots ────────────────────────────────────────────────────────
    # Mirrors: RawDataPlot.DFM(DFM1, OutputPNGFile=TRUE)
    print("Generating raw data plots...")
    for dfm_id in sorted(exp.dfms.keys()):
        dfm = exp.dfms[dfm_id]
        fig = plot_raw(dfm)
        fn = out_dir / f"RawDataPlot_DFM{dfm_id}.png"
        fig.savefig(fn, dpi=150, bbox_inches="tight")
        plt.close(fig)
        print(f"  {fn.name}")

    # ── Per-well binned lick plots ────────────────────────────────────────────
    # Mirrors the per-well PlotLicksandLight.Well loop:
    #   for (i in 1:8) { for (well in 1:12) { PlotLicksandLight.Well(...) } }
    # Here we plot all wells of each DFM together in one figure (binned).
    print("Generating per-DFM binned lick plots...")
    for dfm_id in sorted(exp.dfms.keys()):
        dfm = exp.dfms[dfm_id]
        fig = plot_binned_licks(dfm, binsize_min=args.binsize_min, transform_licks=False)
        fn = out_dir / f"Rplot_DFM{dfm_id}_LicksNoTF.png"
        fig.savefig(fn, dpi=150, bbox_inches="tight")
        plt.close(fig)
        print(f"  {fn.name}")

    # ── QC reports ────────────────────────────────────────────────────────────
    print("Writing QC reports...")
    qc_dir = out_dir / "qc_reports"
    exp.write_qc_reports(qc_dir)
    exp.write_summary(out_dir / "experiment_summary.txt", include_qc=True)
    print(f"  QC: {qc_dir}")
    print(f"  Summary: {out_dir / 'experiment_summary.txt'}")

    # ── Split Treatment column ────────────────────────────────────────────────
    # Mirrors: fs6 <- separate(fs6, col=Treatment, sep="_", into=c("Treatment","Genotype","Group"))
    # The primary split column; adjust --treatment-parts and --treatment-sep as needed.
    if fs_short.empty:
        print("\nWarning: short-window feeding summary is empty – skipping analysis plots.")
        return 0

    fs_short_sp = separate_treatment(
        fs_short, col="Treatment", sep=args.treatment_sep, into=args.treatment_parts
    )
    fs_long_sp = separate_treatment(
        fs_long, col="Treatment", sep=args.treatment_sep, into=args.treatment_parts
    )

    # The first part after splitting becomes the primary grouping variable.
    x_col = args.treatment_parts[0]
    has_genotype = (
        len(args.treatment_parts) >= 2
        and args.treatment_parts[1] in fs_short_sp.columns
        and fs_short_sp[args.treatment_parts[1]].notna().any()
    )
    geno_col = args.treatment_parts[1] if has_genotype else None

    print(f"\nAnalysis grouping column : '{x_col}'")
    if geno_col:
        print(f"Genotype / facet column  : '{geno_col}'")

    # ── Hedonic Feeding: Sucrose Median Event Duration ─────────────────────────
    # Mirrors:
    #   ggplot(fs6, aes(Treatment, MedDurationA)) + geom_jitter(...) + ...
    if "MedDurationA" in fs_short_sp.columns:
        fig = plot_metric_by_treatment(
            fs_short_sp, x_col=x_col, y_col="MedDurationA",
            title=f"{args.hours_short:.0f}hrs Prog Assay  Sucrose Median Event Duration",
            xlabel="Treatment", ylabel="Median Event Duration (s)",
        )
        fn = out_dir / f"Rplot_{args.hours_short:.0f}hrs_SucroseMedDuration.png"
        fig.savefig(fn, dpi=150, bbox_inches="tight")
        plt.close(fig)
        print(f"  {fn.name}")

        # With genotype faceting
        if geno_col:
            fig = plot_metric_by_treatment(
                fs_short_sp, x_col=x_col, y_col="MedDurationA",
                title=f"{args.hours_short:.0f}hrs Prog Assay  Sucrose MedDuration by Genotype",
                xlabel="Treatment", ylabel="Median Event Duration (s)",
                facet_col=geno_col, figsize=(12, 5),
            )
            fn = out_dir / f"Rplot_{args.hours_short:.0f}hrs_wGeno_SucroseMedDuration.png"
            fig.savefig(fn, dpi=150, bbox_inches="tight")
            plt.close(fig)
            print(f"  {fn.name}")

        if not args.no_stats:
            # Unweighted
            run_anova(
                fs_short_sp, y_col="MedDurationA",
                factors=[x_col],
                label=f"{args.hours_short:.0f}hrs Sucrose MedDuration – unweighted",
            )
            # Weighted by number of events (mirrors aov(..., weights=EventsA))
            if "EventsA" in fs_short_sp.columns:
                run_anova(
                    fs_short_sp, y_col="MedDurationA",
                    factors=[x_col], weights_col="EventsA",
                    label=f"{args.hours_short:.0f}hrs Sucrose MedDuration – weighted by EventsA",
                )

    # ── Sucrose Licks ──────────────────────────────────────────────────────────
    # Mirrors: ggplot(fs, aes(Treatment, LicksA)) + ...
    if "LicksA" in fs_short_sp.columns:
        fig = plot_metric_by_treatment(
            fs_short_sp, x_col=x_col, y_col="LicksA",
            title=f"{args.hours_short:.0f}hrs Prog Assay  Sucrose Licks",
            xlabel="Treatment", ylabel="Licks",
        )
        fn = out_dir / f"Rplot_{args.hours_short:.0f}hrs_SucroseLicks.png"
        fig.savefig(fn, dpi=150, bbox_inches="tight")
        plt.close(fig)
        print(f"  {fn.name}")

        if geno_col:
            fig = plot_metric_by_treatment(
                fs_short_sp, x_col=x_col, y_col="LicksA",
                title=f"{args.hours_short:.0f}hrs Prog Assay  Sucrose Licks by Genotype",
                xlabel="Treatment", ylabel="Licks",
                facet_col=geno_col, figsize=(12, 5),
            )
            fn = out_dir / f"Rplot_{args.hours_short:.0f}hrs_wGeno_SucroseLicks.png"
            fig.savefig(fn, dpi=150, bbox_inches="tight")
            plt.close(fig)
            print(f"  {fn.name}")

        if not args.no_stats:
            run_anova(
                fs_short_sp, y_col="LicksA",
                factors=[x_col],
                label=f"{args.hours_short:.0f}hrs Sucrose Licks – unweighted",
            )
            if geno_col:
                run_anova(
                    fs_short_sp, y_col="LicksA",
                    factors=[x_col, geno_col, f"{x_col}:{geno_col}"],
                    label=f"{args.hours_short:.0f}hrs Sucrose Licks – Genotype interaction",
                )

    # ── Sucrose Events ─────────────────────────────────────────────────────────
    # Mirrors: ggplot(fs, aes(Treatment, EventsA)) + ...
    if "EventsA" in fs_short_sp.columns:
        fig = plot_metric_by_treatment(
            fs_short_sp, x_col=x_col, y_col="EventsA",
            title=f"{args.hours_short:.0f}hrs Prog Assay  Sucrose Events",
            xlabel="Treatment", ylabel="Events",
        )
        fn = out_dir / f"Rplot_{args.hours_short:.0f}hrs_SucroseEvents.png"
        fig.savefig(fn, dpi=150, bbox_inches="tight")
        plt.close(fig)
        print(f"  {fn.name}")

        if geno_col:
            fig = plot_metric_by_treatment(
                fs_short_sp, x_col=x_col, y_col="EventsA",
                title=f"{args.hours_short:.0f}hrs Prog Assay  Sucrose Events by Genotype",
                xlabel="Treatment", ylabel="Events",
                facet_col=geno_col, figsize=(12, 5),
            )
            fn = out_dir / f"Rplot_{args.hours_short:.0f}hrs_wGeno_SucroseEvents.png"
            fig.savefig(fn, dpi=150, bbox_inches="tight")
            plt.close(fig)
            print(f"  {fn.name}")

        if not args.no_stats:
            run_anova(
                fs_short_sp, y_col="EventsA",
                factors=[x_col],
                label=f"{args.hours_short:.0f}hrs Sucrose Events – unweighted",
            )

    # ── Homeostatic Feeding: Sucrose vs. Yeast Events ─────────────────────────
    # Mirrors:
    #   tmp    <- fs[, c("Treatment","Group","EventsB","Experiment")] ; Food <- "Yeast"
    #   s2.data <- fs[, c(...)]; Food <- "S2"
    #   s2.data <- rbind(s2.data, tmp)
    #   ggplot(s2.data, aes(Food, Events)) + facet_wrap("Treatment") + ...
    if "EventsA" in fs_long_sp.columns and "EventsB" in fs_long_sp.columns:
        fig = plot_food_comparison(
            fs_long_sp,
            col_a="EventsA", col_b="EventsB",
            label_a=args.sucrose_label, label_b=args.yeast_label,
            value_label="Events",
            title=f"{args.hours_long:.0f}hrs Prog Assay  {args.sucrose_label} vs {args.yeast_label} Events",
            facet_col=x_col,
        )
        fn = out_dir / f"Rplot_{args.hours_long:.0f}hrs_S2vYeastEvents.png"
        fig.savefig(fn, dpi=150, bbox_inches="tight")
        plt.close(fig)
        print(f"  {fn.name}")

        if not args.no_stats:
            # Build long format for ANOVA (equivalent to R's combined s2.data)
            rows_a = fs_long_sp[[x_col, "EventsA"]].copy()
            rows_a.columns = [x_col, "Events"]
            rows_a["Food"] = args.sucrose_label
            rows_b = fs_long_sp[[x_col, "EventsB"]].copy()
            rows_b.columns = [x_col, "Events"]
            rows_b["Food"] = args.yeast_label
            combined_events = pd.concat([rows_a, rows_b], ignore_index=True)
            run_anova(
                combined_events, y_col="Events",
                factors=[x_col, "Food", f"{x_col}:Food"],
                label=f"{args.hours_long:.0f}hrs {args.sucrose_label} vs {args.yeast_label} Events",
            )

    # ── Homeostatic Feeding: Sucrose vs. Yeast Licks ──────────────────────────
    # Mirrors the 'All Lick Graphs' section of the R script
    if "LicksA" in fs_long_sp.columns and "LicksB" in fs_long_sp.columns:
        fig = plot_food_comparison(
            fs_long_sp,
            col_a="LicksA", col_b="LicksB",
            label_a=args.sucrose_label, label_b=args.yeast_label,
            value_label="Licks",
            title=f"{args.hours_long:.0f}hrs Prog Assay  {args.sucrose_label} vs {args.yeast_label} Licks",
            facet_col=x_col,
        )
        fn = out_dir / f"Rplot_{args.hours_long:.0f}hrs_S2vYeastLicks.png"
        fig.savefig(fn, dpi=150, bbox_inches="tight")
        plt.close(fig)
        print(f"  {fn.name}")

        if not args.no_stats:
            rows_a = fs_long_sp[[x_col, "LicksA"]].copy()
            rows_a.columns = [x_col, "Licks"]
            rows_a["Food"] = args.sucrose_label
            rows_b = fs_long_sp[[x_col, "LicksB"]].copy()
            rows_b.columns = [x_col, "Licks"]
            rows_b["Food"] = args.yeast_label
            combined_licks = pd.concat([rows_a, rows_b], ignore_index=True)
            run_anova(
                combined_licks, y_col="Licks",
                factors=[x_col, "Food", f"{x_col}:Food"],
                label=f"{args.hours_long:.0f}hrs {args.sucrose_label} vs {args.yeast_label} Licks",
            )

    # ── Sucrose vs. Yeast Median Duration ─────────────────────────────────────
    # Mirrors the 'Food Duration Plot' section of the R script
    if "MedDurationA" in fs_short_sp.columns and "MedDurationB" in fs_short_sp.columns:
        fig = plot_food_comparison(
            fs_short_sp,
            col_a="MedDurationA", col_b="MedDurationB",
            label_a=args.sucrose_label, label_b=args.yeast_label,
            value_label="MedDuration",
            title=f"{args.hours_short:.0f}hrs Prog Assay  {args.sucrose_label} vs {args.yeast_label} MedDuration",
            facet_col=x_col,
        )
        fn = out_dir / f"Rplot_{args.hours_short:.0f}hrs_S2vYeastMedDuration.png"
        fig.savefig(fn, dpi=150, bbox_inches="tight")
        plt.close(fig)
        print(f"  {fn.name}")

    # ── Binned feeding summaries across time ──────────────────────────────────
    # Mirrors:
    #   bfs <- BinnedFeeding.Summary.Monitors(monitors, p.list, ed2, binsize.min=30, ...)
    print("\nGenerating binned feeding timecourse plots...")

    # Total licks over 24hrs (equivalent to BinnedSummary_noTf_24hrs)
    fig = exp.plot_binned_licks_by_treatment(
        binsize_min=args.binsize_min,
        range_minutes=(0.0, long_min),
        transform_licks=False,
    )
    fn = out_dir / f"BinnedSummary_noTf_{args.hours_long:.0f}hrs.png"
    fig.savefig(fn, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"  {fn.name}")

    # Short window
    fig = exp.plot_binned_licks_by_treatment(
        binsize_min=args.binsize_min,
        range_minutes=(0.0, short_min),
        transform_licks=False,
    )
    fn = out_dir / f"BinnedSummary_noTf_{args.hours_short:.0f}hrs.png"
    fig.savefig(fn, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"  {fn.name}")

    # Sucrose-side metrics binned over 24hrs (well A only)
    fig = exp.plot_binned_metrics_by_treatment(
        metrics=["LicksA", "EventsA", "MedDurationA"],
        two_well_mode="A",
        binsize_min=args.binsize_min,
        range_minutes=(0.0, long_min),
        transform_licks=False,
        ncols=3,
    )
    fn = out_dir / f"BinnedSummary_SucroseMetrics_{args.hours_long:.0f}hrs.png"
    fig.savefig(fn, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"  {fn.name}")

    # Yeast-side metrics binned over 24hrs (well B only)
    fig = exp.plot_binned_metrics_by_treatment(
        metrics=["LicksB", "EventsB", "MedDurationB"],
        two_well_mode="B",
        binsize_min=args.binsize_min,
        range_minutes=(0.0, long_min),
        transform_licks=False,
        ncols=3,
    )
    fn = out_dir / f"BinnedSummary_YeastMetrics_{args.hours_long:.0f}hrs.png"
    fig.savefig(fn, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"  {fn.name}")

    # PI over time
    fig = exp.plot_binned_metric_by_treatment(
        metric="PI",
        binsize_min=args.binsize_min,
        range_minutes=(0.0, long_min),
        transform_licks=False,
    )
    fn = out_dir / f"BinnedSummary_PI_{args.hours_long:.0f}hrs.png"
    fig.savefig(fn, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"  {fn.name}")

    print(f"\nDone. All outputs written to: {out_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
