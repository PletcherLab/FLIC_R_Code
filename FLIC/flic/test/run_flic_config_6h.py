from __future__ import annotations

import argparse
from pathlib import Path

import yaml

from flic import Experiment


def _resolve_data_dir(config_path: Path, data_dir_value: str | None) -> Path | None:
    if not data_dir_value:
        return None
    p = Path(data_dir_value)
    if p.is_absolute():
        return p
    # Interpret relative data_dir as relative to the config file directory.
    return (config_path.parent / p).resolve()


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Load flic_config.yaml, restrict to first 6 hours, and write feeding summary to a text file."
    )
    parser.add_argument(
        "--config",
        default=str((Path(__file__).resolve().parents[1] / "flic_config.yaml")),
        help="Path to YAML config (default: flic/flic_config.yaml next to package).",
    )
    parser.add_argument(
        "--hours",
        type=float,
        default=6.0,
        help="Hours of data to include from start (default: %(default)s).",
    )
    parser.add_argument(
        "--out",
        default=str((Path(__file__).resolve().parent / "feeding_summary_first_6h.txt")),
        help="Output text file path (default: flic/test/feeding_summary_first_6h.txt).",
    )
    parser.add_argument(
        "--summary-out",
        default=str((Path(__file__).resolve().parent / "experiment_summary_first_6h.txt")),
        help="Output experiment summary text file path (default: flic/test/experiment_summary_first_6h.txt).",
    )
    parser.add_argument(
        "--qc-out-dir",
        default=str((Path(__file__).resolve().parent / "qc_reports_first_6h")),
        help="Directory to write per-DFM QC outputs (default: flic/test/qc_reports_first_6h).",
    )
    parser.add_argument(
        "--qc-data-breaks-multiplier",
        type=float,
        default=4.0,
        help="Multiplier for data break detection threshold (default: %(default)s).",
    )
    parser.add_argument(
        "--qc-bleeding-cutoff",
        type=float,
        default=50.0,
        help="Cutoff for bleeding check (default: %(default)s).",
    )
    parser.add_argument(
        "--no-parallel",
        action="store_true",
        help="Disable parallel loading/calculation for DFMs.",
    )
    args = parser.parse_args()

    config_path = Path(args.config).resolve()
    if not config_path.exists():
        raise FileNotFoundError(f"Config file not found: {config_path}")

    cfg = yaml.safe_load(config_path.read_text())
    if not isinstance(cfg, dict):
        raise ValueError("Config YAML root must be a mapping/object.")
    data_dir_value = cfg.get("data_dir")
    resolved_data_dir = _resolve_data_dir(config_path, data_dir_value)

    minutes = float(args.hours) * 60.0
    exp = Experiment.load(
        config_path,
        data_dir=resolved_data_dir,
        range_minutes=(0.0, minutes),
        parallel=not args.no_parallel,
        executor="threads",
    )

    exp.write_qc_reports(
        args.qc_out_dir,
        data_breaks_multiplier=float(args.qc_data_breaks_multiplier),
        bleeding_cutoff=float(args.qc_bleeding_cutoff),
    )
    exp.write_summary(
        args.summary_out,
        include_qc=True,
        qc_data_breaks_multiplier=float(args.qc_data_breaks_multiplier),
        qc_bleeding_cutoff=float(args.qc_bleeding_cutoff),
    )

    design_df = exp.design.design_table().sort_values(["Treatment", "DFM", "Chamber"])
    summary_df = exp.design.feeding_summary()
    if not summary_df.empty and all(c in summary_df.columns for c in ("Treatment", "DFM", "Chamber")):
        summary_df = summary_df.sort_values(["Treatment", "DFM", "Chamber"])

    out_path = Path(args.out).resolve()
    out_path.parent.mkdir(parents=True, exist_ok=True)

    with out_path.open("w", encoding="utf-8") as f:
        f.write("FLIC experiment test run\n")
        f.write("=======================\n\n")
        f.write(f"Config: {config_path}\n")
        f.write(f"Config data_dir: {data_dir_value!r}\n")
        f.write(f"Resolved data_dir: {str(resolved_data_dir) if resolved_data_dir else '(none)'}\n")
        f.write(f"Range minutes: (0, {minutes})\n")
        f.write(f"Loaded DFMs: {sorted(exp.dfms.keys())}\n")
        f.write(f"Treatments: {sorted(exp.design.treatments.keys())}\n\n")

        f.write("Experiment design (DFM/Chamber -> Treatment)\n")
        f.write("--------------------------------------------\n")
        if design_df.empty:
            f.write("(empty)\n\n")
        else:
            f.write(design_df.to_string(index=False))
            f.write("\n\n")

        f.write("Feeding summary\n")
        f.write("--------------\n")
        if summary_df.empty:
            f.write("(empty)\n")
        else:
            f.write(summary_df.to_string(index=False))
            f.write("\n")

    print(f"Wrote feeding summary to {out_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

