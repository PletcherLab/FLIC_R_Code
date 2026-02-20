from __future__ import annotations

import argparse
from pathlib import Path

from flic import load_experiment_yaml


def main() -> int:
    parser = argparse.ArgumentParser(description="Run an experiment defined by a YAML configuration file.")
    parser.add_argument("config", help="Path to YAML config.")
    parser.add_argument(
        "--out-dir",
        default="",
        help="Optional directory to write outputs (design table + summaries).",
    )
    parser.add_argument(
        "--no-parallel",
        action="store_true",
        help="Disable parallel DFM loading/calculation.",
    )
    parser.add_argument(
        "--executor",
        choices=["threads", "processes"],
        default="threads",
        help="Parallel executor type (default: %(default)s).",
    )
    parser.add_argument(
        "--max-workers",
        type=int,
        default=0,
        help="Max parallel workers (0 means default).",
    )
    args = parser.parse_args()

    exp = load_experiment_yaml(
        args.config,
        parallel=not args.no_parallel,
        executor=args.executor,
        max_workers=None if args.max_workers == 0 else args.max_workers,
    )
    design = exp.design.design_table()
    summary = exp.design.feeding_summary()

    if args.out_dir:
        out = Path(args.out_dir)
        out.mkdir(parents=True, exist_ok=True)
        design.to_csv(out / "experiment_design.csv", index=False)
        summary.to_csv(out / "feeding_summary.csv", index=False)
        print(f"Wrote {len(design)} design rows and {len(summary)} summary rows to {out}")
    else:
        print("=== Experiment design ===")
        print(design.sort_values(["Treatment", "DFM", "Chamber"]).to_string(index=False))
        print("\n=== Feeding summary (first 20 rows) ===")
        if summary.empty:
            print("(empty)")
        else:
            print(summary.head(20).to_string(index=False))

    return 0


if __name__ == "__main__":
    raise SystemExit(main())

