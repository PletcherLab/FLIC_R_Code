from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd

from flic import DFM, Parameters


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Load a DFM from the flic/ directory and compute feeding summary + optional QC."
    )
    parser.add_argument(
        "--data-dir",
        default="flic",
        help="Directory containing DFM CSV files (default: %(default)s)",
    )
    parser.add_argument(
        "--dfm-id",
        type=int,
        default=1,
        help="DFM numeric id to load (default: %(default)s)",
    )
    parser.add_argument(
        "--single-well",
        action="store_true",
        help="Use single-well parameter preset instead of two-well.",
    )
    parser.add_argument(
        "--no-transform-licks",
        action="store_true",
        help="Do not apply the ^0.25 lick transform in the summary.",
    )
    parser.add_argument(
        "--out",
        default="",
        help="Optional output CSV path for the summary (default: print only).",
    )
    parser.add_argument(
        "--qc",
        action="store_true",
        help="Run all QC checks (integrity report, data breaks, simultaneous feeding matrix, bleeding check).",
    )
    parser.add_argument(
        "--qc-out-dir",
        default="",
        help="Optional directory to write QC outputs as CSV files (default: print only).",
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
    args = parser.parse_args()

    data_dir = Path(args.data_dir)
    params = Parameters.single_well() if args.single_well else Parameters.two_well()

    dfm = DFM.load(args.dfm_id, params, data_dir=data_dir, range_minutes=(0, 0))
    summary = dfm.feeding_summary(transform_licks=not args.no_transform_licks)

    if args.out:
        out_path = Path(args.out)
        summary.to_csv(out_path, index=False)
        print(f"Wrote {len(summary)} rows to {out_path}")
    else:
        print(summary.to_string(index=False))

    if args.qc:
        out_dir = Path(args.qc_out_dir) if args.qc_out_dir else None
        if out_dir is not None:
            out_dir.mkdir(parents=True, exist_ok=True)

        print("\n--- QC: Integrity report ---")
        integrity = dfm.integrity_report()
        if out_dir is not None:
            pd.DataFrame([integrity]).to_csv(out_dir / f"DFM{dfm.id}_integrity_report.csv", index=False)

        print("\n--- QC: Data breaks ---")
        breaks = dfm.data_breaks(multiplier=float(args.qc_data_breaks_multiplier))
        if breaks is None:
            print("No data breaks found (or Seconds column not present).")
        else:
            print(breaks.head(10).to_string(index=False))
            if out_dir is not None:
                breaks.to_csv(out_dir / f"DFM{dfm.id}_data_breaks.csv", index=False)

        if dfm.params.chamber_size == 2:
            print("\n--- QC: Simultaneous feeding matrix ---")
            mat = dfm.simultaneous_feeding_matrix()
            mat_df = pd.DataFrame(
                mat,
                index=[f"Row{i}" for i in range(1, 7)],
                columns=["Col1", "Col2", "Both", "MaxMin", "HigherInCol1"],
            )
            print(mat_df.to_string())
            if out_dir is not None:
                mat_df.to_csv(out_dir / f"DFM{dfm.id}_simultaneous_feeding_matrix.csv")

            print("\n--- QC: Bleeding check ---")
            bleed = dfm.bleeding_check(cutoff=float(args.qc_bleeding_cutoff))
            matrix_df = bleed["Matrix"]
            all_data = bleed["AllData"]
            print("Matrix (first 5 rows):")
            print(matrix_df.head(5).to_string())
            print("\nAllData:")
            print(all_data.to_string())
            if out_dir is not None:
                matrix_df.to_csv(out_dir / f"DFM{dfm.id}_bleeding_matrix.csv")
                all_data.to_csv(out_dir / f"DFM{dfm.id}_bleeding_alldata.csv", header=True)
        else:
            print("\n--- QC: Simultaneous feeding / bleeding ---")
            print("Skipped (single-well DFM).")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())

