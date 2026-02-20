from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, Sequence

import pandas as pd

from .dfm import DFM
from .treatment import Treatment, TreatmentChamber


@dataclass(slots=True)
class ExperimentDesign:
    """
    A mapping of treatment-name -> Treatment, representing an experiment's chamber assignments.
    """

    dfms: dict[int, DFM] = field(default_factory=dict)
    treatments: dict[str, Treatment] = field(default_factory=dict)

    def add_dfm(self, dfm: DFM, *, overwrite: bool = False) -> None:
        dfm_id = int(dfm.id)
        if (dfm_id in self.dfms) and not overwrite:
            raise ValueError(f"DFM {dfm_id} already exists in design (use overwrite=True to replace).")
        self.dfms[dfm_id] = dfm

    def add_treatment(self, treatment: Treatment, *, overwrite: bool = False) -> None:
        if (treatment.name in self.treatments) and not overwrite:
            raise ValueError(f"Treatment '{treatment.name}' already exists (use overwrite=True to replace).")
        self.treatments[treatment.name] = treatment

    def remove_treatment(self, name: str) -> None:
        self.treatments.pop(name)

    def get(self, name: str) -> Treatment:
        return self.treatments[name]

    def __iter__(self):
        return iter(self.treatments.values())

    def design_table(self) -> pd.DataFrame:
        """
        Return a table like the R `expDesign` data frame: columns DFM, Chamber, Treatment.
        """

        rows: list[dict[str, object]] = []
        for name, trt in self.treatments.items():
            for tc in trt.chambers:
                rows.append({"DFM": tc.dfm_id, "Chamber": tc.chamber_index, "Treatment": name})
        return pd.DataFrame(rows)

    def feeding_summary(
        self, *, range_minutes: Sequence[float] = (0, 0), transform_licks: bool = True
    ) -> pd.DataFrame:
        """
        Concatenate per-treatment feeding summaries into one DataFrame.
        """

        frames: list[pd.DataFrame] = []
        for trt in self.treatments.values():
            df = trt.feeding_summary(range_minutes=range_minutes, transform_licks=transform_licks)
            if not df.empty:
                frames.append(df)
        return pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()

