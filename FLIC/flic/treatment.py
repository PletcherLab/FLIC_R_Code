from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, Protocol, Sequence, overload

import pandas as pd

from .chamber import OneWellChamber, TwoWellChamber


class DFMLike(Protocol):
    id: int
    chambers: list[OneWellChamber | TwoWellChamber]

    def feeding_summary(self, *, range_minutes: Sequence[float] = (0, 0), transform_licks: bool = True) -> pd.DataFrame:
        ...


@dataclass(frozen=True, slots=True)
class TreatmentChamber:
    """
    A reference to a specific chamber within a specific DFM.
    """

    dfm: DFMLike
    chamber: OneWellChamber | TwoWellChamber

    @property
    def dfm_id(self) -> int:
        return int(self.dfm.id)

    @property
    def chamber_index(self) -> int:
        return int(self.chamber.index)


@dataclass(slots=True)
class Treatment:
    """
    A treatment group: chambers pooled across DFMs.
    """

    name: str
    chambers: list[TreatmentChamber] = field(default_factory=list)

    def add_chamber(self, dfm: DFMLike, chamber_index: int) -> None:
        match = next((c for c in dfm.chambers if int(c.index) == int(chamber_index)), None)
        if match is None:
            raise ValueError(f"DFM{dfm.id} has no chamber with index={chamber_index}")
        self.chambers.append(TreatmentChamber(dfm=dfm, chamber=match))

    def add_all_chambers(self, dfm: DFMLike) -> None:
        for c in dfm.chambers:
            self.chambers.append(TreatmentChamber(dfm=dfm, chamber=c))

    def add_chambers(self, dfm: DFMLike, chamber_indices: Iterable[int]) -> None:
        idxs = set(int(x) for x in chamber_indices)
        for c in dfm.chambers:
            if int(c.index) in idxs:
                self.chambers.append(TreatmentChamber(dfm=dfm, chamber=c))

    def feeding_summary(
        self, *, range_minutes: Sequence[float] = (0, 0), transform_licks: bool = True
    ) -> pd.DataFrame:
        """
        Concatenate feeding summaries across DFMs, keeping only the chambers in this treatment.

        Returns a DataFrame with a `Treatment` column prepended.
        """

        if not self.chambers:
            return pd.DataFrame()

        frames: list[pd.DataFrame] = []
        # Group selection by DFM to avoid recomputing summaries repeatedly
        by_dfm: dict[int, list[int]] = {}
        dfm_objs: dict[int, DFMLike] = {}
        for tc in self.chambers:
            by_dfm.setdefault(tc.dfm_id, []).append(tc.chamber_index)
            dfm_objs[tc.dfm_id] = tc.dfm

        for dfm_id, chamber_idxs in by_dfm.items():
            dfm = dfm_objs[dfm_id]
            summ = dfm.feeding_summary(range_minutes=range_minutes, transform_licks=transform_licks)
            if summ.empty:
                continue
            # Both one-well and two-well summaries use `Chamber` as the per-chamber identifier.
            if "Chamber" in summ.columns:
                summ = summ[summ["Chamber"].astype(int).isin(set(int(x) for x in chamber_idxs))]
            frames.append(summ)

        if not frames:
            return pd.DataFrame()

        out = pd.concat(frames, ignore_index=True)
        out.insert(0, "Treatment", self.name)
        return out

