from __future__ import annotations

from dataclasses import dataclass
from typing import Protocol, Sequence

import numpy as np
import pandas as pd

from .algorithms.events import expand_events, get_events, get_surviving_events, link_events
from .parameters import Parameters


def _well_col(well: int) -> str:
    return f"W{well}"


@dataclass(frozen=True, slots=True)
class FeedingWellResult:
    licks: np.ndarray  # bool
    events: np.ndarray  # int


@dataclass(frozen=True, slots=True)
class TastingWellResult:
    licks: np.ndarray  # bool
    events: np.ndarray  # int


def compute_feeding_for_well(
    baselined_well: np.ndarray,
    thresholds_well: pd.DataFrame,
    params: Parameters,
) -> FeedingWellResult:
    """
    Low-level feeding logic for a single well (port of `Set.Feeding.Data.Well`).
    """

    data = np.asarray(baselined_well, dtype=float)
    feeding_min = data > thresholds_well["FeedingMin"].to_numpy(dtype=float)
    feeding_max = data > thresholds_well["FeedingMax"].to_numpy(dtype=float)

    events = get_surviving_events(feeding_min, feeding_max)
    events[events < params.feeding_minevents] = 0

    licks = expand_events(events)
    bridged = link_events(licks, int(params.feeding_event_link_gap))
    events = get_events(bridged)

    return FeedingWellResult(licks=licks.astype(bool), events=events.astype(int))


def compute_tasting_for_well(
    baselined_well: np.ndarray,
    thresholds_well: pd.DataFrame,
    feeding_licks_well: np.ndarray,
    params: Parameters,
) -> TastingWellResult:
    """
    Low-level tasting logic for a single well (port of `Set.Tasting.Data.Well`).
    """

    data = np.asarray(baselined_well, dtype=float)
    licks = (data > thresholds_well["TastingMin"].to_numpy(dtype=float)) & (
        data < thresholds_well["TastingMax"].to_numpy(dtype=float)
    )
    feeding_licks_well = np.asarray(feeding_licks_well, dtype=bool)
    licks[feeding_licks_well] = False

    events = get_events(licks)
    events[events < params.tasting_minevents] = 0

    return TastingWellResult(licks=licks.astype(bool), events=events.astype(int))


class Chamber(Protocol):
    """
    Base protocol for chambers in a DFM.

    A DFM is composed of either:
    - 12 one-well chambers (chamber_size=1)
    - 6 two-well chambers (chamber_size=2)
    """

    index: int  # 1-based chamber index within the DFM
    dfm_id: int
    params: Parameters
    wells: tuple[int, ...]  # 1-based well numbers within the DFM (len 1 or 2)

    def feeding_well_mapping(self, params: Parameters) -> tuple[int, int] | None:
        """
        For two-well chambers, returns (well_a, well_b) adjusted for PI direction.
        For one-well chambers, returns None.
        """


@dataclass(frozen=True, slots=True)
class OneWellChamber:
    index: int
    dfm_id: int
    params: Parameters
    well: int

    @property
    def wells(self) -> tuple[int, ...]:
        return (self.well,)

    def feeding_well_mapping(self, params: Parameters) -> None:
        return None


@dataclass(frozen=True, slots=True)
class TwoWellChamber:
    index: int
    dfm_id: int
    params: Parameters
    left_well: int
    right_well: int
    well_a: int
    well_b: int

    @property
    def wells(self) -> tuple[int, ...]:
        return (self.left_well, self.right_well)

    def feeding_well_mapping(self, params: Parameters | None = None) -> tuple[int, int]:
        # `params` kept for backward compatibility; mapping is fixed at construction time.
        return (self.well_a, self.well_b)

