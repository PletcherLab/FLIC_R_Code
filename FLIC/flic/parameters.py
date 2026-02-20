from __future__ import annotations

from dataclasses import dataclass, replace
from typing import Literal

import numpy as np


def _default_two_well_chamber_sets() -> np.ndarray:
    # R: matrix(1:12, ncol=2, byrow=TRUE) → (1,2),(3,4),...,(11,12)
    return np.array([(1, 2), (3, 4), (5, 6), (7, 8), (9, 10), (11, 12)], dtype=int)


def _default_single_well_chamber_sets() -> np.ndarray:
    return np.arange(1, 13, dtype=int).reshape(-1, 1)


@dataclass(frozen=True, slots=True)
class Parameters:
    """
    Port of `ParametersClass()` from `ParametersClass.R`.

    Field names are pythonic; behavior matches the R implementation.
    """

    baseline_window_minutes: float = 3
    feeding_threshold: float = 10
    feeding_minimum: float = 10
    tasting_minimum: float = 10
    tasting_maximum: float = 20
    feeding_minevents: int = 1
    tasting_minevents: int = 1
    samples_per_second: float = 5
    feeding_event_link_gap: int = 5
    chamber_sets: np.ndarray = None  # shape (n_chambers, chamber_size)
    chamber_size: int = 2
    correct_for_dual_feeding: bool = False
    pi_direction: Literal["left", "right"] = "left"

    def __post_init__(self) -> None:
        cs = self.chamber_size
        if cs not in (1, 2):
            raise ValueError("Only chamber_size 1 or 2 is supported by the port.")

        if self.pi_direction not in ("left", "right"):
            raise ValueError("pi_direction must be 'left' or 'right'.")

        if self.chamber_sets is None:
            object.__setattr__(
                self,
                "chamber_sets",
                _default_two_well_chamber_sets()
                if cs == 2
                else _default_single_well_chamber_sets(),
            )

        arr = np.asarray(self.chamber_sets, dtype=int)
        if arr.ndim != 2:
            raise ValueError("chamber_sets must be a 2D array-like.")
        if arr.shape[1] != cs:
            raise ValueError(f"chamber_sets must have {cs} columns for chamber_size={cs}.")
        if np.any(arr < 1) or np.any(arr > 12):
            raise ValueError("chamber_sets values must be between 1 and 12 (inclusive).")
        object.__setattr__(self, "chamber_sets", arr)

    @classmethod
    def single_well(cls) -> Parameters:
        # Mirrors ParametersClass.SingleWell
        return cls(
            baseline_window_minutes=3,
            feeding_threshold=20,
            feeding_minimum=10,
            tasting_minimum=5,
            tasting_maximum=20,
            feeding_minevents=1,
            tasting_minevents=1,
            samples_per_second=5,
            feeding_event_link_gap=5,
            chamber_size=1,
            chamber_sets=_default_single_well_chamber_sets(),
            pi_direction="left",
            correct_for_dual_feeding=False,
        )

    @classmethod
    def two_well(cls) -> Parameters:
        # Mirrors ParametersClass.TwoWell (and close to ParametersClass())
        return cls(
            baseline_window_minutes=3,
            feeding_threshold=20,
            feeding_minimum=10,
            tasting_minimum=5,
            tasting_maximum=20,
            feeding_minevents=1,
            tasting_minevents=1,
            samples_per_second=5,
            feeding_event_link_gap=5,
            chamber_size=2,
            chamber_sets=_default_two_well_chamber_sets(),
            pi_direction="left",
            correct_for_dual_feeding=True,
        )

    def with_updates(self, **kwargs) -> Parameters:
        return replace(self, **kwargs)

    def parameter_vector(self) -> np.ndarray:
        """
        Mirrors the purpose of `GetParameterVector()` in R.

        Note: order is stable but is a Python definition, not R `unlist()` order.
        """

        flat_sets = self.chamber_sets.reshape(-1)
        # Keep a numeric encoding for direction for downstream numeric-only outputs:
        # left -> 1, right -> 2 (roughly analogous to prior pi_multiplier usage).
        pi_dir_num = 1.0 if self.pi_direction == "left" else 2.0
        return np.array(
            [
                self.baseline_window_minutes,
                self.feeding_threshold,
                self.feeding_minimum,
                self.tasting_minimum,
                self.tasting_maximum,
                self.feeding_minevents,
                self.tasting_minevents,
                self.samples_per_second,
                self.chamber_size,
                *flat_sets.tolist(),
                self.feeding_event_link_gap,
                pi_dir_num,
                int(self.correct_for_dual_feeding),
            ],
            dtype=float,
        )

    def parameter_names(self) -> list[str]:
        chambernames = [f"Ch{i}" for i in range(1, self.chamber_sets.shape[0] + 1)]
        return [
            "BaselineWindowMin",
            "FeedingThreshold",
            "FeedingMinimum",
            "TastingLow",
            "TastingHigh",
            "FeedingMinEvents",
            "TastingMinEvents",
            "SamplesSec",
            "ChamberSize",
            *chambernames,
            "Link.Gap",
            "PI.Direction",
            "Correct.For.Dual.Feeding",
        ]

