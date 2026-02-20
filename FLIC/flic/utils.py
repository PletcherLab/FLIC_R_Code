from __future__ import annotations

import re
from pathlib import Path
from typing import Iterable, Sequence


_nsre = re.compile(r"(\d+)")


def natural_key(s: str) -> list[object]:
    return [int(text) if text.isdigit() else text.lower() for text in _nsre.split(s)]


def natural_sorted(paths: Iterable[Path]) -> list[Path]:
    return sorted(list(paths), key=lambda p: natural_key(p.name))


def range_is_specified(rng: Sequence[float]) -> bool:
    return len(rng) == 2 and (float(rng[0]) + float(rng[1]) != 0.0)

