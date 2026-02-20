from __future__ import annotations

import numpy as np


def get_events(z: np.ndarray) -> np.ndarray:
    """
    R equivalent: `Get.Events(z)` in `PrivateFunctions.R`.

    Input: boolean vector.
    Output: int vector where the first sample of each TRUE-run is its run length; other samples are 0.
    """

    z = np.asarray(z, dtype=bool)
    n = z.size
    if n == 0:
        return np.array([], dtype=int)

    out = np.zeros(n, dtype=int)
    i = 0
    while i < n:
        if not z[i]:
            i += 1
            continue
        j = i + 1
        while j < n and z[j]:
            j += 1
        out[i] = j - i
        i = j
    return out


def expand_events(eventvec: np.ndarray) -> np.ndarray:
    """
    R equivalent: `Expand.Events(eventvec)`.
    """

    ev = np.asarray(eventvec)
    n = ev.size
    out = np.zeros(n, dtype=bool)
    if n == 0:
        return out
    starts = np.flatnonzero(ev > 0)
    for i in starts:
        L = int(ev[i])
        if L <= 0:
            continue
        out[i : min(n, i + L)] = True
    return out


def get_surviving_events(minvec: np.ndarray, maxvec: np.ndarray) -> np.ndarray:
    """
    R equivalent: `Get.Surviving.Events(minvec, maxvec)`.

    Keep min-threshold events only if at least one sample in that event passes max threshold.
    """

    minvec = np.asarray(minvec, dtype=bool)
    maxvec = np.asarray(maxvec, dtype=bool)
    if minvec.shape != maxvec.shape:
        raise ValueError("minvec and maxvec must have the same shape")

    ev = get_events(minvec)
    out = ev.copy()
    starts = np.flatnonzero(ev > 0)
    n = ev.size
    for i in starts:
        L = int(ev[i])
        if L <= 0:
            continue
        if not np.any(maxvec[i : min(n, i + L)]):
            out[i] = 0
    return out


def link_events(z: np.ndarray, thresh: int) -> np.ndarray:
    """
    R equivalent: `Link.Events(z, thresh)`.

    Bridge FALSE runs of length <= thresh by setting them to TRUE.
    """

    z = np.asarray(z, dtype=bool)
    n = z.size
    if n == 0:
        return z.copy()

    out = z.copy()
    i = 0
    while i < n:
        v = out[i]
        j = i + 1
        while j < n and out[j] == v:
            j += 1
        run_len = j - i
        if (v is False) or (v == False):  # noqa: E712
            if run_len <= thresh:
                out[i:j] = True
        i = j
    return out


def get_intervals(z: np.ndarray) -> np.ndarray:
    """
    R equivalent: `Get.Intervals(z)`.

    Input: boolean vector.
    Output: int vector where the first sample of each FALSE-run is its run length; other samples are 0.
    """

    z = np.asarray(z, dtype=bool)
    n = z.size
    if n == 0:
        return np.array([], dtype=int)
    out = np.zeros(n, dtype=int)
    i = 0
    while i < n:
        if z[i]:
            i += 1
            continue
        j = i + 1
        while j < n and (not z[j]):
            j += 1
        out[i] = j - i
        i = j
    return out

