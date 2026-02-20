import numpy as np

from flic.algorithms.events import (
    expand_events,
    get_events,
    get_intervals,
    get_surviving_events,
    link_events,
)


def test_get_events_example_from_r_comment():
    z = np.array([True, True, False, False, True, False, True, True, True])
    ev = get_events(z)
    assert ev.tolist() == [2, 0, 0, 0, 1, 0, 3, 0, 0]


def test_expand_events_example_from_r_comment():
    ev = np.array([2, 0, 0, 0, 1, 0, 3, 0, 0])
    z = expand_events(ev)
    assert z.tolist() == [True, True, False, False, True, False, True, True, True]


def test_get_surviving_events_example_from_r_comment():
    z = np.array([True, True, False, False, True, False, True, True, True])
    zz = np.array([False, True, False, False, False, False, False, False, True])
    ev = get_surviving_events(z, zz)
    assert ev.tolist() == [2, 0, 0, 0, 0, 0, 3, 0, 0]


def test_link_events_example_from_r_comment():
    z = np.array([True, True, False, False, False, True, False, True, True, True])
    linked = link_events(z, thresh=3)
    # Note: the R implementation bridges FALSE runs of length <= thresh.
    assert linked.tolist() == [True, True, True, True, True, True, True, True, True, True]


def test_get_intervals_example_from_r_comment():
    z = np.array([True, True, False, False, True, False, True, True, True])
    ints = get_intervals(z)
    assert ints.tolist() == [0, 0, 2, 0, 0, 1, 0, 0, 0]

