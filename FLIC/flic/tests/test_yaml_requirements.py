from __future__ import annotations

from pathlib import Path

import pytest

from flic import load_experiment_yaml


def _write(tmp_path: Path, name: str, text: str) -> Path:
    p = tmp_path / name
    p.write_text(text)
    return p


def test_yaml_requires_params_somewhere(tmp_path: Path):
    yml = _write(
        tmp_path,
        "bad.yaml",
        """
data_dir: flic
global: {}
dfms:
  1:
    chambers: {1: A}
""".lstrip(),
    )
    with pytest.raises(ValueError, match=r"must define a `params` section"):
        load_experiment_yaml(yml)


def test_yaml_requires_chamber_size(tmp_path: Path):
    yml = _write(
        tmp_path,
        "bad2.yaml",
        """
data_dir: flic
global:
  params: {feeding_threshold: 10}
dfms:
  1:
    params: {pi_multiplier: 1}
    chambers: {1: A}
""".lstrip(),
    )
    with pytest.raises(ValueError, match=r"chamber_size.*explicitly specified"):
        load_experiment_yaml(yml)


def test_yaml_global_params_applied_and_dfm_overrides(tmp_path: Path):
    yml = _write(
        tmp_path,
        "ok.yaml",
        """
data_dir: flic
global:
  params:
    chamber_size: 2
    feeding_threshold: 10
dfms:
  1:
    params:
      pi_direction: right
    chambers:
      1: A
      2: B
""".lstrip(),
    )
    exp = load_experiment_yaml(yml)
    assert 1 in exp.design.dfms
    dfm1 = exp.design.dfms[1]
    assert dfm1.params.chamber_size == 2
    assert dfm1.params.feeding_threshold == 10
    assert dfm1.params.pi_direction == "right"


def test_yaml_pi_direction_validation(tmp_path: Path):
    yml = _write(
        tmp_path,
        "bad_pi.yaml",
        """
data_dir: flic
global:
  params:
    chamber_size: 2
dfms:
  1:
    params: {pi_direction: sideways}
    chambers: {1: A}
""".lstrip(),
    )
    with pytest.raises(ValueError, match=r"pi_direction must be 'left' or 'right'"):
        load_experiment_yaml(yml)


def test_yaml_pi_multiplier_backcompat_numeric(tmp_path: Path):
    yml = _write(
        tmp_path,
        "old_pi.yaml",
        """
data_dir: flic
global:
  params:
    chamber_size: 2
dfms:
  1:
    params: {pi_multiplier: 2}
    chambers: {1: A}
""".lstrip(),
    )
    exp = load_experiment_yaml(yml)
    assert exp.design.dfms[1].params.pi_direction == "right"

