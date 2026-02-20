from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable, Literal, Mapping, Sequence

import numpy as np
import yaml

from .dfm import DFM
from .experiment import Experiment
from .experiment_design import ExperimentDesign
from .parameters import Parameters
from .treatment import Treatment


def _norm_key(k: str) -> str:
    k = str(k).strip()
    k = k.replace(".", "_").replace("-", "_").replace(" ", "_")
    while "__" in k:
        k = k.replace("__", "_")
    return k.lower()


_PARAM_ALIASES: dict[str, str] = {
    # Baseline
    "baseline_window_minutes": "baseline_window_minutes",
    "baseline_window_min": "baseline_window_minutes",
    "baseline_window": "baseline_window_minutes",
    # Feeding
    "feeding_threshold": "feeding_threshold",
    "feeding_minimum": "feeding_minimum",
    "feeding_minevents": "feeding_minevents",
    "feeding_event_link_gap": "feeding_event_link_gap",
    "link_gap": "feeding_event_link_gap",
    # Tasting
    "tasting_minimum": "tasting_minimum",
    "tasting_maximum": "tasting_maximum",
    "tasting_low": "tasting_minimum",
    "tasting_high": "tasting_maximum",
    "tasting_minevents": "tasting_minevents",
    # Hardware / chamber
    "samples_per_second": "samples_per_second",
    "samples_per_sec": "samples_per_second",
    "chamber_sets": "chamber_sets",
    "chamber_size": "chamber_size",
    "correct_for_dual_feeding": "correct_for_dual_feeding",
    "pi_direction": "pi_direction",
    # Back-compat: accept numeric `pi_multiplier` and interpret 1->left, other->right.
    "pi_multiplier": "pi_direction",
}


def _normalize_param_overrides(overrides: Mapping[str, Any] | None) -> dict[str, Any]:
    if not overrides:
        return {}
    out: dict[str, Any] = {}
    for k, v in overrides.items():
        nk = _norm_key(k)
        nk = _PARAM_ALIASES.get(nk, nk)
        out[nk] = v

    # Coercions
    if "chamber_sets" in out and out["chamber_sets"] is not None:
        out["chamber_sets"] = np.asarray(out["chamber_sets"], dtype=int)
    if "chamber_size" in out and out["chamber_size"] is not None:
        out["chamber_size"] = int(out["chamber_size"])
    if "feeding_minevents" in out and out["feeding_minevents"] is not None:
        out["feeding_minevents"] = int(out["feeding_minevents"])
    if "tasting_minevents" in out and out["tasting_minevents"] is not None:
        out["tasting_minevents"] = int(out["tasting_minevents"])
    if "pi_direction" in out and out["pi_direction"] is not None:
        v = out["pi_direction"]
        # If legacy numeric was supplied under `pi_multiplier`, it arrives here via alias mapping.
        if isinstance(v, (int, float)):
            out["pi_direction"] = "left" if int(v) == 1 else "right"
        else:
            s = str(v).strip().lower()
            if s not in ("left", "right"):
                raise ValueError("pi_direction must be 'left' or 'right'.")
            out["pi_direction"] = s
    if "correct_for_dual_feeding" in out and out["correct_for_dual_feeding"] is not None:
        out["correct_for_dual_feeding"] = bool(out["correct_for_dual_feeding"])
    return out


def _parse_chamber_assignments(chambers_node: Any) -> dict[int, str]:
    """
    Accept either:
    - mapping: {1: "DrugA", 2: "Vehicle"}
    - list: [{index: 1, treatment: "DrugA"}, ...]
    """

    if chambers_node is None:
        return {}
    if isinstance(chambers_node, Mapping):
        return {int(k): str(v) for k, v in chambers_node.items()}
    if isinstance(chambers_node, list):
        out: dict[int, str] = {}
        for item in chambers_node:
            if not isinstance(item, Mapping):
                raise ValueError("Each chambers[] entry must be a mapping with index and treatment.")
            idx = int(item.get("index"))
            trt = str(item.get("treatment"))
            out[idx] = trt
        return out
    raise ValueError("chambers must be a mapping or a list of {index,treatment}.")


def _load_dfm_for_config(
    dfm_id: int, params: Parameters, data_dir: str | Path, range_minutes: Sequence[float]
) -> DFM:
    # Separate top-level helper so process pools can pickle it.
    return DFM.load(dfm_id, params, data_dir=data_dir, range_minutes=range_minutes)


def load_experiment_yaml(
    path: str | Path,
    *,
    data_dir: str | Path | None = None,
    range_minutes: Sequence[float] = (0, 0),
    parallel: bool = True,
    max_workers: int | None = None,
    executor: Literal["threads", "processes"] = "threads",
) -> Experiment:
    """
    Load an experiment from a YAML configuration file.

    Expected YAML structure:

    ```yaml
    global:
      params:
        chamber_size: 2
    data_dir: flic
    dfms:
      - id: 1
        params: { feeding_threshold: 20 }
        chambers:
          1: DrugA
          2: Vehicle
      - id: 5
        params: {}
        chambers:
          - index: 1
            treatment: DrugA
    ```
    """

    from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor, as_completed

    path = Path(path)
    cfg = yaml.safe_load(path.read_text())
    if not isinstance(cfg, Mapping):
        raise ValueError("YAML root must be a mapping/object.")

    global_cfg = dict(cfg.get("global", {}) or {})
    global_params_node = global_cfg.get("params", global_cfg.get("parameters", None))
    global_overrides = _normalize_param_overrides(global_params_node)
    global_params_present = global_params_node is not None
    default_data_dir = cfg.get("data_dir", None)
    if data_dir is None:
        data_dir = default_data_dir if default_data_dir is not None else "."
    data_dir = Path(data_dir)

    dfm_nodes = cfg.get("dfms", cfg.get("DFMs", None))
    if dfm_nodes is None:
        raise ValueError("YAML must include `dfms:`.")

    # Support either:
    # - list form (requires '-' entries)
    # - mapping/dict form (no '-' entries), keyed by DFM id
    if isinstance(dfm_nodes, Mapping):
        items = []
        for k, v in dfm_nodes.items():
            if not isinstance(v, Mapping):
                raise ValueError("Each dfms{...} value must be an object/mapping.")
            node = dict(v)
            node.setdefault("id", int(k))
            items.append(node)
        dfm_nodes = items
    elif not isinstance(dfm_nodes, list):
        raise ValueError("`dfms` must be either a list or a mapping keyed by DFM id.")

    design = ExperimentDesign()

    # First pass: compute parameters and capture chamber assignments, but don't load data yet.
    dfm_specs: list[tuple[int, Parameters, dict[int, str]]] = []
    for node in dfm_nodes:
        if not isinstance(node, Mapping):
            raise ValueError("Each dfms[] entry must be an object/mapping.")
        dfm_id = int(node.get("id", node.get("ID")))

        dfm_params_node = node.get("params", node.get("parameters", None))
        dfm_overrides = _normalize_param_overrides(dfm_params_node)
        dfm_params_present = dfm_params_node is not None

        if not (global_params_present or dfm_params_present):
            raise ValueError(
                f"DFM {dfm_id} must define a `params` section either under global: or under the DFM entry."
            )

        # Precedence: defaults < global < dfm
        overrides = {**global_overrides, **dfm_overrides}

        if "chamber_size" not in overrides or overrides["chamber_size"] is None:
            raise ValueError(
                f"DFM {dfm_id}: `chamber_size` must be explicitly specified in params (global or dfm)."
            )

        # Choose a base preset if chamber_size is specified; otherwise default to two-well.
        base_size = int(overrides["chamber_size"])
        if base_size not in (1, 2):
            raise ValueError(f"DFM {dfm_id}: unsupported chamber_size={base_size} (expected 1 or 2).")
        base = Parameters.single_well() if base_size == 1 else Parameters.two_well()
        params = base.with_updates(**overrides)
        chamber_assignments = _parse_chamber_assignments(node.get("chambers", node.get("Chambers")))
        dfm_specs.append((dfm_id, params, chamber_assignments))

    loaded: dict[int, DFM] = {}
    if parallel and len(dfm_specs) > 1:
        if executor not in ("threads", "processes"):
            raise ValueError(f"executor must be 'threads' or 'processes', got {executor!r}")
        Exec = ThreadPoolExecutor if executor == "threads" else ProcessPoolExecutor
        with Exec(max_workers=max_workers) as pool:
            futs = {
                pool.submit(_load_dfm_for_config, dfm_id, params, data_dir, range_minutes): dfm_id
                for dfm_id, params, _ in dfm_specs
            }
            first_error: RuntimeError | None = None
            first_exc: BaseException | None = None
            for fut in as_completed(futs):
                dfm_id = futs[fut]
                try:
                    loaded[dfm_id] = fut.result()
                except Exception as e:  # noqa: BLE001
                    first_error = RuntimeError(f"Failed to load DFM {dfm_id} from YAML config.")
                    first_exc = e
                    # Cancel any futures that haven't started yet, then stop waiting.
                    for f in futs:
                        f.cancel()
                    break
        if first_error is not None:
            if first_exc is not None:
                raise first_error from first_exc
            raise first_error
    else:
        for dfm_id, params, _ in dfm_specs:
            loaded[dfm_id] = _load_dfm_for_config(dfm_id, params, data_dir, range_minutes)

    # Store DFMs and assign chambers to treatments.
    for dfm_id, _, chamber_assignments in dfm_specs:
        dfm = loaded[dfm_id]
        design.add_dfm(dfm, overwrite=True)
        for chamber_index, treatment_name in chamber_assignments.items():
            if treatment_name not in design.treatments:
                design.add_treatment(Treatment(treatment_name))
            design.treatments[treatment_name].add_chamber(dfm, chamber_index)

    # Keep `Experiment.dfms` as a convenience alias to the same dict.
    return Experiment(
        dfms=design.dfms,
        design=design,
        global_config=global_cfg,
        config_path=path.resolve(),
        data_dir=Path(data_dir).resolve(),
        range_minutes=(float(range_minutes[0]), float(range_minutes[1])),
        parallel=bool(parallel),
        executor=executor,
        max_workers=max_workers,
    )

