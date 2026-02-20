"""
FLIC Config Editor
==================
PyQt6 GUI for creating and editing ``flic_config.yaml`` experiment configuration files.

Usage (command line)::

    python -m flic.config_editor

Usage (Python)::

    from flic.config_editor import launch
    launch()
"""
from __future__ import annotations

import re
import sys
from pathlib import Path
from typing import Any

import yaml
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QAction
from PyQt6.QtWidgets import (
    QApplication,
    QCheckBox,
    QComboBox,
    QFileDialog,
    QFormLayout,
    QGroupBox,
    QHBoxLayout,
    QHeaderView,
    QLabel,
    QLineEdit,
    QMainWindow,
    QMessageBox,
    QPushButton,
    QScrollArea,
    QSizePolicy,
    QSpinBox,
    QSplitter,
    QTabWidget,
    QTableWidget,
    QTableWidgetItem,
    QVBoxLayout,
    QWidget,
)

# ---------------------------------------------------------------------------
# Parameter metadata
# ---------------------------------------------------------------------------

_PARAM_DEFAULTS: dict[int, dict[str, Any]] = {
    1: {  # single_well — mirrors Parameters.single_well()
        "baseline_window_minutes": 3,
        "feeding_threshold": 20,
        "feeding_minimum": 10,
        "tasting_minimum": 5,
        "tasting_maximum": 20,
        "feeding_minevents": 1,
        "tasting_minevents": 1,
        "samples_per_second": 5,
        "feeding_event_link_gap": 5,
        "pi_direction": "left",
        "correct_for_dual_feeding": False,
    },
    2: {  # two_well — mirrors Parameters.two_well()
        "baseline_window_minutes": 3,
        "feeding_threshold": 20,
        "feeding_minimum": 10,
        "tasting_minimum": 5,
        "tasting_maximum": 20,
        "feeding_minevents": 1,
        "tasting_minevents": 1,
        "samples_per_second": 5,
        "feeding_event_link_gap": 5,
        "pi_direction": "left",
        "correct_for_dual_feeding": True,
    },
}

_PARAM_LABELS: dict[str, str] = {
    "baseline_window_minutes": "Baseline Window (min)",
    "feeding_threshold": "Feeding Threshold",
    "feeding_minimum": "Feeding Minimum",
    "tasting_minimum": "Tasting Minimum",
    "tasting_maximum": "Tasting Maximum",
    "feeding_minevents": "Feeding Min Events",
    "tasting_minevents": "Tasting Min Events",
    "samples_per_second": "Samples / Second",
    "feeding_event_link_gap": "Event Link Gap (samples)",
    "pi_direction": "PI Direction (side with PI = 1)",
    "correct_for_dual_feeding": "Correct for Dual Feeding",
}

_PARAM_ORDER: list[str] = [
    "baseline_window_minutes",
    "feeding_threshold",
    "feeding_minimum",
    "tasting_minimum",
    "tasting_maximum",
    "feeding_minevents",
    "tasting_minevents",
    "samples_per_second",
    "feeding_event_link_gap",
    "pi_direction",
    "correct_for_dual_feeding",
]

# ---------------------------------------------------------------------------
# Widget helpers
# ---------------------------------------------------------------------------


def _make_param_widget(key: str, default: Any) -> QWidget:
    """Return an appropriate input widget for the given parameter key."""
    if key == "pi_direction":
        w = QComboBox()
        w.addItems(["left", "right"])
        w.setCurrentText(str(default))
        w.setFixedWidth(75)
        return w
    if key == "correct_for_dual_feeding":
        w = QCheckBox()
        w.setChecked(bool(default))
        return w
    # All numeric parameters use integer spinboxes.
    w = QSpinBox()
    w.setFixedWidth(75)
    if key == "samples_per_second":
        w.setRange(1, 1000)
    elif key == "baseline_window_minutes":
        w.setRange(1, 60)
    elif key in ("feeding_threshold", "feeding_minimum", "tasting_minimum", "tasting_maximum"):
        w.setRange(0, 100000)
    else:
        w.setRange(0, 10000)
    w.setValue(int(default))
    return w


def _get_param_value(widget: QWidget) -> Any:
    """Read the current value from a parameter input widget."""
    if isinstance(widget, QComboBox):
        return widget.currentText()
    if isinstance(widget, QCheckBox):
        return widget.isChecked()
    if isinstance(widget, QSpinBox):
        return widget.value()
    return None


def _set_param_value(widget: QWidget, value: Any) -> None:
    """Set the value of a parameter input widget."""
    if isinstance(widget, QComboBox):
        idx = widget.findText(str(value))
        if idx >= 0:
            widget.setCurrentIndex(idx)
    elif isinstance(widget, QCheckBox):
        widget.setChecked(bool(value))
    elif isinstance(widget, QSpinBox):
        widget.setValue(int(round(float(value))))


# ---------------------------------------------------------------------------
# ParamsForm
# ---------------------------------------------------------------------------


class ParamsForm(QWidget):
    """
    A QFormLayout-based widget for all non-chamber_size Parameters fields.

    In *global* mode (``override_mode=False``): every field is shown enabled
    and always included in ``get_values()``.

    In *override* mode (``override_mode=True``): each row has an enable
    checkbox; only checked rows are returned by ``get_values()``.
    """

    def __init__(
        self,
        *,
        override_mode: bool = False,
        chamber_size: int = 2,
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)
        self._override_mode = override_mode
        self._input_widgets: dict[str, QWidget] = {}
        self._enable_checks: dict[str, QCheckBox] = {}
        self._two_well_rows: list[tuple[QFormLayout, int]] = []

        defaults = _PARAM_DEFAULTS.get(chamber_size, _PARAM_DEFAULTS[2])

        # Split parameters evenly into three columns (4 + 4 + 3 for 11 params).
        n = len(_PARAM_ORDER)
        s1 = (n + 2) // 3
        columns = [_PARAM_ORDER[:s1], _PARAM_ORDER[s1 : 2 * s1], _PARAM_ORDER[2 * s1 :]]

        outer = QHBoxLayout(self)
        outer.setSpacing(20)

        for col_keys in columns:
            form = QFormLayout()
            form.setFieldGrowthPolicy(QFormLayout.FieldGrowthPolicy.ExpandingFieldsGrow)
            form.setLabelAlignment(Qt.AlignmentFlag.AlignRight)
            form.setHorizontalSpacing(8)
            form.setVerticalSpacing(2)

            for key in col_keys:
                label = _PARAM_LABELS[key]
                widget = _make_param_widget(key, defaults[key])
                self._input_widgets[key] = widget

                row_idx = form.rowCount()
                if key in ("pi_direction", "correct_for_dual_feeding"):
                    self._two_well_rows.append((form, row_idx))

                if override_mode:
                    cb = QCheckBox()
                    cb.setChecked(False)
                    cb.toggled.connect(lambda checked, w=widget: w.setEnabled(checked))
                    widget.setEnabled(False)
                    self._enable_checks[key] = cb

                    row = QWidget()
                    rl = QHBoxLayout(row)
                    rl.setContentsMargins(0, 0, 0, 0)
                    rl.setSpacing(4)
                    rl.addWidget(cb)
                    rl.addWidget(widget)
                    rl.addStretch()
                    form.addRow(label, row)
                else:
                    form.addRow(label, widget)

            outer.addLayout(form, stretch=1)

        self.set_chamber_size(chamber_size)

    # ------------------------------------------------------------------

    def get_values(self, *, include_disabled: bool = False) -> dict[str, Any]:
        """
        Return a dict of param_name -> value.

        In override mode, unchecked rows are omitted unless ``include_disabled=True``.
        """
        out: dict[str, Any] = {}
        for key, w in self._input_widgets.items():
            if self._override_mode and not include_disabled:
                cb = self._enable_checks.get(key)
                if cb is not None and not cb.isChecked():
                    continue
            out[key] = _get_param_value(w)
        return out

    def load_values(self, values: dict[str, Any], chamber_size: int = 2) -> None:
        """
        Populate the form from a dict.  Keys present in *values* are set and
        (in override mode) their checkboxes are enabled.  Missing keys are
        reset to defaults and unchecked.
        """
        defaults = _PARAM_DEFAULTS.get(chamber_size, _PARAM_DEFAULTS[2])
        for key in _PARAM_ORDER:
            w = self._input_widgets.get(key)
            if w is None:
                continue
            cb = self._enable_checks.get(key)
            if key in values:
                _set_param_value(w, values[key])
                if cb is not None:
                    cb.setChecked(True)
                    w.setEnabled(True)
            else:
                _set_param_value(w, defaults[key])
                if cb is not None:
                    cb.setChecked(False)
                    w.setEnabled(False)

    def reset_defaults(self, chamber_size: int) -> None:
        """Reset all inputs to the factory defaults for *chamber_size*."""
        defaults = _PARAM_DEFAULTS.get(chamber_size, _PARAM_DEFAULTS[2])
        for key in _PARAM_ORDER:
            w = self._input_widgets.get(key)
            if w is not None:
                _set_param_value(w, defaults[key])
            cb = self._enable_checks.get(key)
            if cb is not None:
                cb.setChecked(False)
                w.setEnabled(False)

    def set_chamber_size(self, chamber_size: int) -> None:
        """Show or hide rows that are only meaningful for two-well chambers."""
        show = chamber_size == 2
        for form, row_idx in self._two_well_rows:
            form.setRowVisible(row_idx, show)


# ---------------------------------------------------------------------------
# DFMWidget
# ---------------------------------------------------------------------------


class DFMWidget(QWidget):
    """Configuration widget for a single DFM (one tab in the DFM tab widget)."""

    def __init__(
        self,
        dfm_id: int,
        chamber_size: int = 2,
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)
        self._chamber_size = chamber_size

        outer = QVBoxLayout(self)
        outer.setAlignment(Qt.AlignmentFlag.AlignTop)

        # -- DFM ID --------------------------------------------------------
        id_group = QGroupBox("DFM Identity")
        id_layout = QHBoxLayout(id_group)
        id_layout.addWidget(QLabel("DFM ID:"))
        self._id_spin = QSpinBox()
        self._id_spin.setRange(1, 99)
        self._id_spin.setValue(dfm_id)
        self._id_spin.setMaximumWidth(80)
        id_layout.addWidget(self._id_spin)
        id_layout.addStretch()
        outer.addWidget(id_group)

        # -- Chamber → Treatment table -------------------------------------
        n_chambers = 12 if chamber_size == 1 else 6
        ch_group = QGroupBox("Chamber → Treatment Assignments")
        ch_layout = QVBoxLayout(ch_group)
        hint = QLabel(
            "Leave a Treatment cell blank to omit that chamber from the config."
        )
        hint.setStyleSheet("color: gray; font-size: 11px;")
        ch_layout.addWidget(hint)
        self._chamber_table = _build_chamber_table(n_chambers)
        ch_layout.addWidget(self._chamber_table)
        outer.addWidget(ch_group)

        # -- Parameter overrides -------------------------------------------
        over_group = QGroupBox(
            "Parameter Overrides  (check a box to override the global value)"
        )
        over_layout = QVBoxLayout(over_group)
        self._params_form = ParamsForm(override_mode=True, chamber_size=chamber_size)
        over_layout.addWidget(self._params_form)
        outer.addWidget(over_group)

    # ------------------------------------------------------------------

    def update_chamber_size(self, chamber_size: int) -> None:
        """Resize the chamber table for a new chamber_size, preserving treatment text."""
        self._chamber_size = chamber_size
        n_new = 12 if chamber_size == 1 else 6
        n_old = self._chamber_table.rowCount()

        # Preserve existing treatment strings
        old_treatments: dict[int, str] = {}
        for i in range(n_old):
            item = self._chamber_table.item(i, 1)
            old_treatments[i + 1] = item.text() if item else ""

        self._chamber_table.setRowCount(n_new)
        self._chamber_table.setMinimumHeight(_chamber_table_min_height(n_new))
        for i in range(n_new):
            ch_num = i + 1
            ch_item = QTableWidgetItem(str(ch_num))
            ch_item.setFlags(ch_item.flags() & ~Qt.ItemFlag.ItemIsEditable)
            self._chamber_table.setItem(i, 0, ch_item)
            trt = old_treatments.get(ch_num, "")
            self._chamber_table.setItem(i, 1, QTableWidgetItem(trt))

        self._params_form.set_chamber_size(chamber_size)

    def get_dict(self) -> dict[str, Any]:
        """Return a YAML-ready dict for this DFM."""
        result: dict[str, Any] = {"id": self._id_spin.value()}

        overrides = self._params_form.get_values()
        if overrides:
            result["params"] = overrides

        chambers: dict[int, str] = {}
        for i in range(self._chamber_table.rowCount()):
            item = self._chamber_table.item(i, 1)
            trt = item.text().strip() if item else ""
            if trt:
                chambers[i + 1] = trt
        if chambers:
            result["chambers"] = chambers

        return result

    def load_dict(self, data: dict[str, Any], chamber_size: int) -> None:
        """Populate from a DFM node dict (as parsed from YAML)."""
        self._id_spin.setValue(int(data.get("id", self._id_spin.value())))

        params_raw = data.get("params", data.get("parameters", {})) or {}
        self._params_form.load_values(dict(params_raw), chamber_size)

        # Chamber assignments
        chambers_raw = data.get("chambers", data.get("Chambers", {})) or {}
        if isinstance(chambers_raw, dict):
            assignments = {int(k): str(v) for k, v in chambers_raw.items()}
        elif isinstance(chambers_raw, list):
            assignments = {int(it["index"]): str(it["treatment"]) for it in chambers_raw}
        else:
            assignments = {}

        for i in range(self._chamber_table.rowCount()):
            trt = assignments.get(i + 1, "")
            self._chamber_table.setItem(i, 1, QTableWidgetItem(trt))


def _sanitize_treatment(text: str) -> str:
    """Spaces → underscores; strip everything that isn't alphanumeric or underscore."""
    return re.sub(r"[^A-Za-z0-9_]", "", text.replace(" ", "_"))


def _chamber_table_min_height(n_chambers: int) -> int:
    """Minimum height (px) to show all rows without a scrollbar."""
    return n_chambers * 26 + 32  # ~26 px/row + header


def _build_chamber_table(n_chambers: int) -> QTableWidget:
    table = QTableWidget(n_chambers, 2)
    table.setHorizontalHeaderLabels(["Chamber", "Treatment"])
    table.horizontalHeader().setSectionResizeMode(
        0, QHeaderView.ResizeMode.ResizeToContents
    )
    table.horizontalHeader().setSectionResizeMode(1, QHeaderView.ResizeMode.Stretch)
    table.verticalHeader().setVisible(False)
    table.setAlternatingRowColors(True)
    table.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
    table.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
    table.setMinimumHeight(_chamber_table_min_height(n_chambers))
    for i in range(n_chambers):
        ch_item = QTableWidgetItem(str(i + 1))
        ch_item.setFlags(ch_item.flags() & ~Qt.ItemFlag.ItemIsEditable)
        table.setItem(i, 0, ch_item)
        table.setItem(i, 1, QTableWidgetItem(""))

    def _on_item_changed(item: QTableWidgetItem) -> None:
        if item.column() != 1:
            return
        raw = item.text()
        clean = _sanitize_treatment(raw)
        if clean != raw:
            table.blockSignals(True)
            item.setText(clean)
            table.blockSignals(False)

    table.itemChanged.connect(_on_item_changed)
    return table


# ---------------------------------------------------------------------------
# Main window
# ---------------------------------------------------------------------------


class FLICConfigEditor(QMainWindow):
    """Main window for the FLIC Config Editor."""

    def __init__(self) -> None:
        super().__init__()
        self._current_path: Path | None = None
        self._dfm_widgets: list[DFMWidget] = []

        self.setWindowTitle("FLIC Config Editor")
        self.resize(920, 960)

        self._build_menu()
        self._build_ui()

    # ------------------------------------------------------------------
    # Menu
    # ------------------------------------------------------------------

    def _build_menu(self) -> None:
        menubar = self.menuBar()
        file_menu = menubar.addMenu("&File")

        new_act = QAction("&New", self)
        new_act.setShortcut("Ctrl+N")
        new_act.triggered.connect(self._new)
        file_menu.addAction(new_act)

        open_act = QAction("&Open…", self)
        open_act.setShortcut("Ctrl+O")
        open_act.triggered.connect(self._open)
        file_menu.addAction(open_act)

        file_menu.addSeparator()

        save_act = QAction("&Save", self)
        save_act.setShortcut("Ctrl+S")
        save_act.triggered.connect(self._save)
        file_menu.addAction(save_act)

        saveas_act = QAction("Save &As…", self)
        saveas_act.setShortcut("Ctrl+Shift+S")
        saveas_act.triggered.connect(self._save_as)
        file_menu.addAction(saveas_act)

        file_menu.addSeparator()

        exit_act = QAction("E&xit", self)
        exit_act.setShortcut("Ctrl+Q")
        exit_act.triggered.connect(self.close)
        file_menu.addAction(exit_act)

    # ------------------------------------------------------------------
    # UI construction
    # ------------------------------------------------------------------

    def _build_ui(self) -> None:
        # Use a vertical splitter so users can drag to give DFM tabs more space.
        splitter = QSplitter(Qt.Orientation.Vertical)
        self.setCentralWidget(splitter)

        # ---- Top pane: Experiment Settings + Global Parameters ----------
        top_widget = QWidget()
        top_layout = QVBoxLayout(top_widget)
        top_layout.setAlignment(Qt.AlignmentFlag.AlignTop)
        top_layout.setSpacing(8)
        top_layout.setContentsMargins(6, 6, 6, 6)

        # Experiment Settings
        exp_group = QGroupBox("Experiment Settings")
        exp_form = QFormLayout(exp_group)
        exp_form.setFieldGrowthPolicy(QFormLayout.FieldGrowthPolicy.AllNonFixedFieldsGrow)
        exp_form.setLabelAlignment(Qt.AlignmentFlag.AlignRight)

        dir_row = QWidget()
        dir_layout = QHBoxLayout(dir_row)
        dir_layout.setContentsMargins(0, 0, 0, 0)
        self._data_dir_edit = QLineEdit()
        self._data_dir_edit.setPlaceholderText("e.g. ./flic  (relative to config file)")
        browse_btn = QPushButton("Browse…")
        browse_btn.setMaximumWidth(90)
        browse_btn.clicked.connect(self._browse_data_dir)
        dir_layout.addWidget(self._data_dir_edit, stretch=1)
        dir_layout.addWidget(browse_btn)
        exp_form.addRow("Data Directory:", dir_row)

        self._chamber_size_combo = QComboBox()
        self._chamber_size_combo.addItems(["1  (single-well, 12 chambers)", "2  (two-well, 6 chambers)"])
        self._chamber_size_combo.setCurrentIndex(1)
        self._chamber_size_combo.setMaximumWidth(240)
        self._chamber_size_combo.currentIndexChanged.connect(self._on_chamber_size_changed)
        exp_form.addRow("Chamber Size:", self._chamber_size_combo)

        self._num_dfms_spin = QSpinBox()
        self._num_dfms_spin.setRange(1, 20)
        self._num_dfms_spin.setValue(1)
        self._num_dfms_spin.setMaximumWidth(80)
        self._num_dfms_spin.valueChanged.connect(self._on_num_dfms_changed)
        exp_form.addRow("Number of DFMs:", self._num_dfms_spin)

        top_layout.addWidget(exp_group)

        # Global Parameters
        global_group = QGroupBox("Global Parameters  (applied to all DFMs unless overridden)")
        global_inner = QVBoxLayout(global_group)
        global_inner.setContentsMargins(8, 8, 8, 8)
        self._global_params = ParamsForm(override_mode=False, chamber_size=2)
        global_inner.addWidget(self._global_params)
        top_layout.addWidget(global_group)

        splitter.addWidget(top_widget)

        # ---- Bottom pane: DFM Tabs --------------------------------------
        dfm_group = QGroupBox("DFM Configuration")
        dfm_group_layout = QVBoxLayout(dfm_group)
        dfm_group_layout.setContentsMargins(6, 6, 6, 6)
        self._dfm_tabs = QTabWidget()
        dfm_group_layout.addWidget(self._dfm_tabs)
        splitter.addWidget(dfm_group)

        # Initial split: ~300px for settings, rest for DFM tabs.
        splitter.setSizes([300, 620])
        splitter.setStretchFactor(0, 0)
        splitter.setStretchFactor(1, 1)

        # Initialise with one DFM tab
        self._sync_dfm_tabs(1, 2)

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------

    def _chamber_size(self) -> int:
        """Return the currently selected chamber_size (1 or 2)."""
        return int(self._chamber_size_combo.currentText().split()[0])

    def _on_chamber_size_changed(self) -> None:
        cs = self._chamber_size()
        self._global_params.reset_defaults(cs)
        self._global_params.set_chamber_size(cs)
        for w in self._dfm_widgets:
            w.update_chamber_size(cs)

    def _on_num_dfms_changed(self, n: int) -> None:
        self._sync_dfm_tabs(n, self._chamber_size())

    def _sync_dfm_tabs(self, n: int, chamber_size: int) -> None:
        """Ensure exactly *n* DFM tabs exist, adding or removing without data loss."""
        current = len(self._dfm_widgets)
        if n < current:
            for _ in range(current - n):
                self._dfm_tabs.removeTab(self._dfm_tabs.count() - 1)
                w = self._dfm_widgets.pop()
                w.deleteLater()
        elif n > current:
            for i in range(current, n):
                dfm_id = i + 1
                w = DFMWidget(dfm_id=dfm_id, chamber_size=chamber_size)
                self._dfm_widgets.append(w)
                tab_scroll = QScrollArea()
                tab_scroll.setWidgetResizable(True)
                tab_scroll.setWidget(w)
                self._dfm_tabs.addTab(tab_scroll, f"DFM {dfm_id}")

    def _browse_data_dir(self) -> None:
        start = str(Path.cwd())
        d = QFileDialog.getExistingDirectory(self, "Select Data Directory", start)
        if d:
            self._data_dir_edit.setText(d)

    # ------------------------------------------------------------------
    # YAML serialisation / deserialisation
    # ------------------------------------------------------------------

    def _collect_yaml(self) -> dict[str, Any]:
        """Build a YAML-ready dict from the current UI state."""
        cfg: dict[str, Any] = {}

        data_dir = self._data_dir_edit.text().strip()
        if data_dir:
            cfg["data_dir"] = data_dir

        # Global params (always include chamber_size)
        global_params = self._global_params.get_values()
        global_params["chamber_size"] = self._chamber_size()
        cfg["global"] = {"params": global_params}

        cfg["dfms"] = [w.get_dict() for w in self._dfm_widgets]
        return cfg

    def _populate_from_yaml(self, cfg: dict[str, Any]) -> None:
        """Populate all widgets from a parsed YAML dict."""
        self._data_dir_edit.setText(str(cfg.get("data_dir", "")))

        # Determine chamber_size from global params
        global_cfg = cfg.get("global", {}) or {}
        global_params_raw = global_cfg.get("params", global_cfg.get("parameters", {})) or {}
        chamber_size = int(global_params_raw.get("chamber_size", 2))

        # Update chamber_size combo without triggering the rebuild signal yet
        idx = 0 if chamber_size == 1 else 1
        self._chamber_size_combo.blockSignals(True)
        self._chamber_size_combo.setCurrentIndex(idx)
        self._chamber_size_combo.blockSignals(False)

        # Global params (exclude chamber_size — it's in the combo)
        params_to_load = {k: v for k, v in global_params_raw.items() if k != "chamber_size"}
        self._global_params.load_values(params_to_load, chamber_size)

        # DFM nodes
        dfm_nodes = cfg.get("dfms", cfg.get("DFMs", [])) or []
        if isinstance(dfm_nodes, dict):
            items: list[dict] = []
            for k, v in dfm_nodes.items():
                node = dict(v)
                node.setdefault("id", int(k))
                items.append(node)
            dfm_nodes = items

        n = max(1, len(dfm_nodes))
        self._num_dfms_spin.blockSignals(True)
        self._num_dfms_spin.setValue(n)
        self._num_dfms_spin.blockSignals(False)
        self._sync_dfm_tabs(n, chamber_size)

        for i, node in enumerate(dfm_nodes):
            if i < len(self._dfm_widgets):
                self._dfm_widgets[i].load_dict(node, chamber_size)
                dfm_id = int(node.get("id", i + 1))
                self._dfm_tabs.setTabText(i, f"DFM {dfm_id}")

    # ------------------------------------------------------------------
    # File operations
    # ------------------------------------------------------------------

    def _new(self) -> None:
        self._current_path = None
        self.setWindowTitle("FLIC Config Editor")
        self._data_dir_edit.clear()

        self._chamber_size_combo.blockSignals(True)
        self._num_dfms_spin.blockSignals(True)
        self._chamber_size_combo.setCurrentIndex(1)  # two-well
        self._num_dfms_spin.setValue(1)
        self._chamber_size_combo.blockSignals(False)
        self._num_dfms_spin.blockSignals(False)

        # Remove all DFM tabs
        self._dfm_tabs.clear()
        for w in self._dfm_widgets:
            w.deleteLater()
        self._dfm_widgets.clear()

        self._global_params.reset_defaults(2)
        self._sync_dfm_tabs(1, 2)

    def _open(self) -> None:
        path, _ = QFileDialog.getOpenFileName(
            self,
            "Open Config",
            str(Path.cwd()),
            "YAML files (*.yaml *.yml);;All files (*)",
        )
        if not path:
            return
        try:
            cfg = yaml.safe_load(Path(path).read_text(encoding="utf-8"))
            if not isinstance(cfg, dict):
                raise ValueError("File does not contain a YAML mapping.")
            self._current_path = Path(path)
            self.setWindowTitle(f"FLIC Config Editor — {self._current_path.name}")
            self._populate_from_yaml(cfg)
        except Exception as exc:
            QMessageBox.critical(self, "Error", f"Failed to load config:\n{exc}")

    def _save(self) -> None:
        if self._current_path is None:
            self._save_as()
        else:
            self._write_yaml(self._current_path)

    def _save_as(self) -> None:
        default = str(self._current_path or (Path.cwd() / "flic_config.yaml"))
        path, _ = QFileDialog.getSaveFileName(
            self,
            "Save Config As",
            default,
            "YAML files (*.yaml *.yml);;All files (*)",
        )
        if not path:
            return
        self._current_path = Path(path)
        self.setWindowTitle(f"FLIC Config Editor — {self._current_path.name}")
        self._write_yaml(self._current_path)

    def _write_yaml(self, path: Path) -> None:
        try:
            cfg = self._collect_yaml()
            path.write_text(
                yaml.dump(cfg, default_flow_style=False, allow_unicode=True, sort_keys=False),
                encoding="utf-8",
            )
            QMessageBox.information(self, "Saved", f"Config saved to:\n{path}")
        except Exception as exc:
            QMessageBox.critical(self, "Error", f"Failed to save config:\n{exc}")


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def launch() -> None:
    """Launch the FLIC Config Editor GUI."""
    app = QApplication.instance() or QApplication(sys.argv)
    win = FLICConfigEditor()
    win.show()
    sys.exit(app.exec())


if __name__ == "__main__":
    launch()
