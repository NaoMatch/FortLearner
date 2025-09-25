# -*- coding: utf-8 -*-
"""
Integration tests for FortKMeans using the real shared library.

Skips when the shared library is missing. Keeps assertions on
types/shapes and successful execution rather than numerical equality
to be robust across environments.
"""
from __future__ import annotations

import importlib
import importlib.util
import sys
from pathlib import Path
import ctypes
import os


def _platform_libname() -> str:
    if sys.platform.startswith("win") or os.name == "nt":
        return "libfortlearner.dll"
    if sys.platform == "darwin":
        return "libfortlearner.dylib"
    return "libfortlearner.so"

import numpy as np
import pytest


def _ensure_repo_on_sys_path(module_name: str) -> None:
    """Ensure the repository root is on sys.path so `import flearner...` works.
    Mirrors the helper used in unit tests to be resilient in CI.
    """
    spec = importlib.util.find_spec(module_name)
    if spec is not None:
        return
    # climb upward until we find a directory containing 'flearner'
    here = Path(__file__).resolve()
    for p in [here] + list(here.parents):
        if (p / "flearner").is_dir():
            s = str(p)
            if s not in sys.path:
                sys.path.insert(0, s)
            break
    spec = importlib.util.find_spec(module_name)
    assert spec is not None, f"Cannot import module: {module_name}"


def _ensure_dummy_cerberus_if_missing(monkeypatch):
    try:
        import cerberus  # noqa: F401
        return
    except Exception:
        import types

        dummy = types.ModuleType("cerberus")

        class DummyValidator:
            def __init__(self, schema, allow_unknown=False):  # noqa: D401
                self._errors = {}

            def validate(self, params):
                return True

            @property
            def errors(self):
                return self._errors

        dummy.Validator = DummyValidator
        monkeypatch.setitem(sys.modules, "cerberus", dummy)


def _require_shared_lib():
    so = Path("flearner/lib") / _platform_libname()
    if not so.exists():
        pytest.skip("shared library not found; run `make lib` first")
    # Try to dlopen once to ensure it is actually loadable (deps resolved)
    try:
        ctypes.CDLL(str(so))
    except OSError as e:
        pytest.skip(f"shared library not loadable: {e}")


@pytest.mark.integration
def test_create_and_close_smoke(monkeypatch):
    _require_shared_lib()
    _ensure_repo_on_sys_path("flearner.FortKMeans")
    _ensure_dummy_cerberus_if_missing(monkeypatch)

    from flearner.FortKMeans import FortKMeans

    km = FortKMeans(n_clusters=2, random_state=0)
    # Idempotent close() should not raise
    km.close()
    km.close()


@pytest.mark.integration
def test_fit_predict_score_small(monkeypatch):
    _require_shared_lib()
    _ensure_repo_on_sys_path("flearner.FortKMeans")
    _ensure_dummy_cerberus_if_missing(monkeypatch)

    from flearner.FortKMeans import FortKMeans

    # Small 2-cluster friendly dataset
    X = np.array(
        [
            [0.0, 0.0],
            [0.1, -0.1],
            [0.2, 0.1],
            [5.0, 5.0],
            [5.1, 4.9],
            [4.9, 5.2],
        ],
        dtype=np.float64,
        order="F",
    )

    with FortKMeans(n_clusters=2, random_state=0, max_iter=50) as km:
        km.fit(X)
        labels = km.predict(X)
        score = km.score(X)

    assert isinstance(labels, np.ndarray)
    assert labels.dtype == np.int64 and labels.shape == (X.shape[0], 1)
    assert isinstance(score, float)


@pytest.mark.integration
def test_fit_with_chunk_size_path(monkeypatch):
    _require_shared_lib()
    _ensure_repo_on_sys_path("flearner.FortKMeans")
    _ensure_dummy_cerberus_if_missing(monkeypatch)

    from flearner.FortKMeans import FortKMeans

    # A bit larger to exercise chunked distance path
    rng = np.random.default_rng(0)
    X = np.asfortranarray(rng.normal(size=(64, 8)), dtype=np.float64)

    km = FortKMeans(n_clusters=3, random_state=0, chunk_size=8, max_iter=20)
    km.fit(X)
    labels = km.predict(X)
    s = km.score(X)
    km.close()

    assert labels.shape == (64, 1) and labels.dtype == np.int64
    assert isinstance(s, float)


@pytest.mark.integration
def test_dump_and_load_roundtrip(monkeypatch, tmp_path):
    _require_shared_lib()
    _ensure_repo_on_sys_path("flearner.FortKMeans")
    _ensure_dummy_cerberus_if_missing(monkeypatch)

    from flearner.FortKMeans import FortKMeans

    X = np.asfortranarray([[0.0, 0.0], [1.0, 1.0], [0.9, 1.1], [10.0, -1.0]], dtype=np.float64)

    km = FortKMeans(n_clusters=2, random_state=0, max_iter=30)
    km.fit(X)
    path = tmp_path / "model.bin"
    km.dump(str(path))
    km.close()

    km2 = FortKMeans.load(str(path))
    labels2 = km2.predict(X)
    score2 = km2.score(X)
    km2.close()

    assert labels2.shape == (X.shape[0], 1)
    assert labels2.dtype == np.int64
    assert isinstance(score2, float)


@pytest.mark.integration
def test_load_missing_file_raises(monkeypatch, tmp_path):
    _require_shared_lib()
    _ensure_repo_on_sys_path("flearner.FortKMeans")
    _ensure_dummy_cerberus_if_missing(monkeypatch)

    from flearner.FortKMeans import FortKMeans

    missing = tmp_path / "no_such.bin"
    with pytest.raises(ValueError):
        FortKMeans.load(str(missing))
