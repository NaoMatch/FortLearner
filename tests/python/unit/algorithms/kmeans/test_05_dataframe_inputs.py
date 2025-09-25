# -*- coding: utf-8 -*-
"""
pandas / polars の DataFrame をそのまま FortKMeans に渡せることの確認。
- pandas: DataFrame は __array__ / to_numpy を持ち、2Dとして受理される。
- polars: DataFrame は to_numpy を持つ（__array__は未実装の場合あり）。
  FortKMeans 側で .to_numpy() を優先して呼ぶ実装により受理される。
"""
import importlib
import importlib.util
import pathlib
import sys
import types

import numpy as np
import pytest
import ctypes

from ...common.fake_ctypes import CFunc, FakeLibBase
from ...common.import_utils import _patch_shared_loaders, _ensure_importable


class FakeLib(FakeLibBase):
    def __init__(self):
        super().__init__()
        self._next_id = 3000
        self.kmeans_create  = CFunc(self._kmeans_create)
        self.kmeans_fit     = CFunc(self._kmeans_fit)
        self.kmeans_predict = CFunc(self._kmeans_predict)
        self.kmeans_score   = CFunc(self._kmeans_score)
        self.kmeans_free    = CFunc(lambda *a, **k: None)
        self.last_fit = None
        self.last_predict = None
        self.last_score = None

    def _kmeans_create(self, *args):
        hid = self._next_id
        self._next_id += 1
        return hid

    def _kmeans_fit(self, handle, x_ptr, n_rows, n_cols):
        self.last_fit = dict(
            handle=self._as_int(handle), n_rows=self._as_int(n_rows), n_cols=self._as_int(n_cols),
            ptr_is_cdouble=(getattr(x_ptr, "_type_", None) is ctypes.c_double),
        )

    def _kmeans_predict(self, handle, x_ptr, n_rows, n_cols, labels_ptr):
        r = self._as_int(n_rows)
        for i in range(r):
            labels_ptr[i] = i % 2
        self.last_predict = dict(
            handle=self._as_int(handle), n_rows=r, n_cols=self._as_int(n_cols),
            ptr_is_cdouble=(getattr(x_ptr, "_type_", None) is ctypes.c_double),
        )

    def _kmeans_score(self, handle, x_ptr, n_rows, n_cols):
        val = float(self._as_int(n_rows))
        self.last_score = dict(
            handle=self._as_int(handle), n_rows=self._as_int(n_rows), n_cols=self._as_int(n_cols),
            ptr_is_cdouble=(getattr(x_ptr, "_type_", None) is ctypes.c_double),
            score=val,
        )
        return val


def _import_kmeans_with_fake_cdll(monkeypatch, fake_lib):
    _patch_shared_loaders(monkeypatch, fake_lib)
    # cerberus ダミー挿入（未インストール環境対策）
    try:
        import cerberus  # noqa: F401
    except Exception:
        dummy = types.ModuleType("cerberus")
        class DummyValidator:
            def __init__(self, schema, allow_unknown=False): self._errors = {}
            def validate(self, params): return True
            @property
            def errors(self): return self._errors
        dummy.Validator = DummyValidator
        monkeypatch.setitem(sys.modules, "cerberus", dummy)

    for name in ("flearner.fort_base_estimator", "flearner.handle_owner",
                 "flearner.FortKMeans", "flearner.FortKMeans.FortKMeans"):
        sys.modules.pop(name, None)

    _ensure_importable("flearner.FortKMeans")
    mod = importlib.import_module("flearner.FortKMeans")

    KMeans = getattr(mod, "FortKMeans", None)
    if KMeans is None and hasattr(mod, "__dict__"):
        sub = mod.__dict__.get("FortKMeans")
        if sub and hasattr(sub, "FortKMeans"):
            KMeans = sub.FortKMeans
    assert KMeans is not None, "FortKMeans クラスが見つかりません"
    return KMeans, mod


@pytest.fixture()
def km_env(monkeypatch):
    fake = FakeLib()
    KMeans, mod = _import_kmeans_with_fake_cdll(monkeypatch, fake)
    return types.SimpleNamespace(FortKMeans=KMeans, module=mod, lib=fake)


def test_accepts_pandas_dataframe(km_env):
    pd = pytest.importorskip("pandas")
    KMeans = km_env.FortKMeans
    lib = km_env.lib

    df = pd.DataFrame({
        "x": [0, 1, 2, 3, 4],
        "y": [5, 6, 7, 8, 9],
        "z": [1, 2, 3, 4, 5],
    })

    km = KMeans(n_clusters=2, random_state=0)
    km.fit(df)
    assert lib.last_fit["n_rows"] == 5 and lib.last_fit["n_cols"] == 3
    assert lib.last_fit["ptr_is_cdouble"]

    y = km.predict(df)
    assert y.shape == (5, 1) and y.dtype == np.int64
    assert lib.last_predict["ptr_is_cdouble"]

    s = km.score(df)
    assert isinstance(s, float)
    assert lib.last_score["ptr_is_cdouble"]


def test_accepts_polars_dataframe(km_env):
    pl = pytest.importorskip("polars")
    KMeans = km_env.FortKMeans
    lib = km_env.lib

    df = pl.DataFrame({
        "x": [0, 1, 2, 3, 4],
        "y": [5, 6, 7, 8, 9],
        "z": [1, 2, 3, 4, 5],
    })

    km = KMeans(n_clusters=2, random_state=0)
    km.fit(df)
    assert lib.last_fit["n_rows"] == 5 and lib.last_fit["n_cols"] == 3
    assert lib.last_fit["ptr_is_cdouble"]

    y = km.predict(df)
    assert y.shape == (5, 1) and y.dtype == np.int64
    assert lib.last_predict["ptr_is_cdouble"]

    s = km.score(df)
    assert isinstance(s, float)
    assert lib.last_score["ptr_is_cdouble"]
