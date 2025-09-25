# -*- coding: utf-8 -*-
"""
Float32 / 各種 Int / Cオーダー入力の受け入れ確認（FortKMeans の入口での安全な正規化）

- FortLearner は Float64 + Fオーダーを前提とするが、FortKMeans は入口で
  np.asfortranarray(..., dtype=float64) による正規化を行う。
- 本テストでは、様々な dtype・order の入力で fit/predict/score が例外なく動作し、
  かつ C 側には float64 のポインタ（POINTER(c_double)）が渡っていることを確認する。
"""
import ctypes
import importlib
import importlib.util
import pathlib
import sys
import types

import numpy as np
import pytest
from ctypes import c_int64, c_double, POINTER


from ...common.fake_ctypes import CFunc, FakeLibBase
from ...common.import_utils import _patch_shared_loaders, _ensure_importable


class FakeLib(FakeLibBase):
    def __init__(self):
        super().__init__()
        self._next_id = 2000
        self.created_ids = []
        self.last_fit = None
        self.last_predict = None
        self.last_score = None

        self.kmeans_create  = CFunc(self._kmeans_create)
        self.kmeans_fit     = CFunc(self._kmeans_fit)
        self.kmeans_predict = CFunc(self._kmeans_predict)
        self.kmeans_score   = CFunc(self._kmeans_score)
        self.kmeans_free    = CFunc(self._kmeans_free)

    def _kmeans_create(self, *args):
        hid = self._next_id
        self._next_id += 1
        self.created_ids.append(hid)
        return hid

    def _kmeans_fit(self, handle, x_ptr, n_rows, n_cols):
        # x_ptr は POINTER(c_double) が渡るはず
        tp_ok = getattr(x_ptr, "_type_", None) is ctypes.c_double
        self.last_fit = dict(
            handle=self._as_int(handle),
            n_rows=self._as_int(n_rows),
            n_cols=self._as_int(n_cols),
            ptr_is_cdouble=tp_ok,
        )

    def _kmeans_predict(self, handle, x_ptr, n_rows, n_cols, labels_ptr):
        # 0/1 を交互に詰める
        r = self._as_int(n_rows)
        for i in range(r):
            labels_ptr[i] = i % 2
        tp_ok = getattr(x_ptr, "_type_", None) is ctypes.c_double
        self.last_predict = dict(handle=self._as_int(handle), n_rows=r, n_cols=self._as_int(n_cols), ptr_is_cdouble=tp_ok)

    def _kmeans_score(self, handle, x_ptr, n_rows, n_cols):
        r = self._as_int(n_rows)
        val = float(r)
        tp_ok = getattr(x_ptr, "_type_", None) is ctypes.c_double
        self.last_score = dict(handle=self._as_int(handle), n_rows=r, n_cols=self._as_int(n_cols), ptr_is_cdouble=tp_ok, score=val)
        return val

    def _kmeans_free(self, handle):
        self.freed.append(self._as_int(handle))


def _import_kmeans_with_fake_cdll(monkeypatch, fake_lib):
    """共有ライブラリローダを Fake に置換し、FortKMeans を import する。"""
    _patch_shared_loaders(monkeypatch, fake_lib)

    # cerberus（無い環境もある）をダミーで供給
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

    # 新鮮な import にする
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


DTYPES = [
    np.float32,
    np.int8, np.int16, np.int32, np.int64,
    np.uint8, np.uint16, np.uint32, np.uint64,
]

ORDERS = ["C", "F"]


@pytest.mark.parametrize("dtype", DTYPES)
@pytest.mark.parametrize("order", ORDERS)
def test_accepts_various_dtypes_and_orders_in_fit_predict_score(km_env, dtype, order):
    KMeans = km_env.FortKMeans
    lib = km_env.lib
    km = KMeans(n_clusters=2, random_state=0)

    # 小さめの整数/浮動値で、全 dtype で安全に表現可能な値にする
    base = np.arange(15, dtype=np.int64).reshape(5, 3) % 7
    if np.issubdtype(dtype, np.integer):
        X = base.astype(dtype, copy=True, order=order)
    else:
        X = base.astype(dtype, copy=True, order=order) * (1.0 / 3.0)

    # fit: 例外なく動作し、形状とポインタ型が期待どおり
    km.fit(X)
    assert lib.last_fit["n_rows"] == 5 and lib.last_fit["n_cols"] == 3
    assert lib.last_fit["ptr_is_cdouble"], "X は float64 へ正規化されているべき"

    # predict: 返却は int64 の (n,1)
    y = km.predict(X)
    assert isinstance(y, np.ndarray)
    assert y.dtype == np.int64
    assert y.shape == (5, 1)
    assert lib.last_predict["ptr_is_cdouble"], "X は float64 へ正規化されているべき"

    # score: Python float を返し、ポインタ型は c_double
    s = km.score(X)
    assert isinstance(s, float)
    assert lib.last_score["ptr_is_cdouble"], "X は float64 へ正規化されているべき"

