# tests/python/unit/algorithms/kmeans/test_02_edges_and_concurrency.py
# -*- coding: utf-8 -*-
"""
B8: エッジ形状（n=0, d=1）
B9: 非数値入力のキャスト例外
C12: 並行 close() の安全性（freeは1回）
- PyPI / ローカル両対応のモジュール解決
- 共有ライブラリのローダ（CDLL/LoadLibrary/PyDLL/numpy.ctypeslib.load_library）を import 前に Fake に差し替え
"""
import ctypes
import importlib
import importlib.util
import pathlib
import sys
import types
import threading
import pytest
import numpy as np
from ctypes import c_int64, c_double, POINTER


from ...common.fake_ctypes import CFunc, FakeLibBase
from ...common.import_utils import _patch_shared_loaders, _ensure_importable

class FakeLib(FakeLibBase):
    def __init__(self):
        super().__init__()
        self._next_id = 1000
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
        self.last_fit = dict(
            handle=self._as_int(handle),
            n_rows=self._as_int(n_rows),
            n_cols=self._as_int(n_cols),
        )

    def _kmeans_predict(self, handle, x_ptr, n_rows, n_cols, labels_ptr):
        r = self._as_int(n_rows)
        for i in range(r):
            labels_ptr[i] = i % 2
        self.last_predict = dict(handle=self._as_int(handle), n_rows=r, n_cols=self._as_int(n_cols))

    def _kmeans_score(self, handle, x_ptr, n_rows, n_cols):
        r = self._as_int(n_rows)
        val = -float(r)
        self.last_score = dict(handle=self._as_int(handle), n_rows=r, n_cols=self._as_int(n_cols), score=val)
        return val

    def _kmeans_free(self, handle):
        self.freed.append(self._as_int(handle))


# ---- 共有ライブラリローダの広域パッチ（import前に必須） ---------------------
def _import_kmeans_with_fake_cdll(monkeypatch, fake_lib):
    _patch_shared_loaders(monkeypatch, fake_lib)

    # cerberus が無い環境でも通す（型はここでは正しい値を渡す前提）
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

    # FortKMeans クラスの位置を吸収（単一ファイル版/パッケージ版双方）
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


# =============================== B8: エッジ形状 ===============================
def test_fit_predict_score_edge_shapes(km_env):
    """
    - n=0（ゼロ行）でも predict が (0,1) int64 を返す
    - d=1（1列）でも predict が (n,1) int64
    - score は Python float
    """
    KMeans = km_env.FortKMeans
    lib = km_env.lib
    km = KMeans(n_clusters=2, random_state=0)

    # n=0
    X0 = np.empty((0, 2), dtype=np.float64, order="F")
    km.fit(X0)
    assert lib.last_fit["n_rows"] == 0 and lib.last_fit["n_cols"] == 2

    y0 = km.predict(X0)
    assert isinstance(y0, np.ndarray)
    assert y0.dtype == np.int64
    assert y0.shape == (0, 1)

    s0 = km.score(X0)
    assert isinstance(s0, float)

    # d=1
    X1 = np.array([[0.0], [1.0], [2.0]], dtype=np.float64, order="F")
    km.fit(X1)
    assert lib.last_fit["n_rows"] == 3 and lib.last_fit["n_cols"] == 1

    y1 = km.predict(X1)
    assert isinstance(y1, np.ndarray)
    assert y1.dtype == np.int64
    assert y1.shape == (3, 1)

    s1 = km.score(X1)
    assert isinstance(s1, float)


# =============================== B9: 非数値入力 ===============================
@pytest.mark.parametrize("api", ["fit", "predict", "score"])
def test_non_numeric_raises_from_numpy_cast(km_env, api):
    """
    非数値データは np.asfortranarray(..., float64) の段階で TypeError/ValueError
    """
    KMeans = km_env.FortKMeans
    km = KMeans(n_clusters=2, random_state=0)

    X_bad = np.array([["a", "b"]], dtype=object)
    with pytest.raises((TypeError, ValueError)):
        getattr(km, api)(X_bad)


# =============================== C12: 並行 close ==============================
def test_concurrent_close_is_thread_safe(km_env):
    """
    10スレッド同時 close() でも free はちょうど1回
    """
    KMeans = km_env.FortKMeans
    lib = km_env.lib
    km = KMeans(n_clusters=2, random_state=0)
    mid = km._model_id

    barrier = threading.Barrier(11)
    threads = []
    for _ in range(10):
        t = threading.Thread(target=lambda: (barrier.wait(), km.close()))
        t.start()
        threads.append(t)

    barrier.wait()
    for t in threads:
        t.join()

    # free は1回だけ
    assert lib.freed.count(mid) == 1

    # 以後、_h 参照は RuntimeError（HandleOwner の仕様）
    with pytest.raises(RuntimeError):
        _ = km._h
