# -*- coding: utf-8 -*-
"""
1次元・3次元以上の入力は Python 側で即時に ValueError を投げることを確認する。
Fortran 側では 2次元 (n,d) 前提のため、ここで排除する。
"""
import importlib
import importlib.util
import pathlib
import sys
import types

import numpy as np
import pytest

from ...common.fake_ctypes import CFunc, FakeLibBase
from ...common.import_utils import _patch_shared_loaders, _ensure_importable


class FakeLib(FakeLibBase):
    def __init__(self):
        super().__init__()
        self.kmeans_create  = CFunc(lambda *a, **k: 1)
        # 呼ばれてしまったら分かるようにフラグ
        self.called_fit = False
        self.called_predict = False
        self.called_score = False

        def _fit(*args, **kwargs):
            self.called_fit = True
        def _predict(*args, **kwargs):
            self.called_predict = True
        def _score(*args, **kwargs):
            self.called_score = True
            return 0.0

        self.kmeans_fit     = CFunc(_fit)
        self.kmeans_predict = CFunc(_predict)
        self.kmeans_score   = CFunc(_score)
        self.kmeans_free    = CFunc(lambda *a, **k: None)


def _import_kmeans_with_fake_cdll(monkeypatch, fake_lib):
    from ...common.import_utils import _patch_shared_loaders
    _patch_shared_loaders(monkeypatch, fake_lib)

    # cerberus 無い環境を許容
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


@pytest.mark.parametrize("api", ["fit", "predict", "score"])
def test_rejects_1d_input_with_clear_error(km_env, api):
    KMeans = km_env.FortKMeans
    lib = km_env.lib
    km = KMeans(n_clusters=2, random_state=0)

    X1 = np.array([0.0, 1.0, 2.0], dtype=np.float64)
    with pytest.raises(ValueError) as ei:
        getattr(km, api)(X1)
    assert "2D" in str(ei.value) or "2次元" in str(ei.value)

    # C 側は呼ばれていない
    assert not lib.called_fit
    assert not lib.called_predict
    assert not lib.called_score


@pytest.mark.parametrize("api", ["fit", "predict", "score"])
def test_rejects_3d_input_with_clear_error(km_env, api):
    KMeans = km_env.FortKMeans
    lib = km_env.lib
    km = KMeans(n_clusters=2, random_state=0)

    X3 = np.zeros((2, 3, 4), dtype=np.float64)
    with pytest.raises(ValueError) as ei:
        getattr(km, api)(X3)
    assert "2D" in str(ei.value) or "2次元" in str(ei.value)

    # C 側は呼ばれていない
    assert not lib.called_fit
    assert not lib.called_predict
    assert not lib.called_score

