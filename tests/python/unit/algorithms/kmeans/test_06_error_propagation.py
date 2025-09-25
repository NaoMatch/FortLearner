# -*- coding: utf-8 -*-
"""
例外伝搬（libエラーの拾い上げ）

fit/predict/score それぞれで、C側が get_last_error に非0コードとメッセージを返す状況を
FakeLib で再現し、FortKMeans が _raise_last_error() により ValueError("code=..: msg") を
送出することを確認する。
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
    def __init__(self, *, fail_on: str, code: int, msg: str):
        super().__init__()
        self.fail_on = fail_on  # 'fit' | 'predict' | 'score'
        self.code = int(code)
        self.msg = str(msg)

        self.kmeans_create  = CFunc(self._kmeans_create)
        self.kmeans_fit     = CFunc(self._kmeans_fit)
        self.kmeans_predict = CFunc(self._kmeans_predict)
        self.kmeans_score   = CFunc(self._kmeans_score)
        self.kmeans_free    = CFunc(lambda *a, **k: None)

    def _kmeans_create(self, *args, **kwargs):
        return 42

    def _kmeans_fit(self, *args, **kwargs):
        if self.fail_on == "fit":
            self.set_error(self.code, self.msg)

    def _kmeans_predict(self, *args, **kwargs):
        if self.fail_on == "predict":
            self.set_error(self.code, self.msg)

    def _kmeans_score(self, *args, **kwargs):
        if self.fail_on == "score":
            self.set_error(self.code, self.msg)
        return 0.0


def _import_kmeans_with_fake_cdll(monkeypatch, fake_lib):
    _patch_shared_loaders(monkeypatch, fake_lib)

    # cerberus が無い環境への配慮
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


@pytest.mark.parametrize("api, code, msg", [
    ("fit",  1001, "fit failed"),
    ("predict", 1002, "predict failed"),
    ("score", 1003, "score failed"),
])
def test_error_propagation_raises_valueerror_with_code_and_message(monkeypatch, api, code, msg):
    fake = FakeLib(fail_on=api, code=code, msg=msg)
    KMeans, _ = _import_kmeans_with_fake_cdll(monkeypatch, fake)

    km = KMeans(n_clusters=2, random_state=0)
    X = np.array([[0.0, 1.0], [2.0, 3.0], [4.0, 5.0]], dtype=np.float64, order="F")

    with pytest.raises(ValueError) as ei:
        getattr(km, api)(X)

    s = str(ei.value)
    assert f"code={code}:" in s and msg in s

