# -*- coding: utf-8 -*-
"""
FortKMeans のハイパラ検証（Cerberus が正常に動いているか）

- 目的: __init__ 時の Cerberus バリデーションで不正型/不正値が弾かれることを確認。
- 手法: 共有ライブラリは Fake に差し替えつつ、cerberus は実体を使用（未導入環境では skip）。
"""
import importlib
import sys
import types

import numpy as np
import pytest

from ...common.fake_ctypes import CFunc, FakeLibBase
from ...common.import_utils import _patch_shared_loaders, _ensure_importable


cerberus = pytest.importorskip("cerberus")  # 実 Cerberus を前提にする


class FakeLib(FakeLibBase):
    def __init__(self):
        super().__init__()
        self._next_id = 5000
        self.kmeans_create  = CFunc(self._kmeans_create)
        self.kmeans_fit     = CFunc(lambda *a, **k: None)
        self.kmeans_predict = CFunc(lambda *a, **k: None)
        self.kmeans_score   = CFunc(lambda *a, **k: 0.0)
        self.kmeans_free    = CFunc(lambda *a, **k: None)

    def _kmeans_create(self, *args, **kwargs):
        hid = self._next_id
        self._next_id += 1
        return hid


def _import_kmeans_with_fake_cdll(monkeypatch, fake_lib):
    """共有ライブラリだけ Fake に差し替え、cerberus は実体を使う。"""
    _patch_shared_loaders(monkeypatch, fake_lib)

    # 直前にロード済みの対象モジュールを捨てる（新鮮な import で Fake を反映）
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
def KMeans_cls(monkeypatch):
    fake = FakeLib()
    KMeans, _ = _import_kmeans_with_fake_cdll(monkeypatch, fake)
    return KMeans


def test_valid_params_pass(KMeans_cls):
    # 代表的な正常ケースが __init__ を通過すること
    km = KMeans_cls(
        n_clusters=3,
        max_iter=100,
        n_init=1,
        tol=1e-4,
        init="kmeans++",
        random_state=None,
        num_threads=1,
        print_log=False,
        chunk_size=None,
    )
    assert hasattr(km, "_model_id")


@pytest.mark.parametrize(
    "bad_kwargs, key_substr",
    [
        ({"n_clusters": "3"}, "n_clusters"),
        ({"max_iter": 1.2}, "max_iter"),
        ({"n_init": 1.0}, "n_init"),
        ({"tol": "0.1"}, "tol"),
        ({"init": 123}, "init"),
        ({"random_state": 3.5}, "random_state"),
        ({"num_threads": "2"}, "num_threads"),
        ({"print_log": "yes"}, "print_log"),
        ({"chunk_size": 2.7}, "chunk_size"),
    ],
)
def test_invalid_types_raise_value_error_with_field_in_message(KMeans_cls, bad_kwargs, key_substr):
    kwargs = dict(
        n_clusters=3,
        max_iter=100,
        n_init=1,
        tol=1e-4,
        init="kmeans++",
        random_state=0,
        num_threads=1,
        print_log=False,
        chunk_size=-1,
    )
    kwargs.update(bad_kwargs)

    with pytest.raises(ValueError) as ei:
        KMeans_cls(**kwargs)
    s = str(ei.value)
    assert "Invalid parameters" in s
    assert key_substr in s


def test_nullable_fields_accept_none(KMeans_cls):
    # random_state / chunk_size は nullable=True
    km = KMeans_cls(
        n_clusters=2, max_iter=10, n_init=1, tol=1e-3, init="kmeans++",
        random_state=None, num_threads=1, print_log=False, chunk_size=None,
    )
    assert hasattr(km, "_model_id")

