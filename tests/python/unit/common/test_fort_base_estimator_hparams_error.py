# -*- coding: utf-8 -*-
import ctypes
import importlib
import importlib.util
import pathlib
import sys
import types
import pytest


# ---------- Fake of the shared library (only what we need) ----------
from ..common.fake_ctypes import FakeLibBase

class FakeLib(FakeLibBase):
    """FortBaseEstimator 用。特に追加実装は不要"""
    pass

@pytest.fixture()
def fake_lib():
    return FakeLib()


def _ensure_importable(module_name: str) -> None:
    """
    'flearner.fort_base_estimator' を名前で import できる状態にする。
    できない場合のみ、テストファイルから上位に遡ってリポジトリルートを sys.path に追加。
    """
    spec = importlib.util.find_spec(module_name)
    if spec is not None:
        return

    here = pathlib.Path(__file__).resolve()
    # テストファイルの親ディレクトリを辿り、'flearner' ディレクトリを含む直近のルートを探す
    for p in [here] + list(here.parents):
        if (p / "flearner").is_dir():
            if str(p) not in sys.path:
                sys.path.insert(0, str(p))
            break

    spec = importlib.util.find_spec(module_name)
    assert spec is not None, f"Cannot find module: {module_name}"


def _import_fbe_with_fake_cdll(monkeypatch, fake_lib):
    """
    - import 前に ctypes.CDLL を Fake に差し替え（_LIB 初期化を捕まえる）:contentReference[oaicite:2]{index=2}
    - cerberus が無ければダミー Validator を提供
    - その上で 'flearner.fort_base_estimator' を名前で import（PyPI/ローカルどちらでもOK）
    """
    # 1) CDLL を差し替え（import 時の _LIB 初期化に効かせる）
    monkeypatch.setattr(ctypes, "CDLL", lambda path: fake_lib, raising=True)

    # 2) cerberus が無い環境ではダミーを注入
    try:
        import cerberus  # noqa: F401
    except Exception:
        dummy = types.ModuleType("cerberus")

        class DummyValidator:
            def __init__(self, schema, allow_unknown=False):
                self._errors = {}
            def validate(self, params):  # success by default
                return True
            @property
            def errors(self):
                return self._errors

        dummy.Validator = DummyValidator
        monkeypatch.setitem(sys.modules, "cerberus", dummy)

    # 3) 新鮮な import にする（既に読まれていたら一旦外す）
    sys.modules.pop("flearner.fort_base_estimator", None)

    # 4) 名前で import（見つからなければ repo ルートを探してから再試行）
    _ensure_importable("flearner.fort_base_estimator")
    mod = importlib.import_module("flearner.fort_base_estimator")
    return mod


@pytest.fixture()
def fbe_env(monkeypatch, fake_lib):
    """
    FortBaseEstimator を FakeLib 付きでロードして返すフィクスチャ。
    Validator の具体挙動（Pass/Fail）は各テストで monkeypatch する。
    """
    mod = _import_fbe_with_fake_cdll(monkeypatch, fake_lib)
    return types.SimpleNamespace(FortBaseEstimator=mod.FortBaseEstimator, module=mod, lib=fake_lib)


# ===================== tests =====================

def test_validate_hparams_passes_when_validator_accepts(fbe_env, monkeypatch):
    """
    対象: _validate_hparams() 成功系。:contentReference[oaicite:3]{index=3}
    目的: Validator.validate() が True のとき例外が出ない。
    確認: AlwaysPass を差し替え → _validate_hparams() が通る。
    """
    fbe = fbe_env.module

    class AlwaysPass:
        def __init__(self, schema, allow_unknown=False):
            self._errors = {}
        def validate(self, params):  # cerberus API
            return True
        @property
        def errors(self):
            return self._errors

    monkeypatch.setattr(fbe, "Validator", AlwaysPass, raising=True)

    class Estimator(fbe_env.FortBaseEstimator):
        _hparams_validate_schema = {"alpha": {"type": "integer"}, "mode": {"type": "string"}}
        def __init__(self, alpha, mode):
            super().__init__()
            self.alpha = alpha
            self.mode = mode

    est = Estimator(alpha=10, mode="fast")
    est._validate_hparams()  # 例外なし


def test_validate_hparams_raises_valueerror_with_errors_when_validator_rejects(fbe_env, monkeypatch):
    """
    対象: _validate_hparams() 失敗系。:contentReference[oaicite:4]{index=4}
    目的: validate() False → validator.errors を含む ValueError
    確認: AlwaysFail に差し替え → 'Invalid parameters' と errors が含まれる。
    """
    fbe = fbe_env.module

    class AlwaysFail:
        def __init__(self, schema, allow_unknown=False):
            self._errors = {"alpha": ["must be integer"], "mode": ["must be string"]}
        def validate(self, params):
            return False
        @property
        def errors(self):
            return self._errors

    monkeypatch.setattr(fbe, "Validator", AlwaysFail, raising=True)

    class Estimator(fbe_env.FortBaseEstimator):
        _hparams_validate_schema = {"alpha": {"type": "integer"}, "mode": {"type": "string"}}
        def __init__(self, alpha, mode):
            super().__init__()
            self.alpha = alpha
            self.mode = mode

    est = Estimator(alpha="x", mode=123)
    with pytest.raises(ValueError) as ei:
        est._validate_hparams()
    msg = str(ei.value)
    assert "Invalid parameters" in msg
    assert "alpha" in msg and "mode" in msg


def test_raise_last_error_raises_when_code_nonzero(fbe_env):
    """
    対象: _raise_last_error() 例外伝搬。:contentReference[oaicite:5]{index=5}
    目的: get_last_error の code!=0 を ValueError(code: msg) に正規化。
    確認: code=7, msg='boom 日本語' → 両方がメッセージに含まれる。
    """
    be = fbe_env.FortBaseEstimator()
    fbe_env.lib.set_error(7, "boom 日本語")
    with pytest.raises(ValueError) as ei:
        be._raise_last_error()
    s = str(ei.value)
    assert "code=7" in s and "boom 日本語" in s


def test_raise_last_error_noop_when_code_zero(fbe_env):
    """
    対象: _raise_last_error() 正常系（no-op）。:contentReference[oaicite:6]{index=6}
    目的: code=0 のときは何も起きない。
    確認: 例外なし。
    """
    be = fbe_env.FortBaseEstimator()
    fbe_env.lib.set_error(0, "")
    be._raise_last_error()  # 例外なし
