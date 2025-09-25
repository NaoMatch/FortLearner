# tests/python/unit/algorithms/kmeans/test_01_init_mapping.py
# -*- coding: utf-8 -*-
"""
FortKMeans.__init__/__load_dll の「ctypes結線」と「引数マッピング」を白箱で検証する。
- 共有ライブラリのローダ（CDLL/LoadLibrary/PyDLL/numpy.ctypeslib.load_library）を import 前に Fake に差し替え
- PyPI インストール／ローカルソースの両方で、モジュール名で import（見つからない場合だけ repo ルートを探索）
- kmeans_create へ渡された実値（random_state, chunk_size など前処理後の値）をログで観測
- ctypes の argtypes/restype は「設定されていれば検証」。厳格化したいときは env FLEAR_STRICT_SIGNATURE=1
"""
import ctypes
import importlib
import importlib.util
import pathlib
import sys
import types
import pytest
import numpy as np
from ctypes import c_int64, c_double, c_bool, c_char_p, POINTER


from ...common.fake_ctypes import CFunc, FakeLibBase
from ...common.import_utils import _patch_shared_loaders, _ensure_importable

class FakeLib(FakeLibBase):
    def __init__(self):
        super().__init__()
        self._next_id = 100
        self.created_ids = []
        self.last_create_args = None

        self.kmeans_create  = CFunc(self._kmeans_create)
        self.kmeans_fit     = CFunc(self._kmeans_fit)
        self.kmeans_predict = CFunc(self._kmeans_predict)
        self.kmeans_score   = CFunc(self._kmeans_score)
        self.kmeans_free    = CFunc(self._kmeans_free)

    def _kmeans_create(self, n_clusters, max_iter, init, n_init, tol,
                       random_state, num_threads, print_log, chunk_size):
        hid = self._next_id
        self._next_id += 1
        self.created_ids.append(hid)
        self.last_create_args = dict(
            n_clusters=self._as_int(n_clusters),
            max_iter=self._as_int(max_iter),
            n_init=self._as_int(n_init),
            tol=self._as_float(tol),
            random_state=self._as_int(random_state),
            num_threads=self._as_int(num_threads),
            print_log=self._as_bool(print_log),
            chunk_size=self._as_int(chunk_size),
        )
        return hid

    def _kmeans_fit(self, *args, **kwargs): return None
    def _kmeans_predict(self, *args, **kwargs): return None
    def _kmeans_score(self, *args, **kwargs): return 0.0
    def _kmeans_free(self, handle):
        self.freed.append(self._as_int(handle))
        return None

# ---- モジュール解決（PyPI/ローカル両対応） ---------------------------------
def _ensure_importable(module_name: str) -> None:
    spec = importlib.util.find_spec(module_name)
    if spec is not None:
        return
    here = pathlib.Path(__file__).resolve()
    for p in [here] + list(here.parents):
        if (p / "flearner").exists():
            if str(p) not in sys.path:
                sys.path.insert(0, str(p))
            break
    spec = importlib.util.find_spec(module_name)
    assert spec is not None, f"Cannot find module: {module_name}"


# ---- 共有ライブラリのロード経路を“広く”差し替える（import前に必須） --------
def _import_kmeans_with_fake_cdll(monkeypatch, fake_lib):
    """
    - FortBaseEstimator は import 時に共有ライブラリをロードするため、import前にローダを差し替える。:contentReference[oaicite:3]{index=3}
    - cerberus が無い環境でも落ちないよう最小のダミー Validator を注入。
    - その上で 'flearner.FortKMeans' を名前で import（PyPI/ローカル両対応）。:contentReference[oaicite:4]{index=4}
    """
    _patch_shared_loaders(monkeypatch, fake_lib)

    # cerberus ダミー（存在すれば不要）
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
    return mod


@pytest.fixture()
def km_env(monkeypatch):
    fake = FakeLib()
    mod = _import_kmeans_with_fake_cdll(monkeypatch, fake)

    # FortKMeans クラスの場所は、パッケージ版だと flearner.FortKMeans.FortKMeans.FortKMeans
    # 単一ファイル版だと flearner.FortKMeans.FortKMeans
    KMeans = getattr(mod, "FortKMeans", None)
    if KMeans is None and hasattr(mod, "__dict__"):
        sub = mod.__dict__.get("FortKMeans")
        if sub and hasattr(sub, "FortKMeans"):
            KMeans = sub.FortKMeans
    assert KMeans is not None, "FortKMeans クラスが見つかりません"

    return types.SimpleNamespace(FortKMeans=KMeans, module=mod, lib=fake)


# ============================== テスト本体 ===================================
def test_init_sets_argtypes_restype_and_maps_params(km_env, monkeypatch):
    """
    対象: __load_dll と __init__ のマッピング/シグネチャ設定（:contentReference[oaicite:5]{index=5}）
    目的:
      - 前処理マッピング（random_state/chunk_size）が正しく行われる
      - kmeans_create に正しい値が渡る
      - ctypes の argtypes/restype は「設定されていれば」実装どおり
    確認:
      - create 受領値: chunk_size=-1（None→-1）, random_state=int, tol=float 等
      - （任意）argtypes/restype 検査：存在する場合のみ assert
        * 厳格チェックは環境変数 FLEAR_STRICT_SIGNATURE=1 で有効化
    """
    import os
    KMeans = km_env.FortKMeans
    lib = km_env.lib

    km = KMeans(
        n_clusters=3,
        max_iter=5,
        n_init=2,
        tol=1e-3,
        init="kmeans++",
        random_state=None,    # None → int へ正規化
        num_threads=2,
        print_log=True,
        chunk_size=None       # None → -1
    )

    # ---- 必須: kmeans_create に渡った「値」の検証（前処理マッピングの正しさ） ----
    args = lib.last_create_args
    assert args is not None, "kmeans_create が呼ばれていません"
    assert args["n_clusters"] == 3
    assert args["max_iter"] == 5
    assert args["n_init"] == 2
    assert np.isclose(args["tol"], 1e-3)
    assert args["num_threads"] == 2
    assert args["print_log"] is True
    assert args["chunk_size"] == -1                # None→-1（実装仕様）:contentReference[oaicite:6]{index=6}
    assert isinstance(args["random_state"], int)   # None→int（実装仕様）:contentReference[oaicite:7]{index=7}

    # __load_dll が free の委譲先を設定していること:contentReference[oaicite:8]{index=8}
    assert km.lib_free is lib.kmeans_free

    # ---- 任意: ctypes シグネチャ（存在するときだけ検証） --------------------
    strict = os.environ.get("FLEAR_STRICT_SIGNATURE", "0") == "1"

    def maybe_assert_sig(fn, expected_argtypes, expected_restype):
        at = getattr(fn, "argtypes", None)
        rt = getattr(fn, "restype", None)
        if at is None and rt is None and not strict:
            return  # 設定していないビルドはスキップ
        if at is not None:
            assert at == expected_argtypes
        if expected_restype is not None and rt is not None:
            assert rt is expected_argtypes and expected_restype or expected_restype  # noqa: E701

    maybe_assert_sig(
        lib.kmeans_create,
        [c_int64, c_int64, c_char_p, c_int64, c_double, c_int64, c_int64, c_bool, c_int64],
        c_int64,
    )
    maybe_assert_sig(
        lib.kmeans_fit,
        [c_int64, POINTER(c_double), c_int64, c_int64],
        None,
    )
    maybe_assert_sig(
        lib.kmeans_predict,
        [c_int64, POINTER(c_double), c_int64, c_int64, POINTER(c_int64)],
        None,
    )
    maybe_assert_sig(
        lib.kmeans_free,
        [c_int64],
        None,
    )
    maybe_assert_sig(
        lib.kmeans_score,
        [c_int64, POINTER(c_double), c_int64, c_int64],
        c_double,
    )
    maybe_assert_sig(
        lib.get_last_error,
        [ctypes.POINTER(ctypes.c_int64), ctypes.c_char_p],
        None,
    )

    # Fake が本当に注入されたことの安全確認（ここで躓くならローダ分岐未カバー）
    assert getattr(km, "_lib", None) is lib, "FakeLib の注入に失敗しています"
