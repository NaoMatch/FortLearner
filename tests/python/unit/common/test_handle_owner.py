# -*- coding: utf-8 -*-
import ctypes
import threading
from dataclasses import dataclass
import pytest

# 可能なら通常インポート。無いときは後述の conftest.py で動的ロードする想定。
try:
    from flearner.handle_owner import HandleOwner
except Exception as e:  # pragma: no cover - conftest が面倒を見る
    raise


# ---- FakeのC free関数 --------------------------------------------------------
class CFakeFree:
    """ctypes関数風: __call__(c_int64) を受け、呼び出し記録を残す"""
    def __init__(self, sink):
        self.sink = sink
        self.argtypes = (ctypes.c_int64,)  # 見た目だけ付けておく
        self.restype = None

    def __call__(self, v):
        # c_int64 で来る想定。valueを取り出しリストへ記録
        hid = int(getattr(v, "value", v))
        self.sink.append(hid)


# ========== 1) close + detach + free(once) ===================================
def test_handle_owner_close_detach_and_free_once():
    """
    対象: HandleOwner.close() の解放フロー
    目的: 明示的解放が唯一の経路として動く/二重に走らないことの保証
    確認: handle->None/closed=True、finalizer.alive=False、freeはちょうど1回
    """
    freed = []
    free_fn = CFakeFree(freed)
    ho = HandleOwner(handle=123, free_fn=free_fn, name="X")

    assert ho.handle == 123 and ho.closed is False
    fin = ho._finalizer
    assert fin.alive is True  # detach前

    ho.close()

    assert ho.handle is None and ho.closed is True
    assert fin.alive is False            # detach 済み
    assert freed == [123]                # freeは1回だけ


# ========== 2) close の冪等性 =================================================
def test_handle_owner_close_is_idempotent():
    """
    対象: close() の冪等性
    目的: 複数回closeでも追加解放されないこと
    確認: 2回目以降のcloseでfree回数が増えない
    """
    freed = []
    ho = HandleOwner(handle=77, free_fn=CFakeFree(freed))
    ho.close()
    assert freed == [77]
    ho.close()  # 2回目
    assert freed == [77]  # 変化なし


# ========== 3) free_fn の型チェック ==========================================
def test_handle_owner_requires_callable_free_fn():
    """
    対象: コンストラクタ引数 free_fn の検証
    目的: API契約に反する引数を早期に弾く
    確認: free_fn が callable でなければ TypeError
    """
    with pytest.raises(TypeError):
        HandleOwner(handle=1, free_fn=object())


# ========== 4) _free_safely は例外を握る =====================================
def test_free_safely_swallows_exceptions():
    """
    対象: HandleOwner._free_safely（finalizerコールバック）
    目的: free 失敗時も例外を外へ伝播させない方針の確認
    確認: 例外を投げるfree関数を渡しても例外にならない
    """
    def bad_free(_):
        raise RuntimeError("boom")

    # 例外が外に出ないこと（何も起こらなければOK）
    HandleOwner._free_safely(bad_free, 999)


# ========== 5) プロパティの状態遷移 ==========================================
def test_handle_and_closed_properties():
    """
    対象: handle/closed プロパティ
    目的: 状態の可視性（正しいハンドル値とclose後の変化）
    確認: 生成直後: closed=False, handle=int / close後: closed=True, handle=None
    """
    freed = []
    ho = HandleOwner(handle=5, free_fn=CFakeFree(freed))
    assert isinstance(ho.handle, int) and ho.closed is False
    ho.close()
    assert ho.handle is None and ho.closed is True


# ========== 6) 並行 close() の安全性 =========================================
def test_concurrent_close_is_thread_safe():
    """
    対象: 複数スレッドからの同時 close()（RLockによる排他）
    目的: 並行環境でも free は1回のみ
    確認: 10スレッド同時に close() しても free 呼び出しは1回
    """
    freed = []
    ho = HandleOwner(handle=31415, free_fn=CFakeFree(freed))

    # スタート用バリアでほぼ同時にcloseさせる
    n = 10
    barrier = threading.Barrier(n + 1)
    threads = []
    for _ in range(n):
        t = threading.Thread(target=lambda: (barrier.wait(), ho.close()))
        t.start()
        threads.append(t)

    barrier.wait()  # 一斉スタート
    for t in threads:
        t.join()

    assert freed == [31415]


# ========== 7) finalizer（保険）の明示実行 ====================================
def test_finalizer_manual_call_invokes_free_when_not_detached():
    """
    対象: weakref.finalize に登録された保険の解放
    目的: detachされていないfinalizerを明示実行するとfreeが走る
    確認: ho._finalizer() 実行で free は1回呼ばれ、finalizer.alive=False
    """
    freed = []
    ho = HandleOwner(handle=2024, free_fn=CFakeFree(freed))
    fin = ho._finalizer
    assert fin.alive is True
    fin()  # 明示実行
    assert freed == [2024]
    assert fin.alive is False
    # このテストでは以後 ho.close() は呼ばない（double-freeを避ける）
