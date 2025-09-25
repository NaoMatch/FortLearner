import ctypes
import threading
import weakref
from ctypes import c_int64

class HandleOwner:
    """Cハンドルの安全なライフサイクル管理 (合成で使う・内部 API)。"""
    __slots__ = ("_h", "_free", "_finalizer", "_closed", "_lock", "_name", "__weakref__")

    def __init__(self, *, handle: int, free_fn, name: str = ""):
        """
        handle : int (C側ハンドル; 64bit想定)
        free_fn: ctypesで型設定済みのC関数 (戻り値int, 引数int64)
        """
        if not callable(free_fn):
            raise TypeError("free_fn must be callable C function")

        self._h = int(handle)
        self._free = free_fn     # ここは関数ポインタだけ（selfを閉じない）
        self._closed = False
        self._lock = threading.RLock()
        self._name = name or "Handle"

        # finalizer は「保険」。selfを参照しない（関数と整数のみ）。
        self._finalizer = weakref.finalize(self, HandleOwner._free_safely, free_fn, int(self._h))

    @staticmethod
    def _free_safely(fn, hid: int):
        try:
            fn(c_int64(hid))
        except Exception:
            # 終了時の順序問題（ライブラリが既に解放等）は握る
            pass

    @property
    def handle(self) -> int | None:
        return self._h

    @property
    def closed(self) -> bool:
        return self._closed

    def close(self) -> None:
        """解放経路は一本化：finalizerは実行せず detach→直接freeを一度だけ"""
        with self._lock:
            if self._closed:
                return
            hid = self._h
            self._h = None
            self._closed = True
            fin = self._finalizer
            if fin is not None:
                try:
                    fin.detach()  # 以後finalizerは走らない
                except Exception:
                    pass

            if hid is not None:
                try:
                    self._free(c_int64(hid))
                except Exception:
                    # ライブラリ終了順などで失敗しても落とさない
                    pass
