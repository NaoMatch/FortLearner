# tests/python/unit/common/fake_ctypes.py
import ctypes

class CFunc:
    def __init__(self, func):
        self._func = func
        self.argtypes = None
        self.restype = None
    def __call__(self, *args, **kwargs):
        return self._func(*args, **kwargs)


class FakeLibBase:
    def __init__(self):
        self._err_code = 0
        self._err_msg = b""
        self.freed = []

        self.get_last_error   = CFunc(self._get_last_error)
        self.clear_last_error = CFunc(self._clear_last_error)

    def set_error(self, code: int, msg: str = ""):
        self._err_code = int(code)
        self._err_msg = msg.encode("utf-8")

    def _get_last_error(self, code_ptr, msg_buf):
        p = ctypes.cast(code_ptr, ctypes.POINTER(ctypes.c_int64))
        p.contents.value = self._err_code
        data = self._err_msg + b"\x00"
        ctypes.memmove(msg_buf, data, len(data))

    def _clear_last_error(self):
        self._err_code = 0
        self._err_msg = b""

    # ---- helpers for unwrapping ctypes values in tests ----
    @staticmethod
    def _unwrap(value):
        """Return underlying Python value from ctypes scalars/arrays."""
        try:
            # ctypes scalars (c_int64, c_double, c_bool, etc.)
            if hasattr(value, "value"):
                return value.value
        except Exception:
            pass
        return value

    def _as_int(self, value) -> int:
        v = self._unwrap(value)
        return int(v)

    def _as_float(self, value) -> float:
        v = self._unwrap(value)
        return float(v)

    def _as_bool(self, value) -> bool:
        v = self._unwrap(value)
        return bool(v)
