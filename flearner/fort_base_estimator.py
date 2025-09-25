import ctypes
from ctypes import c_int64, byref
from cerberus import Validator
import weakref
import sys
import os
from pathlib import Path
import numpy as np
from numpy.typing import NDArray
from importlib.resources import files, as_file
from flearner.handle_owner import HandleOwner


def _platform_lib_name() -> str:
    """Return platform-specific shared library name for libfortlearner."""
    if sys.platform.startswith("win") or os.name == "nt":
        return "libfortlearner.dll"
    if sys.platform == "darwin":
        return "libfortlearner.dylib"
    return "libfortlearner.so"


def _debug_enabled() -> bool:
    val = os.environ.get("FLEAR_LIB_DEBUG", "")
    return val.lower() in {"1", "true", "yes", "on"}


def _debug(msg: str) -> None:
    if _debug_enabled():
        print(f"[flearner] {msg}")


def _cdll_load(path: str) -> ctypes.CDLL:
    """Load a shared library at PATH, trying RTLD_GLOBAL when available.

    Falls back to a simple call without keyword arguments so tests that
    monkeypatch ctypes.CDLL(path) continue to work.
    """
    CDLL = ctypes.CDLL
    mode = getattr(ctypes, "RTLD_GLOBAL", None)
    if mode is not None:
        try:
            _debug(f"ctypes.CDLL load (RTLD_GLOBAL): {path}")
            lib = CDLL(path, mode=mode)  # real ctypes supports this
            _debug("ctypes.CDLL load OK")
            return lib
        except TypeError:
            # Fake CDLL patched in tests may not accept keyword args
            _debug("ctypes.CDLL(mode=..) not supported; retry without mode")
        except OSError as e:
            _debug(f"ctypes.CDLL error: {e}")
            raise
    _debug(f"ctypes.CDLL load: {path}")
    lib = CDLL(path)
    _debug("ctypes.CDLL load OK")
    return lib


def _load_libfortlearner() -> ctypes.CDLL:
    """Load packaged libfortlearner.* (bundled) or fall back to repo-relative.

    Raises a helpful OSError when dependencies (e.g., OpenBLAS) are missing.
    """
    libname = _platform_lib_name()
    _debug(f"resolved libname: {libname}")

    # 1) Try packaged resource: flearner/lib/libfortlearner.*
    try:
        pkg_root = files("flearner.lib")  # Traversable resource root
        cand = pkg_root / libname
        _debug(f"packaged candidate: {cand}")
        with as_file(cand) as real:
            p = os.fspath(real)
            if os.path.isfile(p):
                _debug(f"loading packaged: {p}")
                return _cdll_load(p)
    except Exception:
        # Ignore and fall back
        _debug("packaged load path not available; falling back to repo-relative")

    # 2) Fallback to repo-relative path (developer mode)
    rel = Path(__file__).resolve().parent / "lib" / libname
    try:
        _debug(f"fallback candidate: {rel}")
        return _cdll_load(str(rel))
    except OSError as e:
        # 3) Give actionable guidance for missing native deps
        hint = (
            f"Failed to load {libname}. If this is a source install, ensure OpenBLAS and a Fortran runtime are available.\n"
            "Ubuntu/Debian: sudo apt-get install -y libopenblas-dev gfortran\n"
            "Fedora/RHEL : sudo dnf install -y openblas-devel gcc-gfortran\n"
            "macOS (brew): brew install openblas gcc\n"
        )
        raise OSError(f"{e}\n{hint}") from e


_LIB = _load_libfortlearner()

def _safe_free(lib, func_name, model_id: int):
    if sys.is_finalizing():  # 終了中は触らない（落とさないほうが大事）
        return
    try:
        func = getattr(lib, func_name, None)
        if func:
            func(c_int64(model_id))
    except Exception:
        pass

class FortBaseEstimator:
    """Internal base for FortLearner estimators.

    Responsibilities:
    - Load bundled native library (`libfortlearner.*`).
    - Provide error propagation bridge (`get_last_error`, `clear_last_error`).
    - Offer common array normalization to 2D `float64` Fortran-order.
    - Manage native handle lifecycle via `HandleOwner`.

    Notes:
    - This is an internal base; it is not part of the stable API surface.
    """
    _hparams_validate_schema = {}
    
    def __init__(self):
        self.lib = None
        self.lib_free = None
        self._model_id = None
        self._manage_handle = True
        self._finalizer = None
        self._closed = False
        self._owner = None

        self._lib = _LIB
        self._err_code = c_int64(0)
        self._err_msg  = ctypes.create_string_buffer(512)
        # 共通エラーAPIの型定義（存在する場合のみ）
        self._bind_error_api()

    # ---- common array coercion helper -------------------------------------
    @staticmethod
    def _coerce_to_2d_float64_f(X) -> NDArray[np.float64]:
        """Normalize input to a 2D `float64` Fortran-ordered array.

        - Accepts `numpy.ndarray` or objects exposing `.to_numpy()` (pandas/polars DataFrame).
        - Validates 2D shape `(n_samples, n_features)`.
        - Converts dtype to `float64` and memory order to Fortran (`order='F'`).
        """
        arr = X.to_numpy() if hasattr(X, "to_numpy") else np.asarray(X)
        if getattr(arr, "ndim", None) != 2:
            raise ValueError(
                f"X must be a 2D array (n_samples, n_features); got ndim={getattr(arr, 'ndim', None)}"
            )
        return np.asfortranarray(arr, dtype=np.float64)

    def _validate_hparams(self):
        params = {key: getattr(self, key) for key in self._hparams_validate_schema}
        validator = Validator(self._hparams_validate_schema, allow_unknown=False)
        if not validator.validate(params):
            raise ValueError(f"Invalid parameters:\n{validator.errors}")

    def _raise_last_error(self):
        """Raise a Python exception if the last native error code is non-zero.

        - Populates `self._err_code` and `self._err_msg` via `get_last_error`.
        - Clears native error buffer on raise.
        - Raises `ValueError(f"code={code}: {message}")`.
        """
        self._lib.get_last_error(byref(self._err_code), self._err_msg)
        if self._err_code.value != 0:
            # UTF-8 でデコード（非ASCII排除済みなら単純 decode でOK）
            msg = self._err_msg.value.decode("utf-8", errors="replace")
            self._lib.clear_last_error()
            raise ValueError(f"code={self._err_code.value}: {msg}")

    def _attach_finalizer(self, lib, func_name, model_id):
        self._finalizer = weakref.finalize(self, _safe_free, lib, func_name, int(model_id))
    
    # ---- common: bind error api ---------------------------------------------
    def _bind_error_api(self):
        try:
            self._lib.get_last_error.argtypes = [ctypes.POINTER(c_int64), ctypes.c_char_p]
            self._lib.get_last_error.restype  = None
            self._lib.clear_last_error.argtypes = []
            self._lib.clear_last_error.restype  = None
        except Exception:
            # Fake/簡易環境では未設定でも動作するため握る
            pass

    # ---- common: adopt & manage a C handle ---------------------------------
    def _adopt_handle(self, handle: int, free_fn, *, name: str | None = None) -> None:
        """Register a C handle with a free function for safe lifecycle.
        - Creates a HandleOwner to ensure single free and thread-safe close().
        - Enables context manager and __del__-based cleanup via close().
        """
        self._owner = HandleOwner(handle=int(handle), free_fn=free_fn, name=name or self.__class__.__name__)
        self._closed = False

    @property
    def _h(self) -> int:
        h = self._owner.handle if self._owner is not None else None
        if h is None:
            raise RuntimeError("Handle already closed")
        return h

    def close(self) -> None:
        if self._owner is not None:
            self._owner.close()
        self._closed = True
    
    def __enter__(self): 
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb): 
        self.close()

    def __del__(self):
        try:
            self.close()
        except Exception:
            pass        
