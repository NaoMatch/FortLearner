import numpy as np
from numpy.typing import NDArray
from ctypes import c_int64, create_string_buffer, byref, c_char_p, POINTER, c_double, c_bool, c_size_t
from flearner.handle_owner import HandleOwner
from flearner.fort_base_estimator import FortBaseEstimator

class FortKMeans(FortBaseEstimator):
    """K-Means estimator (stable Python API).

    Parameters
    - n_clusters: int >= 2
    - max_iter: int >= 1
    - n_init: int >= 1
    - tol: float >= 0
    - init: str in {"kmeans++", "random"}
    - random_state: int | None
    - num_threads: int >= 1
    - print_log: bool
    - chunk_size: int | None (if None → auto)

    Notes
    - Input arrays are normalized to `float64` and Fortran order internally.
    - Labels are returned as `int64` with shape `(n_samples, 1)`.
    """
    _algo = "kmeans"
    
    _hparams_validate_schema = {
        "n_clusters":     {"type": "integer"},
        "max_iter":       {"type": "integer"},
        "init":           {"type": "string"},
        "n_init":         {"type": "integer"},
        "tol":            {"type": "float"},
        "random_state":   {"type": "integer", "nullable": True},
        "num_threads":    {"type": "integer"},
        "print_log":      {"type": "boolean"},
        "chunk_size":     {"type": "integer", "nullable": True}
    }
    
    def __init__(
        self,
        n_clusters=6,
        max_iter=300,
        n_init=1,
        tol=1e-4,
        init="kmeans++",
        random_state=None,
        num_threads=1,
        print_log=False,
        chunk_size=None
    ):
        super().__init__()
        self.n_clusters = n_clusters
        self.max_iter = max_iter
        self.n_init = n_init
        self.tol = tol
        self.init = init
        self.random_state = random_state
        self.num_threads = num_threads
        self.print_log = print_log
        self.chunk_size = chunk_size

        self._closed = False
        self._manage_handle = True   # ← 追加：所有権フラグ
        self._validate_hparams()
        self.random_state = self.random_state if self.random_state is not None else np.random.randint(0, 2**32, dtype=np.int64).item()
        self.chunk_size   = -1 if self.chunk_size is None else self.chunk_size 

        self.__load_dll()

        n_clusters_   = c_int64(int(self.n_clusters))
        max_iter_     = c_int64(self.max_iter)
        init_         = create_string_buffer(self.init.encode("utf-8"))
        n_init_       = c_int64(self.n_init)
        tol_          = c_double(float(self.tol))  # 念のため float() で純Python floatに
        random_state_ = c_int64(self.random_state)
        num_threads_  = c_int64(self.num_threads)
        print_log_    = c_bool(bool(self.print_log))
        chunk_size_   = c_int64(self.chunk_size)

        self._model_id = self._lib.kmeans_create(
                n_clusters_,    
                max_iter_,      
                init_,          
                n_init_,        
                tol_,           
                random_state_,  
                num_threads_,   
                print_log_,     
                chunk_size_   
            )

        # エラー状態の確認（_raise_last_error 内で get_last_error を呼ぶため単一呼び出しで十分）
        self._raise_last_error()  # 失敗時はここで例外
        # ★ 合成（共通基盤へ移譲）
        self._adopt_handle(self._model_id, self.lib_free, name="FortKMeans")

    # _h / close は FortBaseEstimator に移譲

    def __load_dll(self):
        # 型定義
        # lib.kmeans_create.argtypes = [c_int64]
        self._lib.kmeans_create.argtypes = [
            c_int64, 
            c_int64, 
            c_char_p, 
            c_int64, 
            c_double, 
            c_int64, 
            c_int64, 
            c_bool, 
            c_int64]
        self._lib.kmeans_create.restype = c_int64
    
        self._lib.kmeans_fit.argtypes = [c_int64, POINTER(c_double), c_int64, c_int64]
        self._lib.kmeans_fit.restype = None
    
        self._lib.kmeans_predict.argtypes = [c_int64, POINTER(c_double), c_int64, c_int64, POINTER(c_int64)]
        self._lib.kmeans_predict.restype = None
    
        self._lib.kmeans_free.argtypes = [c_int64]
        self._lib.kmeans_free.restype = None
    
        self._lib.kmeans_score.argtypes = [c_int64, POINTER(c_double), c_int64, c_int64]
        self._lib.kmeans_score.restype = c_double

        # dump/load bindings (optional in FakeLib during unit tests)
        kd = getattr(self._lib, "kmeans_dump", None)
        if kd is not None:
            kd.argtypes = [c_int64, c_char_p, c_size_t]
            kd.restype  = None
        kl = getattr(self._lib, "kmeans_load", None)
        if kl is not None:
            kl.argtypes = [c_char_p, c_size_t]
            kl.restype  = c_int64

        self.lib_free = self._lib.kmeans_free

        self._lib.get_last_error.argtypes = [POINTER(c_int64), c_char_p]
        self._lib.get_last_error.restype  = None

        self._lib.clear_last_error.argtypes = []
        self._lib.clear_last_error.restype  = None
            
    def fit(self, X: NDArray[np.floating] | np.ndarray):
        """Fit model on 2D samples array.

        - X: array-like `(n_samples, n_features)` of numeric type. Coerced to
          `float64` Fortran-order.
        - Returns: None (scikit-learn style; method mutates instance)
        """
        X_f = self._coerce_to_2d_float64_f(X)
        n_rows, n_cols = map(c_int64, X_f.shape)

        self._lib.kmeans_fit(
            c_int64(self._h),
            X_f.ctypes.data_as(POINTER(c_double)),
            n_rows, n_cols
        )
        self._lib.get_last_error(byref(self._err_code), self._err_msg)
        self._raise_last_error()  # 失敗時はここで例外

    def predict(self, X: NDArray[np.floating] | np.ndarray) -> NDArray[np.int64]:
        """Predict labels for 2D samples array.

        - X: array-like `(n_samples, n_features)`, coerced to `float64` F-order.
        - Returns: `int64` labels with shape `(n_samples, 1)` (Fortran order).
        """
        # --- 入力は 2D 限定。pandas/polars DataFrame を許容し、内部で float64 F に正規化 ---
        X_f = self._coerce_to_2d_float64_f(X)
    
        # 行・列は Python int で保持 → ctypes 呼び出し時に c_int64 に変換
        n_rows, n_cols = X_f.shape
    
        # --- 出力バッファ: 2 次元の列ベクトル (n_rows, 1)。order='F' で列優先 ---
        labels = np.empty((n_rows, 1), dtype=np.int64, order='F')
    
        # --- ctypes 呼び出し ---
        self._lib.kmeans_predict(
            c_int64(self._h),
            X_f.ctypes.data_as(POINTER(c_double)),
            c_int64(n_rows), c_int64(n_cols),
            labels.ctypes.data_as(POINTER(c_int64)),
        )

        self._lib.get_last_error(byref(self._err_code), self._err_msg)
        self._raise_last_error()  # 失敗時はここで例外
    
        # 2D ndarray (n_rows, 1) を返却（実装仕様）
        return labels
        

    def score(self, X: NDArray[np.floating] | np.ndarray) -> float:
        """Compute inertia-like score given samples.

        - X: array-like `(n_samples, n_features)`, coerced to `float64` F-order.
        - Returns: float64 score (lower is better).
        """
        X_f = self._coerce_to_2d_float64_f(X)
        
        # 行・列は Python int で保持 → ctypes 呼び出し時に c_int64 に変換
        n_rows, n_cols = X_f.shape
    
        # --- 出力バッファ: 1 次元で OK。order='F' で列優先にしておく ---
        score = c_double(0.0)
    
        # --- ctypes 呼び出し ---
        score = self._lib.kmeans_score(
            c_int64(self._h),
            X_f.ctypes.data_as(POINTER(c_double)),
            c_int64(n_rows), c_int64(n_cols),
        )
        self._lib.get_last_error(byref(self._err_code), self._err_msg)
        self._raise_last_error()  # 失敗時はここで例外

        # 1D ndarray を返却（scikit-learn 互換）
        return score
    
    def dump(self, filename: str) -> None:
        """Persist model to file path using Fortran-side binary format.

        - filename: str (UTF-8)
        - Raises: ValueError when native layer reports an error.
        """
        if not hasattr(self._lib, "kmeans_dump"):
            raise NotImplementedError("dump is not available in the current backend")
        b = filename.encode("utf-8")
        buf = create_string_buffer(b)
        self._lib.kmeans_dump(c_int64(self._h), buf, c_size_t(len(b)+1))
        self._lib.get_last_error(byref(self._err_code), self._err_msg)
        self._raise_last_error()

    @classmethod
    def load(cls, filename: str) -> "FortKMeans":
        """Load model from file and return a ready instance.

        - filename: str (UTF-8)
        - Returns: FortKMeans
        - Raises: ValueError on native error; NotImplementedError if backend lacks I/O
        """
        obj = cls.__new__(cls)  # bypass __init__
        # Initialize base to set up loader and error buffers
        FortBaseEstimator.__init__(obj)
        obj.__load_dll()
        if not hasattr(obj._lib, "kmeans_load"):
            raise NotImplementedError("load is not available in the current backend")
        b = filename.encode("utf-8")
        buf = create_string_buffer(b)
        model_id = obj._lib.kmeans_load(buf, c_size_t(len(b)+1))
        obj._lib.get_last_error(byref(obj._err_code), obj._err_msg)
        # Propagate error if any
        try:
            obj._raise_last_error()
        except Exception:
            # Ensure we don't hold an invalid owner on failure
            raise
        # Adopt handle and finish wiring
        obj._adopt_handle(model_id, obj.lib_free, name="FortKMeans")
        # Populate minimal public attributes (optional)
        obj._closed = False
        obj._manage_handle = True
        return obj
