% flearner.FortKMeans.FortKMeans

# flearner.FortKMeans.FortKMeans

K-Means 推定器（安定 API）。

## コンストラクタ

- `n_clusters: int >= 2`（既定: 6）
- `max_iter: int >= 1`（既定: 300）
- `n_init: int >= 1`（既定: 1）
- `tol: float >= 0`（既定: 1e-4）
- `init: str in {"kmeans++", "random"}`（既定: `"kmeans++"`）
- `random_state: int | None`（既定: None → 乱数）
- `num_threads: int >= 1`（既定: 1）
- `print_log: bool`（既定: False）
- `chunk_size: int | None`（既定: None → 自動）

## メソッド

- `fit(X)`
  - `X`: 2D array-like `(n_samples, n_features)`
  - 内部で `float64` Fortran オーダーに変換
  - 戻り値: `None`

- `predict(X) -> ndarray[int64]`
  - `X`: 2D array-like `(n_samples, n_features)`
  - 戻り値: `int64` 型、`(n_samples, 1)` の 2D 配列（Fortran オーダー）

- `score(X) -> float`
  - `X`: 2D array-like `(n_samples, n_features)`
  - 戻り値: `float`（小さいほど良い）

- `dump(path)` / `@classmethod load(path) -> FortKMeans`
  - Fortran 側のバイナリ互換形式で保存/復元
  - 後方互換ポリシーは `docs/reference/I_Oガイド.md` を参照

## 例外

- ネイティブ側でエラーが発生すると `ValueError("code=<int>: <message>")` を送出。
  - 代表コード: `ERR_NOT_FITTED`, `ERR_FILE_NOT_FOUND`, `ERR_INVALID_FORMAT_VERSION` など。
