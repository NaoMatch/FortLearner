# Python API 概観（flearner）

FortLearner の Python バインディング `flearner` の安定 API とデータ形状の規約をまとめます。

## 安定 API（Stable）

- `flearner.FortKMeans.FortKMeans`
  - `fit(X)`
  - `predict(X) -> ndarray[int64]` （`(n_samples, 1)`）
  - `score(X) -> float`（低いほど良い）
  - `dump(path)` / `@classmethod load(path) -> FortKMeans`

内部 API（Internal）は変更の可能性があり、ドキュメント対象外です。

## 入出力の型・shape（規約）

- 入力 `X` は 2D 配列 `(n_samples, n_features)`。
- 受理する型: `numpy.ndarray`、または `pandas`/`polars` の DataFrame（内部で `.to_numpy()`）。
- 内部では `float64` かつ Fortran オーダー（列優先）に正規化してネイティブ層へ渡します。
- `predict` の戻り値は `int64` で `(n_samples, 1)` の 2D 配列。

## 例（最小）

```python
from flearner.FortKMeans import FortKMeans
import numpy as np

X = np.random.RandomState(0).randn(100, 3)
km = FortKMeans(n_clusters=4, n_init=5, max_iter=200)
km.fit(X)
labels = km.predict(X)         # shape=(100, 1), dtype=int64
print(float(km.score(X)))      # float
```

## エラー処理

- ネイティブ側のエラーは `get_last_error(code, msg)` を介して Python に伝播し、
  `ValueError("code=<int>: <message>")` として送出されます。
- 代表コードは `docs/FortLearner/fortlearner/utils/check/mod_error_codes.md` を参照。

## 機械可読 JSON

- Python API（機械可読）: `docs/python/api_index.json`
- Fortran バインディングのシグネチャ: `docs/fortran/signatures.json`

