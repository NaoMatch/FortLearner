# モデルの保存と復元（Dump / Load）

本ライブラリの KMeans は、Fortran 側でバイナリ形式（stream/unformatted, little_endian）により
モデル（ハイパラ・学習済みパラメータ）を保存/復元できます。Python バインディングからも
同等の API を提供しています。

## Python API

- 保存: `FortKMeans.dump(path: str) -> None`
- 復元: `FortKMeans.load(path: str) -> FortKMeans`

使用例:

```python
import numpy as np
from flearner.FortKMeans import FortKMeans

X = np.asfortranarray([[0.0, 0.0], [1.0, 1.0], [0.9, 1.1], [10.0, -1.0]], dtype=np.float64)

# 学習→保存
km = FortKMeans(n_clusters=2, random_state=0)
km.fit(X)
km.dump("model.bin")
km.close()

# 復元→利用
km2 = FortKMeans.load("model.bin")
labels = km2.predict(X)
score = km2.score(X)
km2.close()
```

例外伝搬:
- 復元時にファイルが存在しない/壊れている場合、`ValueError("code=...: <message>")` を送出します。

## ファイル形式（参考）
- 形式: `access='stream', form='unformatted', convert='little_endian'`
- バージョン: 先頭に `int32 :: FORMAT_VER` を書き出し、将来の後方互換に備えています。
- 配列は列優先（Fortran order）。Python/Numpy から読む場合は `order='F'` を指定してください。

## Fortran 実装詳細
- 保存実体: `fortlearner/kmeans/mod_kmeans.f90` の `dump_kmeans`
- 復元実体: 同 `load_kmeans`
- Python バインディング: `fortlearner/kmeans/bindings/mod_kmeans_binding.f90` の
  `kmeans_dump` / `kmeans_load` を通じて呼び出されます。

