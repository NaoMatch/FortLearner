# test_03_dtype_and_order_coercion

- 対象: `FortKMeans.fit/predict/score` の入力正規化（dtype / メモリオーダー）
- 目的:
  - `float32` や各種 `int*`/`uint*`、および `'C'`/`'F'` オーダーの Numpy 配列が、
    入口で `np.asfortranarray(..., dtype=float64)` により安全に受理されることを確認。
  - C 側に渡るポインタ型が `POINTER(c_double)`（= float64）であることを FakeLib 経由で検証。
- 確認:
  - 例外が出ないこと
  - `fit` に渡る `(n_rows, n_cols)` が一致
  - `predict` は `np.int64` かつ `(n,1)` を返す（実装仕様）
  - `score` は Python `float` を返す

