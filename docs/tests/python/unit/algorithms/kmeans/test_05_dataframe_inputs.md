# test_05_dataframe_inputs

- 対象: pandas / polars の DataFrame 入力
- 目的:
  - DataFrame をそのまま `fit/predict/score` に渡しても、`to_numpy()` 経由で 2D `float64` の F-order に正規化されることを確認。
  - C 側ポインタが `POINTER(c_double)` であることを FakeLib で検証。
- 補足:
  - `pytest.importorskip("pandas")` / `pytest.importorskip("polars")` により、未インストール環境では各ケースを自動 skip。
  - `predict` は `(n,1)` かつ `int64` を返す（実装仕様）。

