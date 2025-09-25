# test_04_shape_validation

- 対象: `FortKMeans.fit/predict/score` の入力形状バリデーション
- 目的:
  - 1D や 3D 以上の入力は Python 側で即座に `ValueError` を送出し、C 側関数が呼ばれないことを確認。
  - 0 行や 0 列など 2D の範囲内のエッジは別テストで担保（test_02）。
- 確認:
  - 例外メッセージに「2D」を含む
  - Fake 側の `kmeans_fit/predict/score` は一切呼ばれない

