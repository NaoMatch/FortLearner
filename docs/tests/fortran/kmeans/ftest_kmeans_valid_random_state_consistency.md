# ftest_kmeans_valid_random_state_consistency

- 対象: 乱数シード固定時の決定性
- 目的: 同一 `random_state`/`max_iter` で学習すると、ラベル・セントロイドが一致すること
- 確認: `all(lab1==lab2)` かつ `abs(cen1-cen2)` の差分が十分小さい

