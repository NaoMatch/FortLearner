# ftest_kmeans_valid_monotone_inertia

- 対象: 反復数と目的関数（慣性/`best_score`）の単調性
- 目的: 反復上限を増やすと慣性が単調非増加であること
- 確認: `inert[m]` が `m` 増加に対して上昇しない（違反があれば失敗）

