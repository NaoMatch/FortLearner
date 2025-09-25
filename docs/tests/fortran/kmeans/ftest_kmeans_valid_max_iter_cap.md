# ftest_kmeans_valid_max_iter_cap

- 対象: 反復上限 `max_iter` の上限動作
- 目的: 収束条件が厳しい場合でも反復回数が `max_iter` にキャップされること
- 確認: 学習後の `n_iter_ == max_iter`

