# ftest_kmeans_invalid_n_init

- 対象: `new_kmeans(n_init=0)` のバリデーション
- 目的: 非正値の初期化回数でエラー停止になること
- 確認: `FAIL` 実行後の終了コードが `ERR_NON_POSITIVE_VALUE`

