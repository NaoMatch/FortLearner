# ftest_kmeans_invalid_max_iter

- 対象: `new_kmeans(max_iter=0)` のバリデーション
- 目的: 非正値の最大反復数でエラー停止になること
- 確認: `FAIL` 実行後の終了コードが `ERR_NON_POSITIVE_VALUE`

