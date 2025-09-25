# ftest_kmeans_invalid_non_finite

- 対象: 入力データに `NaN`/`+Inf` を含む場合の学習
- 目的: 非有限値を含むデータでエラー停止となること
- 確認: `FAIL` 実行後の終了コードが `ERR_NON_FINITE`

