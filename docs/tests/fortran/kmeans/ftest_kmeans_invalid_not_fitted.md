# ftest_kmeans_invalid_not_fitted

- 対象: `fit` 前の `predict` 呼び出し
- 目的: 未学習状態での推論がエラー停止となること
- 確認: `FAIL` 実行後の終了コードが `ERR_NOT_FITTED`

