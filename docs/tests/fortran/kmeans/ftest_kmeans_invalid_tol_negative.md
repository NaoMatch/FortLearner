# ftest_kmeans_invalid_tol_negative

- 対象: `new_kmeans(tol < 0)` のバリデーション
- 目的: 負の許容誤差でエラー停止になること
- 確認: `FAIL` 実行後の終了コードが `ERR_NEGATIVE_VALUE`

