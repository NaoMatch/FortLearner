# ftest_kmeans_invalid_tol_nan

- 対象: `new_kmeans(tol=NaN)` のバリデーション
- 目的: 非有限値（NaN）の許容誤差でエラー停止になること
- 確認: `FAIL` 実行後の終了コードが `ERR_NON_FINITE`

