# ftest_kmeans_invalid_tol_inf

- 対象: `new_kmeans(tol=+Inf)` のバリデーション
- 目的: 非有限値（+Inf）の許容誤差でエラー停止になること
- 確認: `FAIL` 実行後の終了コードが `ERR_NON_FINITE`

