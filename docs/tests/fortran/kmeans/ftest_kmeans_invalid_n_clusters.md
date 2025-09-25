# ftest_kmeans_invalid_n_clusters

- 対象: `new_kmeans(n_clusters=0)` のバリデーション
- 目的: クラスタ数の下限違反でエラー停止になること
- 確認: `FAIL` 実行後の終了コードが `ERR_BELOW_MIN`

