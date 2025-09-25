# ftest_kmeans_invalid_chunk_size

- 対象: `new_kmeans(chunk_size=0)` のバリデーション
- 目的: 非正値のチャンクサイズでエラー停止になること
- 確認: `FAIL` 実行後の終了コードが `ERR_NON_POSITIVE_VALUE`

