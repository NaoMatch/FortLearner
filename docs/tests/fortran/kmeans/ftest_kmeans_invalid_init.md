# ftest_kmeans_invalid_init

- 対象: `new_kmeans(init="invalid_option")` のバリデーション
- 目的: 許可されない初期化方式でエラー停止になること
- 確認: `FAIL` 実行後の終了コードが `ERR_DISALLOWED_OPTION`

