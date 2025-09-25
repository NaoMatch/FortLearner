# ftest_kmeans_invalid_format_mismatch

- 対象: モデルの dump/load フォーマットバージョン不一致
- 目的: 保存後に先頭ヘッダのバージョンを書き換えたファイルの `load` がエラー停止となること
- 確認: `FAIL` 実行後の終了コードが `ERR_INVALID_FORMAT_VERSION`

