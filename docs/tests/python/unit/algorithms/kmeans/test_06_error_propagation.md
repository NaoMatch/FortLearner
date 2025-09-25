# test_06_error_propagation

- 対象: 例外伝搬（lib エラーの拾い上げ）
- 目的:
  - `fit`/`predict`/`score` 呼び出し直後に C 側がエラー状態を返した場合、
    `FortBaseEstimator._raise_last_error()` により `ValueError("code=...: msg")` が送出されることを確認。
- 確認:
  - 例外文字列に `code=<int>:` とエラーメッセージが含まれる

