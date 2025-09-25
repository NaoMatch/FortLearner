# test_fort_base_estimator_hparams_error

- 対象: `FortBaseEstimator` のパラメータ検証/エラー正規化
- 目的:
  - `_validate_hparams()` が Validator 成功時は例外にならないこと
  - Validator 失敗時に `ValueError("Invalid parameters ...")` を投げ、`errors` 内容を含むこと
  - `_raise_last_error()` が `get_last_error` の code!=0 で `ValueError(code: msg)` を投げ、code=0 では no-op
- 補足: 共有ライブラリは Fake に差し替え、`cerberus` が無い環境でもダミーを注入

