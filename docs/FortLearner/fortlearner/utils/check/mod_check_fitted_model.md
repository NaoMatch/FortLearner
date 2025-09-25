# mod_check_fitted_model

- 概要: 推論系 API の前提として「学習済みであること」を検証。

- サブルーチン:
  - `check_fitted_model(is_fitted, file, class_name, func_name, fatal)`: 未学習なら `ERR_NOT_FITTED` を送出。

