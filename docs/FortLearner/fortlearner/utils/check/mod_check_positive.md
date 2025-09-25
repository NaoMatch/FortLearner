# mod_check_positive

- 概要: `> 0` の検証。実数は有限性もチェック。

- サブルーチン:
  - `check_positive_r64(a, file, class_name, value_name, fatal)`: 非有限→エラー、`a <= 0`→エラー。
  - `check_positive_i64(a, file, class_name, value_name, fatal)`: `a <= 0`→エラー。

