# mod_check_finite

- 概要: 実数/配列の有限性チェック（NaN/±Inf を詳細に判別してメッセージ生成）。

- サブルーチン:
  - `check_finite_r64_scalar(a, file, class_name, value_name, fatal)`
  - `check_finite_r64_vec(vec, file, class_name, value_name, fatal)`
  - `check_finite_r64_mat(mat, file, class_name, value_name, fatal)`

