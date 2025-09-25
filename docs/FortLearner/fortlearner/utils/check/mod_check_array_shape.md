# mod_check_array_shape

- 概要: 行数・列数の下限チェックと厳密形状チェック。

- サブルーチン:
  - `check_min_rows_r64(a, min_rows, file, class_name, value_name, fatal)` / `check_min_rows_i64(...)`
  - `check_min_cols_r64(a, min_cols, file, class_name, value_name, fatal)` / `check_min_cols_i64(...)`
  - `check_exact_shape_r64(a, exp_n_rows, exp_n_cols, file, class_name, value_name, fatal)` / `check_exact_shape_i64(...)`

