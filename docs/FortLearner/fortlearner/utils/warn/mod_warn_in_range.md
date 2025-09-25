# mod_warn_in_range

- 概要: 値が指定範囲外の場合に警告を出力（停止しない）。

- サブルーチン:
  - `warn_in_range_i64(a, lo, hi, file, class_name, value_name)`
  - `warn_ge_i64(a, lo, file, class_name, value_name)` / `warn_le_i64(a, hi, ...)`: 片側制限の薄いラッパ。

