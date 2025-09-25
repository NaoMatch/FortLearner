# mod_check_in_range

- 概要: 値が範囲に入っていることの検証（上下限いずれか/両方指定に対応）。

- サブルーチン:
  - `check_in_range_i64(a, lo, hi, file, class_name, value_name, fatal)`
  - `check_greater_equal_i64(a, lo, file, class_name, value_name, fatal)`
  - `check_less_equal_i64(a, hi, file, class_name, value_name, fatal)`

