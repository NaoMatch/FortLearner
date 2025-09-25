# mod_check_rows_distinct

- 概要: 行列の全行が同一という退化ケースを検出。

- サブルーチン:
  - `check_rows_distinct(a, file, class_name, value_name, fatal)`: `all(a(i,:)==a(1,:))` を検出しエラー。

