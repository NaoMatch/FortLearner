# mod_timer

- 概要: `date_and_time` の値配列から経過時間（ミリ秒精度）を算出する補助。

- 関数:
  - `datetime2second(date_value) -> real(r64)`: 8 要素の日時配列を秒換算（ミリ秒まで）に変換。
  - `time_diff(date_value1, date_value2) -> i64`: 2 つの日時配列の差分（ミリ秒）を返す。

