# mod_check_allowed_option

- 概要: 文字列の選択肢検証と、選択肢→コード変換を提供。

- 関数:
  - `option_to_code(option, allowed_options) -> i64`: 一致するインデックス（1-based）を返す。一致なしは -1。

- サブルーチン:
  - `check_allowed_option(option, allowed_options, file, class_name, value_name, fatal)`

