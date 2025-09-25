# mod_error_bindings

- 概要: `mod_error_manager` の状態を C から取得/クリアするためのバインディング補助。

- 関数/サブルーチン:
  - `c_strlen(str) -> integer`: C 文字列長をカウント。
  - `get_last_error(code, msg)` (bind C): エラーコードとメッセージを C バッファにコピー。
  - `clear_last_error()` (bind C): 内部状態のリセット。

