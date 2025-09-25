# mod_error_manager

- 概要: 直近のエラーコード/メッセージの保持と `raise_error` を提供。

- サブルーチン/関数:
  - `raise_error(code, msg, fatal)`: 状態更新・必要なら `error stop`。
  - `set_last_error_msg(msg)`: 内部/ C 向け文字列バッファへ設定。
  - 変数: `last_error_code`, `last_error_msg_internal`, `last_error_msg_c(:)`

