# mod_character_helpers

- 概要: 文字列関連の軽量ユーティリティ。

- 関数:
  - `bound_str(bnd, is_low) -> character(:)`（i64/r64）: 境界値を `-inf`/`+inf` を含む整形文字列に。
  - `make_prefix(file, class, name, sep, quote) -> character(:)`: エラーメッセージ接頭辞の組み立て。
  - `join_char_array(list, sep) -> character(:)`: 文字列配列の連結。

