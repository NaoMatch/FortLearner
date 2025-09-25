# mod_dump_load

- 概要: ストリーム・アンフォーマット I/O による基本型・配列の dump/load ユーティリティ。

- サブルーチン（型別 I/O）:
  - `dump_r64(unit, x)` / `load_r64(unit, x)`
  - `dump_i64(unit, x)` / `load_i64(unit, x)`
  - `dump_logical(unit, x)` / `load_logical(unit, x)`

- サブルーチン（文字列）:
  - `dump_char_fixed_scalar(unit, s)` / `load_char_fixed_scalar(unit, s)`: 固定長 1 個。
  - `dump_char_fixed_vec(unit, arr)` / `load_char_fixed_vec(unit, arr)`: 固定長配列と要素数メタ。

- サブルーチン（配列）:
  - `dump_vec(unit, v)` / `load_vec(unit, v)`: 1D 実配列（要素数メタ付）。
  - `dump_mat(unit, a)` / `load_mat(unit, a)`: 2D 実配列（行数・列数メタ付）。

