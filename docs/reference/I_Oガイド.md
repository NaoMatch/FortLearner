# I/O ガイド（dump/load）

バイナリ互換を維持するための基本的な dump/load ルーチン。

## 関数一覧（簡易シグネチャ）

- mod_dump_load
  - `dump_r64(unit: i64, x: r64)` / `load_r64(unit: i64, x: out r64)`
  - `dump_i64(unit: i64, x: i64)` / `load_i64(unit: i64, x: out i64)`
  - `dump_logical(unit: i64, x: logical)` / `load_logical(unit: i64, x: out logical)`
  - 固定長文字列
    - `dump_char_fixed_scalar(unit: i64, s: character(len=MAX_NAME_LEN))`
    - `load_char_fixed_scalar(unit: i64, s: out character(len=MAX_NAME_LEN))`
  - 固定長文字列ベクトル
    - `dump_char_fixed_vec(unit: i64, arr: character(len=MAX_NAME_LEN)(:))`
    - `load_char_fixed_vec(unit: i64, arr: out character(len=MAX_NAME_LEN)(:))`
  - 配列（列優先）
    - `dump_vec(unit: i64, v: r64(:))` / `load_vec(unit: i64, v: out alloc r64(:))`
    - `dump_mat(unit: i64, a: r64(:,:))` / `load_mat(unit: i64, a: out alloc r64(:,:))`

備考: ファイルレベルでは `access='stream', form='unformatted'`, エンディアンは `little_endian` を想定。

## 実装ファイル（モジュール → パス）

- `mod_dump_load` → `fortlearner/utils/io/mod_dump_load.f90`
