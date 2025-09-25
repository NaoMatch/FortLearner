# mod_random_seed

- 概要: 再現性のための乱数シード設定と解放。Marsaglia/ツァンの 32bit 整数生成器も同梱。

- サブルーチン/関数:
  - `random_int_32_scalar(jsr, iran)`: 32bit 整数を生成（内部状態 `jsr` 更新）。
  - `fix_random_seed(iseed)`: 単一整数から `random_seed(put=...)` 用の配列を生成して設定。
  - `release_random_seed()`: 現在時刻ベースで乱数シードを再設定。

