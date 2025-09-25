# mod_prefix_sum

- 概要: 実数・整数ベクトルに対して包含的な prefix sum（累積和）を計算するユーティリティ。

- 関数:
  - `prefix_sum_r64(vec_in, vec_out, vec_size)`
  - `prefix_sum_i64(vec_in, vec_out, vec_size)`

- 備考:
  - `vec_size` が 0 以下になるケースは呼び出し側で除外する前提です。
  - `vec_out` は `vec_in` と同じ長さで割り当ててください。
