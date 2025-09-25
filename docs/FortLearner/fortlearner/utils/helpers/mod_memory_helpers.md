# mod_memory_helpers

- 概要: 可変配列の安全な解放ヘルパ（割当済みなら deallocate）。

- サブルーチン:
  - `force_deallocate_vec_i64(vec)` / `force_deallocate_vec_r64(vec)`
  - `force_deallocate_mat_i64(mat)` / `force_deallocate_mat_r64(mat)`

