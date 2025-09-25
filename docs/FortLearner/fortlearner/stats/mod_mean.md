# mod_mean

- 概要: ベクトル/行列/軸方向の平均を安定版/通常版で計算する実装と、入力の基本検証を提供。

- サブルーチン（ベクトル）:
  - `mean_vec_r64(mean, vec, n, status, stable, do_check)`
  - `mean_vec_canonical_r64(mean, vec, n)` / `mean_vec_stable_r64(mean, vec, n)`

- サブルーチン（行列 全体平均）:
  - `mean_mat_r64(mean, mat, n, m, status, stable, do_check)`
  - `mean_mat_canonical_r64(mean, mat, n, m)` / `mean_mat_stable_r64(mean, mat, n, m)`

- サブルーチン（行列 軸平均）:
  - `mean_mat_axis_r64(mean(:), mat, n, m, axis, status, stable, do_check)`
  - `mean_mat_axis1_canonical_r64` / `mean_mat_axis1_stable_r64`
  - `mean_mat_axis2_canonical_r64` / `mean_mat_axis2_stable_r64`

