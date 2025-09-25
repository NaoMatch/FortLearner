# mod_euclidean_distance

- 概要: BLAS (dgemv/dgemm) を用いた二乗ユークリッド距離の高速計算。

- サブルーチン:
  - `euclidean_distance_mv(m, k, distances, mat, mat_row_sq_norm, vec, vec_sq_norm)`:
    行列 vs ベクトルの距離（各行）を計算。
  - `euclidean_distance_mm(m, k, l, mat_dist, mat_A, mat_A_row_sq_norm, mat_B, mat_B_row_sq_norm)`:
    行列 vs 行列（行×行）の距離行列を計算。
  - `euclidean_distance_mm_chunk(n_rows, k, l, i_start, i_end, cs, dist_buf, mat_A, mat_A_row_sq_norm, mat_B, mat_B_row_sq_norm)`:
    行チャンク分割版。
  - `euclidean_distance_mm_bnorm(m, k, l, mat_dist, mat_A, mat_B, mat_B_row_sq_norm)`:
    事前計算済み `|b_j|^2` をブロードキャストして活用する版。
  - `euclidean_distance_mm_bnorm_chunk(n_rows, k, l, i_start, i_end, cs, dist_buf, mat_A, mat_B, mat_B_row_sq_norm)`:
    上記のチャンク分割版。

