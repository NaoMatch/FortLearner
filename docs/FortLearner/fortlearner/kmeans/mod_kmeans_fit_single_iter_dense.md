# mod_kmeans_fit_single_iter_dense

- 概要: KMeans の 1 反復（全行/チャンク）の実装。距離計算→ラベル更新→重心更新→収束判定までを担当。

- 関数:
  - `fit_single_iter_whole_dense(...) -> logical`: 全行の距離行列で 1 反復実行。収束（ラベル不変/重心変化 ≤ tol）なら `.true.`。
  - `fit_single_iter_chunk_dense(...) -> logical`: 行チャンク分割で同様の処理を行う。

- 入出力の主な役割:
  - 入力: `X`, `centroids_*`, `C_row_sq_norm`, 既存ラベル等
  - 出力: 更新後の重心/ラベル/カウンタ、反復番号 `n_iter_`、収束フラグ

