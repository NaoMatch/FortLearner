# mod_kmeans_registry

- 概要: KMeans モデルのプール管理。C バインディング層とやり取りするために、ID でモデルを管理。

- グローバル:
  - `kmeans_pool(:)`, `kmeans_used(:)`, `initialized`

- サブルーチン/関数:
  - `initialize_kmeans_registry()`: プール初期化。
  - `extend_pool()`: 使用率に応じて拡張。
  - `kmeans_create_model(n_clusters, max_iter, init, n_init, tol, random_state, num_threads, print_log, chunk_size) -> c_long`: 新規モデルをプール上に作成し ID を返す。

