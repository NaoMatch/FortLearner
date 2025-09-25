# mod_base_kmeans

- 概要: KMeans 系の抽象基底。ハイパラの保持・初期化・共通検証・Dump/Load を提供。

- 型 `type(base_kmeans) extends(base_estimator)` フィールド（抜粋）:
  - 事前設定: `n_clusters`, `max_iter`, `n_init`, `tol`, `init`, `random_state`, `num_threads`, `chunk_size`
  - 実行時: `n_cols_`, `n_iter_`, `n_init_`, `tol_`, `init_code_`, `random_state_`, `set_random_state_`, `num_threads_`, `chunk_size_`, `best_score`
  - 学習結果: `centroids(:,:)`, `C_row_sq_norm(:)`

- メソッド:
  - 初期化: `initialize_clusters(centroids, n_clusters, X, n_rows, n_cols)`
    - `initialize_clusters_random(...)`: データからランダム初期中心を選択。
    - `initialize_clusters_kmeanspp(...)`: kmeans++ による初期化。
  - 入力検証: `check_X_train(...)`, `check_X_predict(...)`: 共通検証＋KMeans 特有のチェック/警告。
  - モデル I/O: `dump_base_kmeans(unit)` / `load_base_kmeans(unit)`

