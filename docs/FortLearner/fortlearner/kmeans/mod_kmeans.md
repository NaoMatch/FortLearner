# mod_kmeans

- 概要: 具体的な KMeans 実装。学習/予測/スコア計算とモデルの dump/load を提供。

- 型 `type(kmeans) extends(base_kmeans)` メソッド:
  - `fit(X, init_C)`: 反復ループを回して重心を最適化。`n_init>1` の場合は最良実行を選択。
  - `predict(X) -> labels(n_rows,1)`: 予測ラベルを返す。
  - `score(X) -> real(r64)`: SSE（距離の最小列合計 + `sum(X**2)`）。
  - `dump(filename)` / `load(filename)`: モデルの保存/復元。

- 関数:
  - `new_kmeans(n_clusters, max_iter, init, n_init, tol, random_state, num_threads, print_log, chunk_size, fatal) -> kmeans`
    - 主要ハイパラの検証（`check_*`）と初期化コードの決定（`option_to_code`）。

