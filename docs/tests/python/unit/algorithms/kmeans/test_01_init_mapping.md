# test_01_init_mapping

- 対象: `FortKMeans.__load_dll` と `__init__` の引数マッピング/ctypes シグネチャ
- 目的:
  - 共有ライブラリローダを Fake に差し替え、`kmeans_create` に渡る実値（前処理後）が正しいことを白箱確認
  - `random_state=None → int`、`chunk_size=None → -1` 等の正規化
  - `lib_free` の委譲設定
  - `argtypes`/`restype` は「設定されていれば」仕様どおり（厳格チェックは `FLEAR_STRICT_SIGNATURE=1`）
- 確認:
  - `last_create_args` に `n_clusters,max_iter,n_init,tol,num_threads,print_log,chunk_size,random_state` が期待値で入る
  - Fake の注入により `_lib` が差し替わっている

