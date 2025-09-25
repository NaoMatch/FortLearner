# mod_kmeans_binding

- 概要: C から呼び出すための KMeans API（create/fit/predict/score/dump/load/free）。

- 補助:
  - `c_ptr_to_fstring(cptr, fstr)`: C の文字列ポインタを Fortran 文字列に変換。

- C で公開される関数（bind C）:
  - `kmeans_create(...) -> int64`: モデル生成（ハイパラ引数を受け取り Fortran 側へ橋渡し）。
  - `kmeans_fit(model_id, X, n_rows, n_cols)` / `kmeans_predict(...)` / `kmeans_score(...)`
  - `kmeans_dump(model_id, file_name)` / `kmeans_load(file_name) -> int64` / `kmeans_free(model_id)`

