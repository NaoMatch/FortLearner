# mod_openblas_threads

- 概要: OpenBLAS のスレッド数取得/設定の C バインディング宣言。

- 関数（C バインディング）:
  - `openblas_get_num_threads() -> c_int`: 現在のスレッド数を取得。
  - `openblas_set_num_threads(n)`: スレッド数を設定。
  - `openblas_set_num_threads_local(n) -> c_int`: ローカルに設定し、以前の値を返す（0.3.27+）。

