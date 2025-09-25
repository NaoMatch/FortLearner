# test_02_edges_and_concurrency

- 対象: KMeans のエッジ入力形状、非数値入力、`close()` の並行安全性
- 目的:
  - `n=0`/`d=1` の入力でも `predict` 形状/型が仕様どおり、`score` が Python `float`
  - 非数値データは `np.asfortranarray(..., float64)` で `TypeError`/`ValueError`
  - 10 スレッド同時 `close()` でも `free` はちょうど1回、以後 `_h` 参照は `RuntimeError`
- 補足: 共有ライブラリローダ（CDLL/LoadLibrary/PyDLL/numpy.ctypeslib.load_library）を import 前に Fake 化

