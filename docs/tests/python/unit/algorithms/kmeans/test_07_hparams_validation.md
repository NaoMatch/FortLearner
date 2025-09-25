# test_07_hparams_validation

- 対象: FortKMeans のハイパーパラメータ検証（Cerberus）
- 目的:
  - `__init__` 時に Cerberus によるスキーマ検証が走り、不正な型/値を検出して `ValueError` を送出することを確認。
  - `nullable=True` なフィールド（`random_state`/`chunk_size`）は `None` を受理することを確認。
- 手法:
  - 共有ライブラリは Fake に差し替え（ctypes 呼び出しは到達させるが副作用なし）。
  - Cerberus は実体を使用（未導入環境では `pytest.importorskip('cerberus')` で skip）。
- 確認:
  - 不正型（例: `n_clusters='3'`, `tol='0.1'`, `print_log='yes'` 等）で `ValueError`。
  - 例外メッセージに `Invalid parameters` と該当フィールド名が含まれる。
  - 正常ケースは通過し `_model_id` が設定される。

