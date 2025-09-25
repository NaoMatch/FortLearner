# 安定性マトリクス（Python/Fortran）

## Python

- Stable
  - `flearner.FortKMeans.FortKMeans`
    - `fit`, `predict`, `score`, `dump`, `load`
- Internal
  - `flearner.fort_base_estimator`
  - `flearner.handle_owner`
  - サブモジュールの関数・属性（先頭 `_` を含むもの）

## Fortran（Python公開境界）

- Stable（C バインディング経由で公開）
  - `kmeans_create`, `kmeans_fit`, `kmeans_predict`, `kmeans_score`, `kmeans_free`, `kmeans_dump`, `kmeans_load`
  - `get_last_error`, `clear_last_error`
- Internal（直接 `use` する側の API）
  - `mod_kmeans`, `mod_kmeans_registry`, `mod_check`, `mod_error_manager`, `mod_error_codes` ほか

互換性ポリシー:
- Stable の破壊的変更はマイナーバージョンでは行いません。
- Internal は変更/削除の可能性があり、リリースノートで簡易告知のみ行います。
