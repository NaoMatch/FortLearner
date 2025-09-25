# Fortran/KMeans テスト概要

Fortran 実装の KMeans に対するテスト群です。パラメータ検証（異常系）から、収束性/決定性などの正常系までをカバーします。

- 異常系（エラーコード検証）
  - ftest_kmeans_invalid_chunk_size.md
  - ftest_kmeans_invalid_n_init.md
  - ftest_kmeans_invalid_n_clusters.md
  - ftest_kmeans_invalid_not_fitted.md
  - ftest_kmeans_invalid_non_finite.md
  - ftest_kmeans_invalid_max_iter.md
  - ftest_kmeans_invalid_init.md
  - ftest_kmeans_invalid_format_mismatch.md
  - ftest_kmeans_invalid_tol_negative.md
  - ftest_kmeans_invalid_tol_nan.md
  - ftest_kmeans_invalid_tol_inf.md
- 正常系（性質/品質の検証）
  - ftest_kmeans_valid_init_quality.md
  - ftest_kmeans_valid_mean_centerer_consistency.md
  - ftest_kmeans_valid_max_iter_cap.md
  - ftest_kmeans_valid_monotone_inertia.md
  - ftest_kmeans_valid_random_state_consistency.md

