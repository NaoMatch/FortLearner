# Changelog

## 0.1.0 (未リリース)
- ステータス: リリース準備中（TestPyPI での検証を実施中）
- Python packaging (wheel/sdist) and bundled shared library (Linux)
- KMeans: fit/predict/score; dump/load via Python bindings
- Safer resource management and robust ctypes bindings
- Integration tests; CI for wheels and TestPyPI publishing
- Added: `make bench-search`（探索ベンチ, `demos/fortran/benchmarks/bench_binary_search.f90`）
- Added: `make bench-weighted`（重み付きサンプリングベンチ, `demos/fortran/benchmarks/bench_weighted_sampling.f90`）
- Added: `make bench`（bench-search と bench-weighted を連続実行）
- Added: `make verify-prefix-sum` ターゲットと prefix sum ベースの重み付きサンプリング検証 (`demos/fortran/verification/verify_prefix_sum_selection.f90`)
- Changed: KMeans++ 初期化の重み付きサンプリングをリザーバ方式から prefix-sum (`mod_prefix_sum_selection`) へ移行
