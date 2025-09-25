# ftest_kmeans_valid_init_quality

- 対象: 初期化品質（random vs k-means++）
- 目的: `k-means++` による初期セントロイドの方が SSE が小さいこと
- 確認: `SSE(k-means++) < SSE(random)` で合格

