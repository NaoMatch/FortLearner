# mod_weighted_reservoir

- 概要: 重み付きリザーバサンプリング（Efraimidis-Spirakis）の 1 サンプル版。KMeans++ 初期化では `mod_prefix_sum_selection` に置き換え済みのため、新規利用は推奨しません。

- 関数:
  - `weighted_reservoir_single(fitness, n_rows) -> i64`: 正の重みから 1 行インデックスをサンプル（レガシー用途）。
