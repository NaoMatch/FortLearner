# mod_mean_centerer

- 概要: 各列の平均を引くセンタリング変換器。

- 型:
  - `type(mean_centerer) extends(base_transformer)`
    - フィールド: `mean(:)`
    - メソッド:
      - `fit(X)`: 列平均を計算して内部に保持。
      - `transform(X)`: 行方向に列平均を引き、`X` を原位でセンタリング。

- 関数:
  - `new_mean_centerer(stable) -> mean_centerer`: オプションで安定版を選択。

