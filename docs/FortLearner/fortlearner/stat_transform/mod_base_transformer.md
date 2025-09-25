# mod_base_transformer

- 概要: 前処理器の抽象基底型（`base_estimator` を拡張）。学習/推論の入力検証と Dump/Load を提供。

- 抽象型 `type(base_transformer) extends(base_estimator)` フィールド:
  - `stable`: 数値安定計算を選ぶフラグ。

- 型境界/メソッド:
  - `check_X_train(X, file, class_name, value_name)`: 前処理学習の入力検証（下限設定→共通チェック）。
  - `check_X_predict(X, file, class_name, value_name)`: 学習済み検証→共通チェック。
  - `dump_base_transformer(unit)` / `load_base_transformer(unit)`

