# mod_base_estimator

- 概要: すべての推定器の抽象基底型と、学習/推論時の共通入力検証・Dump/Load を提供。

- 抽象型 `type(base_estimator)` フィールド（抜粋）:
  - `name`, `summary(data_summary)`, `is_fitted`, `print_log`, `fatal`, `min_n_rows`, `min_n_cols`

- 型境界/メソッド:
  - `get_class_name() -> character(len=MAX_NAME_LEN)`: クラス名を返す。
  - `check_X_train_common_r64(X, file, class_name, value_name)`: 学習入力の共通チェック。
  - `check_X_predict_common_r64(X, file, class_name, value_name)`: 予測入力の共通チェック。
  - `dump_base_estimator(unit)` / `load_base_estimator(unit)`: メタ/状態の入出力。

