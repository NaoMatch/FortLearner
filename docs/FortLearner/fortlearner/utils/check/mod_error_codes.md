# mod_error_codes

- 概要: 共通エラーコード（`ERR_*`）の定義。検証・I/O などで使用。

- 代表コード一覧（定義元: `fortlearner/utils/check/mod_error_codes.f90`）
  - `ERR_SUCCESS = 0`: 成功（エラーなし）
  - `ERR_NEGATIVE_VALUE = 10`: 値が負であってはならない
  - `ERR_NON_NEGATIVE_VALUE = 11`: 値が非負であってはならない（負のみ許容）
  - `ERR_POSITIVE_VALUE = 12`: 値が正であってはならない
  - `ERR_NON_POSITIVE_VALUE = 13`: 値が非正であってはならない（正のみ許容）
  - `ERR_BELOW_MIN = 14`: 下限未満
  - `ERR_ABOVE_MAX = 15`: 上限超過
  - `ERR_SHAPE_MISMATCH = 16`: 配列次元/形状の不一致
  - `ERR_NON_FINITE = 17`: `NaN` または `Inf` を含む
  - `ERR_TOO_FEW_ROWS = 18`: 行数が下限を満たさない
  - `ERR_TOO_FEW_COLS = 19`: 列数が下限を満たさない
  - `ERR_RELATION = 20`: 値の関係（例: `min <= max`）が成り立たない
  - `ERR_ROWS_IDENTICAL = 21`: 全行が同一データ
  - `ERR_DISALLOWED_OPTION = 30`: 許可されていないオプション値
  - `ERR_NOT_FITTED = 40`: 学習前に推論系 API を呼んだ
  - `ERR_INVALID_FORMAT_VERSION = 50`: ファイルフォーマットのバージョン不一致
  - `ERR_INVALID_ARGUMENT = 51`: 引数が不正
  - `ERR_FILE_NOT_FOUND = 52`: ファイルが存在しない

- Python 側からの扱い
  - `ctypes` 経由で `get_last_error(code, msg)` / `clear_last_error()` を呼び出し、
    `code != 0` の場合は `ValueError(f"code={code}: {msg}")` を送出します。
  - 詳細は `flearner.fort_base_estimator.FortBaseEstimator._raise_last_error` を参照。

