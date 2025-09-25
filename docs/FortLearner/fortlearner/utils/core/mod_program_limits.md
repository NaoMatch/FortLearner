# mod_program_limits

- 概要: しきい値やサイズ等のグローバル定数を定義します。

- 主な定数:
  - `MAX_NAME_LEN`/`MAX_MSG_LEN`/`MAX_PARAM_LEN`: 文字列長の上限
  - `ATOL_R64`/`RTOL_R64`: 実数比較の既定許容誤差
  - `ATOL_MEAN_WARN_R64`: 平均の警告閾値
  - `MAX_ARRAY_SIZE_GB`: 距離行列等に対するサイズ上限（GiB）
  - `HUGE_R64`/`SAFE_FACTOR_R64`, `HUGE_I64`/`SAFE_FACTOR_I64`: オーバーフロー警戒の閾値・係数

関数・サブルーチンは持ちません。

