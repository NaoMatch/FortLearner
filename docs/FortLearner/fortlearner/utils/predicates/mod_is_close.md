# mod_is_close

- 概要: 実数の近傍判定と 0/1 判定の述語関数群。

- 関数:
  - `is_close(x, y, rtol, atol) -> logical`: `|x-y| <= atol + rtol*max(|x|,|y|)` に基づく等価判定。
  - `is_zero(x, atol) -> logical`: `is_close(x, 0)` の簡便版（厳しめの `atol` 既定）。
  - `is_one(x, rtol, atol) -> logical`: `is_close(x, 1)` の簡便版。

