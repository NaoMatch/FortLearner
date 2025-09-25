# mod_c_to_f

- 概要: C の `char*` を Fortran 文字列に変換する補助。

- 関数:
  - `c_str_to_f(cstr) -> character(:)`: null 終端までの長さを数えて可変長の Fortran 文字列に変換。

