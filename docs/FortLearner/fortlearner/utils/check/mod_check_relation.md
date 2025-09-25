# mod_check_relation

- 概要: 2 値の関係（==, !=, >=, >, <=, <）を検証。実数は `is_close` を用いた近傍比較に対応。

- サブルーチン:
  - `check_eq(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)`（r64/i64）
  - `check_ne(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)`（r64/i64）
  - `check_ge(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)`（r64/i64）
  - `check_gt(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)`（r64/i64）
  - `check_le(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)`（r64/i64）
  - `check_lt(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)`（r64/i64）

