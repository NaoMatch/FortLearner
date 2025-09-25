module mod_error_codes
    use :: mod_kinds
    implicit none

    ! --- 共通エラーコード (0–255) -------------------------------
    integer(i64), parameter :: ERR_SUCCESS        = 0

    integer(i64), parameter :: ERR_NEGATIVE_VALUE     = 10   ! 負値は禁止
    integer(i64), parameter :: ERR_NON_NEGATIVE_VALUE = 11   ! 非負値は禁止
    integer(i64), parameter :: ERR_POSITIVE_VALUE     = 12   ! 正値は禁止
    integer(i64), parameter :: ERR_NON_POSITIVE_VALUE = 13   ! 非正値は禁止
    integer(i64), parameter :: ERR_BELOW_MIN      = 14   ! 下限未満
    integer(i64), parameter :: ERR_ABOVE_MAX      = 15   ! 上限超過
    integer(i64), parameter :: ERR_SHAPE_MISMATCH = 16   ! 配列次元不整合
    integer(i64), parameter :: ERR_NON_FINITE     = 17   ! NaN or Inf
    integer(i64), parameter :: ERR_TOO_FEW_ROWS   = 18   ! 最小行数
    integer(i64), parameter :: ERR_TOO_FEW_COLS   = 19   ! 最小列数
    integer(i64), parameter :: ERR_RELATION       = 20   ! 二つの値の関係性エラー
    integer(i64), parameter :: ERR_ROWS_IDENTICAL = 21   ! 全行同一データ

    integer(i64), parameter :: ERR_DISALLOWED_OPTION = 30   ! 不正なオプション
    
    integer(i64), parameter :: ERR_NOT_FITTED     = 40  ! Not Fitted Error

    integer(i64), parameter :: ERR_INVALID_FORMAT_VERSION   = 50  ! Format error
    
    integer(i64), parameter :: ERR_INVALID_ARGUMENT   = 51  ! Format error
    integer(i64), parameter :: ERR_FILE_NOT_FOUND     = 52  ! Format error


end module mod_error_codes
