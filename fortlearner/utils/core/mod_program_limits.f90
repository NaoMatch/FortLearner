module mod_program_limits
    use :: mod_kinds
    implicit none

    ! Maximum class name length
    integer(i64), parameter :: MAX_NAME_LEN = 64

    ! Maximum message length
    integer(i64), parameter :: MAX_MSG_LEN = 512

    ! Maximum hyperparameter name length
    integer(i64), parameter :: MAX_PARAM_LEN = 64

    ! Tolerance, absolute and relative
    real(r64), parameter :: ATOL_R64 = 1.0e-10_r64   ! ゼロ近傍の実用閾値
    real(r64), parameter :: RTOL_R64 = 1.0e-6_r64    ! 相対 20 bit 相当

    ! Tolerance for mean center
    real(r64), parameter :: ATOL_MEAN_WARN_R64 = 1.0e-3_r64                   ! 平均警告用

    ! Maximum array size in GB
    integer(i64), parameter :: MAX_ARRAY_SIZE_GB = 4_i64 * 1024_i64 * 1024_i64 * 1024_i64

    ! Huge value and Safe factor
    real(r64), parameter :: HUGE_R64 = huge(1.0_r64)
    real(r64), parameter :: SAFE_FACTOR_R64 = 0.5_r64

    integer(i64), parameter :: HUGE_I64 = huge(1_i64)
    integer(i64), parameter :: SAFE_FACTOR_I64 = 2_i64

end module mod_program_limits
