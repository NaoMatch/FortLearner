subroutine mattxmat_r8(mat_out, mat_in, n_samples, n_columns, with_intercept)
    implicit none
    real(kind=8), ALLOCATABLE, intent(inout) :: mat_out(:,:)
    real(kind=8), intent(in)               :: mat_in(n_samples, n_columns)
    integer(kind=8), intent(in)            :: n_samples, n_columns
    logical(kind=4), intent(in)            :: with_intercept
    real(kind=8), allocatable              :: col_sums(:)
    real(kind=8)                           :: tmp_sums(3), tmp_sum, tmp_val
    integer(kind=8) :: buffer_size=3

    integer(kind=8) :: i, j, k, l, one=1
    integer(kind=8) :: n_columns_unroll, j_unroll
    real(kind=8)    :: zero=0
    include "./include/linalg_mattxmat/inc_linalg_mattxmat_detail.f90"
end subroutine mattxmat_r8

subroutine mattxmat_i4(mat_out, mat_in, n_samples, n_columns, with_intercept)
    implicit none
        integer(kind=4), ALLOCATABLE, intent(inout) :: mat_out(:,:)
        integer(kind=4), intent(in)                 :: mat_in(n_samples, n_columns)
        integer(kind=4), intent(in)                 :: n_samples, n_columns
        logical(kind=4), intent(in)                 :: with_intercept
        integer(kind=4), allocatable                :: col_sums(:)
        integer(kind=4)                             :: tmp_sums(3), tmp_sum, tmp_val
        integer(kind=4) :: buffer_size=3

        integer(kind=4) :: i, j, k, l, one=1
        integer(kind=4) :: n_columns_unroll, j_unroll
        integer(kind=4)    :: zero=0
    include "./include/linalg_mattxmat/inc_linalg_mattxmat_detail.f90"
end subroutine mattxmat_i4

subroutine mattxmat_i8(mat_out, mat_in, n_samples, n_columns, with_intercept)
    implicit none
        integer(kind=8), ALLOCATABLE, intent(inout) :: mat_out(:,:)
        integer(kind=8), intent(in)                 :: mat_in(n_samples, n_columns)
        integer(kind=8), intent(in)                 :: n_samples, n_columns
        logical(kind=4), intent(in)                 :: with_intercept
        integer(kind=8), allocatable                :: col_sums(:)
        integer(kind=8)                             :: tmp_sums(3), tmp_sum, tmp_val
        integer(kind=8) :: buffer_size=3

        integer(kind=8) :: i, j, k, l, one=1
        integer(kind=8) :: n_columns_unroll, j_unroll
        integer(kind=8)    :: zero=0
    include "./include/linalg_mattxmat/inc_linalg_mattxmat_detail.f90"
end subroutine mattxmat_i8
