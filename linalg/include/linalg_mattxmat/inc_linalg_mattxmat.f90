subroutine mattxmat_r8(mat_out, mat_in, n_rows, n_cols, with_intercept)
    implicit none
    real(kind=8), allocatable, intent(out) :: mat_out(:,:)
    real(kind=8), intent(in)               :: mat_in(n_rows, n_cols)
    integer(kind=8), intent(in)            :: n_rows, n_cols
    logical(kind=4), intent(in)            :: with_intercept
    real(kind=8), allocatable              :: tmp_sum(:)
    include "./include/linalg_mattxmat/inc_linalg_mattxmat_detail.f90"
end subroutine mattxmat_r8

subroutine mattxmat_i4(mat_out, mat_in, n_rows, n_cols, with_intercept)
    implicit none
    integer(kind=4), allocatable, intent(out) :: mat_out(:,:)
    integer(kind=4), intent(in)               :: mat_in(n_rows, n_cols)
    integer(kind=4), intent(in)               :: n_rows, n_cols
    logical(kind=4), intent(in)               :: with_intercept
    integer(kind=4), allocatable              :: tmp_sum(:)
    include "./include/linalg_mattxmat/inc_linalg_mattxmat_detail.f90"
end subroutine mattxmat_i4

subroutine mattxmat_i8(mat_out, mat_in, n_rows, n_cols, with_intercept)
    implicit none
    integer(kind=8), allocatable, intent(out) :: mat_out(:,:)
    integer(kind=8), intent(in)               :: mat_in(n_rows, n_cols)
    integer(kind=8), intent(in)               :: n_rows, n_cols
    logical(kind=4), intent(in)               :: with_intercept
    integer(kind=8), allocatable              :: tmp_sum(:)
    include "./include/linalg_mattxmat/inc_linalg_mattxmat_detail.f90"
end subroutine mattxmat_i8
