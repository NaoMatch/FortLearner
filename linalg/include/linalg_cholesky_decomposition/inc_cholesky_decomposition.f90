subroutine cholesky_decomposition_r8(matrix_lower, matrix, n_dim)
    implicit none
    real(kind=8), intent(inout) :: matrix_lower(n_dim, n_dim)
    real(kind=8), intent(in)    :: matrix(n_dim, n_dim)
    integer(kind=8), intent(in) :: n_dim
    integer(kind=8)             :: i, j, k
    real(kind=8)                :: tmp_sum

    include "./include/linalg_cholesky_decomposition/inc_cholesky_decomposition_detail.f90"
end subroutine cholesky_decomposition_r8

subroutine cholesky_decomposition_i4(matrix_lower, matrix, n_dim)
    implicit none
    real(kind=4), intent(inout) :: matrix_lower(n_dim, n_dim)
    integer(kind=4), intent(in) :: matrix(n_dim, n_dim)
    integer(kind=4), intent(in) :: n_dim
    integer(kind=4)             :: i, j, k
    real(kind=4)                :: tmp_sum

    include "./include/linalg_cholesky_decomposition/inc_cholesky_decomposition_detail.f90"
end subroutine cholesky_decomposition_i4

subroutine cholesky_decomposition_i8(matrix_lower, matrix, n_dim)
    implicit none
    real(kind=8), intent(inout) :: matrix_lower(n_dim, n_dim)
    integer(kind=8), intent(in) :: matrix(n_dim, n_dim)
    integer(kind=8), intent(in) :: n_dim
    integer(kind=8)             :: i, j, k
    real(kind=8)                :: tmp_sum

    include "./include/linalg_cholesky_decomposition/inc_cholesky_decomposition_detail.f90"
end subroutine cholesky_decomposition_i8


