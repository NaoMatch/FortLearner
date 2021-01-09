subroutine cholesky_decomposition_modified_real64(matrix_lower, diagonal_elements, matrix, n_dim)
    implicit none
    real(kind=8), intent(inout) :: matrix_lower(n_dim, n_dim)
    real(kind=8), intent(inout) :: diagonal_elements(n_dim)
    real(kind=8), intent(in)    :: matrix(n_dim, n_dim)
    integer(kind=8), intent(in) :: n_dim
    integer(kind=8)             :: i, j, k
    real(kind=8)                :: tmp_sum, tmp_minus

    include "./include/linalg_cholesky_decomposition_modified/inc_cholesky_decomposition_modified_detail.f90"
end subroutine cholesky_decomposition_modified_real64

subroutine cholesky_decomposition_modified_int32(matrix_lower, diagonal_elements, matrix, n_dim)
    implicit none
    real(kind=4), intent(inout) :: matrix_lower(n_dim, n_dim)
    real(kind=4), intent(inout) :: diagonal_elements(n_dim)
    integer(kind=4), intent(in) :: matrix(n_dim, n_dim)
    integer(kind=4), intent(in) :: n_dim
    integer(kind=4)             :: i, j, k
    real(kind=4)                :: tmp_sum, tmp_minus

    include "./include/linalg_cholesky_decomposition_modified/inc_cholesky_decomposition_modified_detail.f90"
end subroutine cholesky_decomposition_modified_int32

subroutine cholesky_decomposition_modified_int64(matrix_lower, diagonal_elements, matrix, n_dim)
    implicit none
    real(kind=8), intent(inout) :: matrix_lower(n_dim, n_dim)
    real(kind=8), intent(inout) :: diagonal_elements(n_dim)
    integer(kind=8), intent(in) :: matrix(n_dim, n_dim)
    integer(kind=8), intent(in) :: n_dim
    integer(kind=8)             :: i, j, k
    real(kind=8)                :: tmp_sum, tmp_minus

    include "./include/linalg_cholesky_decomposition_modified/inc_cholesky_decomposition_modified_detail.f90"
end subroutine cholesky_decomposition_modified_int64
