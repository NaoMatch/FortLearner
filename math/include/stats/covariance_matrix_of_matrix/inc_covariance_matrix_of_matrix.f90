subroutine covariance_matrix_of_matrix_r8(cov_mat, matrix, n_rows, n_cols, means_of_matrix)
    implicit none
    real(kind=8), intent(inout) :: cov_mat(n_cols,n_cols)
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    integer(kind=8), intent(in) :: n_rows, n_cols
    real(kind=8), optional      :: means_of_matrix(n_cols)

    integer(kind=8) :: i, j, k, l, n_cols_unroll
    real(kind=8) :: means_of_matrix_opt(n_cols)
    real(kind=8) :: var_vals(n_cols)
    real(kind=8) :: tmp_v1xv2(7), tmp
    include "./include/stats/covariance_matrix_of_matrix/inc_covariance_matrix_of_matrix_detail.f90"
end subroutine covariance_matrix_of_matrix_r8

subroutine covariance_matrix_of_matrix_i4(cov_mat, matrix, n_rows, n_cols, means_of_matrix)
    implicit none
    real(kind=4), intent(inout) :: cov_mat(n_cols,n_cols)
    integer(kind=4), intent(in)    :: matrix(n_rows, n_cols)
    integer(kind=4), intent(in) :: n_rows, n_cols
    real(kind=4), optional      :: means_of_matrix(n_cols)

    integer(kind=4) :: i, j, k, l, n_cols_unroll
    real(kind=4) :: means_of_matrix_opt(n_cols)
    real(kind=4) :: var_vals(n_cols)
    real(kind=4) :: tmp_v1xv2(7), tmp
    include "./include/stats/covariance_matrix_of_matrix/inc_covariance_matrix_of_matrix_detail.f90"
end subroutine covariance_matrix_of_matrix_i4

subroutine covariance_matrix_of_matrix_i8(cov_mat, matrix, n_rows, n_cols, means_of_matrix)
    implicit none
    real(kind=8), intent(inout) :: cov_mat(n_cols,n_cols)
    integer(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    integer(kind=8), intent(in) :: n_rows, n_cols
    real(kind=8), optional      :: means_of_matrix(n_cols)

    integer(kind=8) :: i, j, k, l, n_cols_unroll
    real(kind=8) :: means_of_matrix_opt(n_cols)
    real(kind=8) :: var_vals(n_cols)
    real(kind=8) :: tmp_v1xv2(7), tmp
    include "./include/stats/covariance_matrix_of_matrix/inc_covariance_matrix_of_matrix_detail.f90"
end subroutine covariance_matrix_of_matrix_i8
