function variance_values_of_matrix_r8(matrix, n_rows, n_cols, means_of_matrix)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    integer(kind=8), intent(in) :: n_rows, n_cols
    real(kind=8)                :: variance_values_of_matrix_r8(n_cols)
    real(kind=8), optional      :: means_of_matrix(n_cols)

    integer(kind=8) :: i, j, k
    real(kind=8)    :: tmp_sq_sum, means_of_matrix_opt(n_cols)
    integer(kind=8) :: n_cols_unroll, buffer_len=7
    real(kind=8)    :: tmp_sq_sums(7), val
    real(kind=8)    :: tmp_variances(n_cols), tmp_inv, diff_factor

    include "./include/stats/variance_values_of_matrix/inc_variance_values_of_matrix_detail.f90"
    variance_values_of_matrix_r8 = tmp_variances
end function variance_values_of_matrix_r8

function variance_values_of_matrix_i4(matrix, n_rows, n_cols, means_of_matrix)
    implicit none
    integer(kind=4), intent(in)    :: matrix(n_rows, n_cols)
    integer(kind=4), intent(in) :: n_rows, n_cols
    real(kind=4)                :: variance_values_of_matrix_i4(n_cols)
    real(kind=4), optional      :: means_of_matrix(n_cols)

    integer(kind=4) :: i, j, k
    real(kind=4)    :: tmp_sq_sum, means_of_matrix_opt(n_cols)
    integer(kind=4) :: n_cols_unroll, buffer_len=7
    real(kind=4)    :: tmp_sq_sums(7), val
    real(kind=4)    :: tmp_variances(n_cols), tmp_inv

    include "./include/stats/variance_values_of_matrix/inc_variance_values_of_matrix_detail.f90"
    variance_values_of_matrix_i4 = tmp_variances
end function variance_values_of_matrix_i4

function variance_values_of_matrix_i8(matrix, n_rows, n_cols, means_of_matrix)
    implicit none
    integer(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    integer(kind=8), intent(in) :: n_rows, n_cols
    real(kind=8)                :: variance_values_of_matrix_i8(n_cols)
    real(kind=8), optional      :: means_of_matrix(n_cols)

    integer(kind=8) :: i, j, k
    real(kind=8)    :: tmp_sq_sum, means_of_matrix_opt(n_cols)
    integer(kind=8) :: n_cols_unroll, buffer_len=7
    real(kind=8)    :: tmp_sq_sums(7), val
    real(kind=8)    :: tmp_variances(n_cols), tmp_inv

    include "./include/stats/variance_values_of_matrix/inc_variance_values_of_matrix_detail.f90"
    variance_values_of_matrix_i8 = tmp_variances
end function variance_values_of_matrix_i8
