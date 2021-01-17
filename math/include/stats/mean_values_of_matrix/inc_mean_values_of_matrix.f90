function mean_values_of_matrix_r8(matrix, n_samples, n_columns)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    integer(kind=8), intent(in) :: n_samples, n_columns
    real(kind=8)                :: mean_values_of_matrix_r8(n_columns)

    integer(kind=8) :: i, j, k
    real(kind=8)    :: tmp_sum
    integer(kind=8) :: n_columns_unroll
    real(kind=8)    :: tmp_sums(7)
    real(kind=8)    :: tmp_means(n_columns), tmp_inv
    include "./include/stats/mean_values_of_matrix/inc_mean_values_of_matrix_detail.f90"
    mean_values_of_matrix_r8 = tmp_means
end function mean_values_of_matrix_r8

function mean_values_of_matrix_i4(matrix, n_samples, n_columns)
    implicit none
    integer(kind=4), intent(in)    :: matrix(n_samples, n_columns)
    integer(kind=4), intent(in) :: n_samples, n_columns
    real(kind=4)                :: mean_values_of_matrix_i4(n_columns)

    integer(kind=4) :: i, j, k
    real(kind=4)    :: tmp_sum
    integer(kind=4) :: n_columns_unroll
    integer(kind=4)    :: tmp_sums(7)
    real(kind=4)    :: tmp_means(n_columns), tmp_inv
    include "./include/stats/mean_values_of_matrix/inc_mean_values_of_matrix_detail.f90"
    mean_values_of_matrix_i4 = tmp_means
end function mean_values_of_matrix_i4

function mean_values_of_matrix_i8(matrix, n_samples, n_columns)
    implicit none
    integer(kind=8), intent(in)    :: matrix(n_samples, n_columns)
    integer(kind=8), intent(in) :: n_samples, n_columns
    real(kind=8)                :: mean_values_of_matrix_i8(n_columns)

    integer(kind=8) :: i, j, k
    real(kind=8)    :: tmp_sum
    integer(kind=8) :: n_columns_unroll
    integer(kind=8)    :: tmp_sums(7)
    real(kind=8)    :: tmp_means(n_columns), tmp_inv
    include "./include/stats/mean_values_of_matrix/inc_mean_values_of_matrix_detail.f90"
    mean_values_of_matrix_i8 = tmp_means
end function mean_values_of_matrix_i8
