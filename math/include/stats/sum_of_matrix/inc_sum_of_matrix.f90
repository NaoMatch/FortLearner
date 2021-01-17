subroutine sum_of_matrix_r8(sum_mat, matrix, n_samples, n_columns, dim)
    implicit none
    real(kind=8), allocatable, intent(inout) :: sum_mat(:)
    real(kind=8), intent(in)                 :: matrix(n_samples, n_columns)
    integer(kind=8), intent(in)              :: n_samples, n_columns
    integer(kind=8), intent(in)              :: dim

    integer(kind=8) :: i, j, k, one=1, two=2
    real(kind=8) :: tmp_sum, zero=0.0
    include "./include/stats/sum_of_matrix/inc_sum_of_matrix_detail.f90"
end subroutine sum_of_matrix_r8

subroutine sum_of_matrix_i4(sum_mat, matrix, n_samples, n_columns, dim)
    implicit none
    integer(kind=4), allocatable, intent(inout) :: sum_mat(:)
    integer(kind=4), intent(in)                 :: matrix(n_samples, n_columns)
    integer(kind=4), intent(in)                 :: n_samples, n_columns
    integer(kind=4), intent(in)                 :: dim

    integer(kind=4) :: i, j, k, one=1, two=2
    integer(kind=4) :: tmp_sum, zero=0.0
    include "./include/stats/sum_of_matrix/inc_sum_of_matrix_detail.f90"
end subroutine sum_of_matrix_i4

subroutine sum_of_matrix_i8(sum_mat, matrix, n_samples, n_columns, dim)
    implicit none
    integer(kind=8), allocatable, intent(inout) :: sum_mat(:)
    integer(kind=8), intent(in)                 :: matrix(n_samples, n_columns)
    integer(kind=8), intent(in)                 :: n_samples, n_columns
    integer(kind=8), intent(in)                 :: dim

    integer(kind=8) :: i, j, k, one=1, two=2
    integer(kind=8) :: tmp_sum, zero=0.0
    include "./include/stats/sum_of_matrix/inc_sum_of_matrix_detail.f90"
end subroutine sum_of_matrix_i8

