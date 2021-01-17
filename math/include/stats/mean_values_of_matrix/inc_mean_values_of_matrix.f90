function mean_values_of_matrix_r8(matrix, n_rows, n_cols)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    integer(kind=8), intent(in) :: n_rows, n_cols
    real(kind=8)                :: mean_values_of_matrix_r8(n_cols)

    integer(kind=8) :: i, j, k
    real(kind=8)    :: tmp_sum
    integer(kind=8) :: n_cols_unroll
    real(kind=8)    :: tmp_sums(7)
    real(kind=8)    :: tmp_means(n_cols), tmp_inv
    tmp_inv = 1.0 / dble(n_rows)

    include "./include/stats/mean_values_of_matrix/inc_mean_values_of_matrix_detail.f90"
    mean_values_of_matrix_r8 = tmp_means
end function mean_values_of_matrix_r8

function mean_values_of_matrix_i4(matrix, n_rows, n_cols)
    implicit none
    integer(kind=4), intent(in)    :: matrix(n_rows, n_cols)
    integer(kind=4), intent(in) :: n_rows, n_cols
    real(kind=4)                :: mean_values_of_matrix_i4(n_cols)

    integer(kind=4) :: i, j, k
    real(kind=4)    :: tmp_sum
    integer(kind=4) :: n_cols_unroll
    real(kind=4)    :: tmp_sums(7)
    real(kind=4)    :: tmp_means(n_cols), tmp_inv
    tmp_inv = 1.0 / float(n_rows)

    n_cols_unroll = n_cols - mod(n_cols, 7)
    do j=1, n_cols_unroll, 7
        tmp_sums = 0
        do i=1, n_rows
            do k=0, 7-1, 1
                tmp_sums(k+1) = tmp_sums(k+1) + float(matrix(i,j+k))
            end do
        end do

        do k=0, 7-1, 1
            tmp_means(j+k) = tmp_sums(k+1) * tmp_inv
        end do
    end do

    do j=n_cols_unroll+1, n_cols
        tmp_sum = 0d0
        do i=1, n_rows
            tmp_sum = tmp_sum + float(matrix(i,j))
        end do
        tmp_means(j) = tmp_sum * tmp_inv
    end do
    mean_values_of_matrix_i4 = tmp_means
end function mean_values_of_matrix_i4

function mean_values_of_matrix_i8(matrix, n_rows, n_cols)
    implicit none
    integer(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    integer(kind=8), intent(in) :: n_rows, n_cols
    real(kind=8)                :: mean_values_of_matrix_i8(n_cols)

    integer(kind=8) :: i, j, k
    real(kind=8)    :: tmp_sum
    integer(kind=8) :: n_cols_unroll
    real(kind=8)    :: tmp_sums(7)
    real(kind=8)    :: tmp_means(n_cols), tmp_inv
    tmp_inv = 1.0 / dble(n_rows)

    n_cols_unroll = n_cols - mod(n_cols, 7)
    do j=1, n_cols_unroll, 7
        tmp_sums = 0
        do i=1, n_rows
            do k=0, 7-1, 1
                tmp_sums(k+1) = tmp_sums(k+1) + dble(matrix(i,j+k))
            end do
        end do

        do k=0, 7-1, 1
            tmp_means(j+k) = tmp_sums(k+1) * tmp_inv
        end do
    end do

    do j=n_cols_unroll+1, n_cols
        tmp_sum = 0d0
        do i=1, n_rows
            tmp_sum = tmp_sum + dble(matrix(i,j))
        end do
        tmp_means(j) = tmp_sum * tmp_inv
    end do
    mean_values_of_matrix_i8 = tmp_means
end function mean_values_of_matrix_i8
