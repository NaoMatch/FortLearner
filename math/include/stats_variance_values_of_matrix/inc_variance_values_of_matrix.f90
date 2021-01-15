function variance_values_of_matrix_real64(matrix, n_rows, n_cols, means_of_matrix)
    implicit none
    real(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    integer(kind=8), intent(in) :: n_rows, n_cols
    real(kind=8)                :: variance_values_of_matrix_real64(n_cols)
    real(kind=8), optional      :: means_of_matrix(n_cols)

    integer(kind=8) :: i, j, k
    real(kind=8)    :: tmp_sq_sum, means_of_matrix_opt(n_cols)
    integer(kind=8) :: n_cols_unroll
    real(kind=8)    :: tmp_sq_sums(7)
    real(kind=8)    :: tmp_variances(n_cols), tmp_inv, diff_factor

    if (present(means_of_matrix)) then
        means_of_matrix_opt = means_of_matrix
    else
        means_of_matrix_opt = mean(matrix, n_rows, n_cols)
    end if
    tmp_inv = 1d0 / dble(n_rows)

    n_cols_unroll = n_cols - mod(n_cols, 7)
    do j=1, n_cols_unroll, 7
        tmp_sq_sums = 0
        do i=1, n_rows
            do k=0, 7-1, 1
                tmp_sq_sums(k+1) = tmp_sq_sums(k+1) + (matrix(i,j+k) - means_of_matrix_opt(j+k)) ** 2d0
            end do
        end do

        do k=0, 7-1, 1
            tmp_variances(j+k) = tmp_sq_sums(k+1) * tmp_inv
        end do
    end do
    do j=n_cols_unroll+1, n_cols
        tmp_sq_sum = 0
        do i=1, n_rows
            tmp_sq_sum = tmp_sq_sum + (matrix(i,j) - means_of_matrix_opt(j)) ** 2d0
        end do
        tmp_variances(j) = tmp_sq_sum * tmp_inv
    end do
    variance_values_of_matrix_real64 = tmp_variances
end function variance_values_of_matrix_real64

function variance_values_of_matrix_int32(matrix, n_rows, n_cols, means_of_matrix)
    implicit none
    integer(kind=4), intent(in)    :: matrix(n_rows, n_cols)
    integer(kind=4), intent(in) :: n_rows, n_cols
    real(kind=4)                :: variance_values_of_matrix_int32(n_cols)
    real(kind=4), optional      :: means_of_matrix(n_cols)

    integer(kind=4) :: i, j, k
    real(kind=4)    :: tmp_sq_sum, means_of_matrix_opt(n_cols)
    integer(kind=4) :: n_cols_unroll
    real(kind=4)    :: tmp_sq_sums(7)
    real(kind=4)    :: tmp_variances(n_cols), tmp_inv

    tmp_inv = 1.0 / float(n_rows)

    if (present(means_of_matrix)) then
        means_of_matrix_opt = means_of_matrix
    else
        means_of_matrix_opt = mean(matrix, n_rows, n_cols)
    end if

    n_cols_unroll = n_cols - mod(n_cols, 7)
    do j=1, n_cols_unroll, 7
        tmp_sq_sums = 0
        do i=1, n_rows
            do k=0, 7-1, 1
                tmp_sq_sums(k+1) = tmp_sq_sums(k+1) + (float(matrix(i,j+k)) - means_of_matrix_opt(j+k))**2.0
            end do
        end do

        do k=0, 7-1, 1
            tmp_variances(j+k) = tmp_sq_sums(k+1) * tmp_inv
        end do
    end do
    do j=n_cols_unroll+1, n_cols
        tmp_sq_sum = 0
        do i=1, n_rows
            tmp_sq_sum = tmp_sq_sum + (float(matrix(i,j)) - means_of_matrix_opt(j))**2.0
        end do
        tmp_variances(j) = tmp_sq_sum * tmp_inv
    end do
    variance_values_of_matrix_int32 = tmp_variances
end function variance_values_of_matrix_int32

function variance_values_of_matrix_int64(matrix, n_rows, n_cols, means_of_matrix)
    implicit none
    integer(kind=8), intent(in)    :: matrix(n_rows, n_cols)
    integer(kind=8), intent(in) :: n_rows, n_cols
    real(kind=8)                :: variance_values_of_matrix_int64(n_cols)
    real(kind=8), optional      :: means_of_matrix(n_cols)

    integer(kind=8) :: i, j, k
    real(kind=8)    :: tmp_sq_sum, means_of_matrix_opt(n_cols)
    integer(kind=8) :: n_cols_unroll
    real(kind=8)    :: tmp_sq_sums(7)
    real(kind=8)    :: tmp_variances(n_cols), tmp_inv

    tmp_inv = 1d0 / dble(n_rows)

    if (present(means_of_matrix)) then
        means_of_matrix_opt = means_of_matrix
    else
        means_of_matrix_opt = mean(matrix, n_rows, n_cols)
    end if

    n_cols_unroll = n_cols - mod(n_cols, 7)
    do j=1, n_cols_unroll, 7
        tmp_sq_sums = 0
        do i=1, n_rows
            do k=0, 7-1, 1
                tmp_sq_sums(k+1) = tmp_sq_sums(k+1) + (dble(matrix(i,j+k)) - means_of_matrix_opt(j+k))**2d0
            end do
        end do

        do k=0, 7-1, 1
            tmp_variances(j+k) = tmp_sq_sums(k+1) * tmp_inv
        end do
    end do
    do j=n_cols_unroll+1, n_cols
        tmp_sq_sum = 0
        do i=1, n_rows
            tmp_sq_sum = tmp_sq_sum + (dble(matrix(i,j)) - means_of_matrix_opt(j))**2d0
        end do
        tmp_variances(j) = tmp_sq_sum * tmp_inv
    end do
    variance_values_of_matrix_int64 = tmp_variances
end function variance_values_of_matrix_int64
