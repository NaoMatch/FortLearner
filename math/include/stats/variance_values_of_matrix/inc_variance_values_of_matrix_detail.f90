tmp_inv = 1.0 / real(n_rows, kind=kind(tmp_inv))
if (present(means_of_matrix)) then
    means_of_matrix_opt = means_of_matrix
else
    means_of_matrix_opt = mean(matrix, n_rows, n_cols)
end if

n_cols_unroll = n_cols - mod(n_cols, buffer_len)
do j=1, n_cols_unroll, buffer_len
    tmp_sq_sums = 0
    do i=1, n_rows
        do k=0, buffer_len-1, 1
            val = real(matrix(i,j+k), kind=kind(val)) - means_of_matrix_opt(j+k)
            tmp_sq_sums(k+1) = tmp_sq_sums(k+1) + val*val
        end do
    end do

    do k=0, buffer_len-1, 1
        tmp_variances(j+k) = tmp_sq_sums(k+1) * tmp_inv
    end do
end do
do j=n_cols_unroll+1, n_cols
    tmp_sq_sum = 0
    do i=1, n_rows
        val = real(matrix(i,j), kind=kind(val)) - means_of_matrix_opt(j)
        tmp_sq_sum = tmp_sq_sum + val * val
    end do
    tmp_variances(j) = tmp_sq_sum * tmp_inv
end do
