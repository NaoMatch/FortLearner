n_cols_unroll = n_cols - mod(n_cols, 7)
do j=1, n_cols_unroll, 7
    tmp_sums = 0
    do i=1, n_rows
        do k=0, 7-1, 1
            tmp_sums(k+1) = tmp_sums(k+1) + matrix(i,j+k)
        end do
    end do

    do k=0, 7-1, 1
        tmp_means(j+k) = tmp_sums(k+1) * tmp_inv
    end do
end do

do j=n_cols_unroll+1, n_cols
    tmp_sum = 0d0
    do i=1, n_rows
        tmp_sum = tmp_sum + matrix(i,j)
    end do
    tmp_means(j) = tmp_sum * tmp_inv
end do
