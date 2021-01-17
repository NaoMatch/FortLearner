tmp_inv = 1.0 / real(n_samples, kind=kind(tmp_inv))
n_columns_unroll = n_columns - mod(n_columns, 7)
do j=1, n_columns_unroll, 7
    tmp_sums = 0
    do i=1, n_samples
        do k=0, 7-1, 1
            tmp_sums(k+1) = tmp_sums(k+1) + matrix(i,j+k)
        end do
    end do

    do k=0, 7-1, 1
        tmp_means(j+k) = real(tmp_sums(k+1), kind=kind(tmp_inv)) * tmp_inv
    end do
end do

do j=n_columns_unroll+1, n_columns
    tmp_sum = 0d0
    do i=1, n_samples
        tmp_sum = tmp_sum + matrix(i,j)
    end do
    tmp_means(j) = tmp_sum * tmp_inv
end do
