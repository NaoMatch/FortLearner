n_cols_unroll = n_cols - mod(n_cols, cache_size_multi_mat_vec)
do j=1, n_cols_unroll, cache_size_multi_mat_vec
    do k=0, cache_size_multi_mat_vec-1, 1
        buffer_input(k+1) = input_vector(j+k)
    end do

    do i=1, n_rows, 1
        tmp_out = 0
        do k=0, cache_size_multi_mat_vec-1, 1
            tmp_out = tmp_out + matrix(i,j+k) * buffer_input(k+1)
        end do
        output_vector(i) = output_vector(i) + tmp_out
    end do
end do

do j=n_cols_unroll+1, n_cols, 1
    tmp_input = input_vector(j)
    do i=1, n_rows, 1
        output_vector(i) = output_vector(i) + matrix(i,j) * tmp_input
    end do
end do
