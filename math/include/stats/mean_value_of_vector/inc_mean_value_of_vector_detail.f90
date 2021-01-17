tmp_sum = 0.0
i_unroll = n_samples - mod(n_samples, buffer_size)
do i=1, i_unroll, buffer_size
    do j=0, buffer_size-1, 1
        buffer(j+1) = vector(i+j)
    end do

    do j=0, buffer_size-1, 1
        tmp_sum = tmp_sum + buffer(j+1)
    end do
end do

do i=i_unroll+1, n_samples, 1
    tmp_sum = tmp_sum + vector(i)
end do
