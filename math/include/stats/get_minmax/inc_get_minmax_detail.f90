min_val = huge(min_val)
max_val = -huge(max_val)
n_samples_unroll = n_samples - mod(n_samples, buffer_get_minmax)
do n=1, n_samples_unroll, buffer_get_minmax
    do j=0, buffer_get_minmax-1, 1
        buffer(j+1) = vector(n+j)
    end do
    tmp_min_val = minval(buffer)
    tmp_max_val = maxval(buffer)
    min_val = minval((/min_val, tmp_min_val/))
    max_val = maxval((/max_val, tmp_max_val/))
end do

do n=n_samples_unroll+1, n_samples
    tmp_val = vector(n)
    min_val = minval((/min_val, tmp_val/))
    max_val = maxval((/max_val, tmp_val/))
end do