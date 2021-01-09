min_val = huge(min_val)
max_val = -huge(max_val)

do n=1, unroll, buffer_get_minmax
    do j=0, buffer_get_minmax-1, 1
        buffer(j+1) = vector(n+j)
    end do
    tmp_min_val = minval(buffer)
    tmp_max_val = maxval(buffer)
    min_val = minval((/min_val, tmp_min_val/))
    max_val = maxval((/max_val, tmp_max_val/))
end do

do n=unroll+1, num
    tmp_val = vector(n)
    min_val = minval((/min_val, tmp_val/))
    max_val = maxval((/max_val, tmp_val/))
end do