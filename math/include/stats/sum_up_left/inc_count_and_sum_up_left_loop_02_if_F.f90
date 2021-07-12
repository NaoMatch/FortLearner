unroll_size = 2
n_remain = mod(n_samples, unroll_size)
n_unroll = n_samples - n_remain

tmp_sum = 0
tmp_cnt = 0
do i=1, n_unroll, unroll_size

    if ( y(i) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i)
        tmp_cnt = tmp_cnt + 1_8
    end if

    if ( y(i+1) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i+1)
        tmp_cnt = tmp_cnt + 1_8
    end if

end do

do i=n_unroll+1, n_samples, 1
    if ( y(i) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i)
        tmp_cnt = tmp_cnt + 1_8
    end if    
end do