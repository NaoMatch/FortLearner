unroll_size = 16
n_remain = mod(n_samples, unroll_size)
n_unroll = n_samples - n_remain

tmp_sum = 0
tmp_cnt = 0
do i=1, n_unroll, unroll_size

    if ( y(i) .gt. threshold_y) then
        tmp_sum = tmp_sum + x(i)
        tmp_cnt = tmp_cnt + 1_8
    end if

    if ( y(i+1) .gt. threshold_y) then
        tmp_sum = tmp_sum + x(i+1)
        tmp_cnt = tmp_cnt + 1_8
    end if

    if ( y(i+2) .gt. threshold_y) then
        tmp_sum = tmp_sum + x(i+2)
        tmp_cnt = tmp_cnt + 1_8
    end if

    if ( y(i+3) .gt. threshold_y) then
        tmp_sum = tmp_sum + x(i+3)
        tmp_cnt = tmp_cnt + 1_8
    end if

    if ( y(i+4) .gt. threshold_y) then
        tmp_sum = tmp_sum + x(i+4)
        tmp_cnt = tmp_cnt + 1_8
    end if

    if ( y(i+5) .gt. threshold_y) then
        tmp_sum = tmp_sum + x(i+5)
        tmp_cnt = tmp_cnt + 1_8
    end if

    if ( y(i+6) .gt. threshold_y) then
        tmp_sum = tmp_sum + x(i+6)
        tmp_cnt = tmp_cnt + 1_8
    end if

    if ( y(i+7) .gt. threshold_y) then
        tmp_sum = tmp_sum + x(i+7)
        tmp_cnt = tmp_cnt + 1_8
    end if

    if ( y(i+8) .gt. threshold_y) then
        tmp_sum = tmp_sum + x(i+8)
        tmp_cnt = tmp_cnt + 1_8
    end if

    if ( y(i+9) .gt. threshold_y) then
        tmp_sum = tmp_sum + x(i+9)
        tmp_cnt = tmp_cnt + 1_8
    end if

    if ( y(i+10) .gt. threshold_y) then
        tmp_sum = tmp_sum + x(i+10)
        tmp_cnt = tmp_cnt + 1_8
    end if

    if ( y(i+11) .gt. threshold_y) then
        tmp_sum = tmp_sum + x(i+11)
        tmp_cnt = tmp_cnt + 1_8
    end if

    if ( y(i+12) .gt. threshold_y) then
        tmp_sum = tmp_sum + x(i+12)
        tmp_cnt = tmp_cnt + 1_8
    end if

    if ( y(i+13) .gt. threshold_y) then
        tmp_sum = tmp_sum + x(i+13)
        tmp_cnt = tmp_cnt + 1_8
    end if

    if ( y(i+14) .gt. threshold_y) then
        tmp_sum = tmp_sum + x(i+14)
        tmp_cnt = tmp_cnt + 1_8
    end if

    if ( y(i+15) .gt. threshold_y) then
        tmp_sum = tmp_sum + x(i+15)
        tmp_cnt = tmp_cnt + 1_8
    end if

end do

do i=n_unroll+1, n_samples, 1
    if ( y(i) .gt. threshold_y) then
        tmp_sum = tmp_sum + x(i)
        tmp_cnt = tmp_cnt + 1_8
    end if    
end do