tmp_sum = 0
tmp_cnt = 0
do i=1, n_samples, 1
    if ( y(i) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i)
        tmp_cnt = tmp_cnt + 1_8
    end if
end do