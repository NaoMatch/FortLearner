unroll_size=16
n_remain = mod(n_samples, unroll_size)
n_unroll = n_samples - n_remain

tmp_sum = 0d0
do i=1, n_unroll, unroll_size
    if (y(i) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i)
    end if

    if (y(i+1) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i+1)
    end if

    if (y(i+2) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i+2)
    end if

    if (y(i+3) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i+3)
    end if

    if (y(i+4) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i+4)
    end if

    if (y(i+5) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i+5)
    end if

    if (y(i+6) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i+6)
    end if

    if (y(i+7) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i+7)
    end if

    if (y(i+8) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i+8)
    end if

    if (y(i+9) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i+9)
    end if

    if (y(i+10) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i+10)
    end if

    if (y(i+11) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i+11)
    end if

    if (y(i+12) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i+12)
    end if

    if (y(i+13) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i+13)
    end if

    if (y(i+14) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i+14)
    end if

    if (y(i+15) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i+15)
    end if
end do

if (n_remain .gt. 0_8) then
    do i=n_unroll+1, n_samples, 1
        if (y(i) .le. threshold_y) then
            tmp_sum = tmp_sum + x(i)
        end if
    end do
end if