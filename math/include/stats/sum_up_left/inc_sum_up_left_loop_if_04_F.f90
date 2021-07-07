unroll_size=4
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
end do

if (n_remain .gt. 0_8) then
    do i=n_unroll+1, n_samples, 1
        if (y(i) .le. threshold_y) then
            tmp_sum = tmp_sum + x(i)
        end if
    end do
end if