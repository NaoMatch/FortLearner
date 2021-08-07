unroll_size=4
n_remain = mod(n_samples, unroll_size)
n_unroll = n_samples - n_remain

r00=0
r01=0
r02=0
r03=0
do i=1, n_unroll, unroll_size
    if (y(i) .le. threshold_y) then
        r00 = r00 + x(i)
    end if

    if (y(i+1) .le. threshold_y) then
        r01 = r01 + x(i+1)
    end if

    if (y(i+2) .le. threshold_y) then
        r02 = r02 + x(i+2)
    end if

    if (y(i+3) .le. threshold_y) then
        r03 = r03 + x(i+3)
    end if
end do

tmp_sum = r00 + r01 + r02 + r03
if (n_remain .gt. 0_8) then
    do i=n_unroll+1, n_samples, 1
        if (y(i) .le. threshold_y) then
            tmp_sum = tmp_sum + x(i)
        end if
    end do
end if