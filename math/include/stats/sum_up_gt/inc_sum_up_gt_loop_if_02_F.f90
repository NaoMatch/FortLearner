unroll_size=2
n_remain = mod(n_samples, unroll_size)
n_unroll = n_samples - n_remain

r00 = 0d0
r01 = 0d0
do i=1, n_unroll, unroll_size
    if (y(i) .le. threshold_y) then
        r00 = r00 + x(i)
    end if

    if (y(i+1) .le. threshold_y) then
        r01 = r01 + x(i+1)
    end if
end do

tmp_sum = r00 + r01
if (n_remain .gt. 0_8) then
    if (y(n_samples) .le. threshold_y) then
        tmp_sum = tmp_sum + x(n_samples)
    end if
end if