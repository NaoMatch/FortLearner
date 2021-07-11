unroll_size=2
n_remain = mod(n_samples, unroll_size)
n_unroll = n_samples - n_remain

r00 = 0d0
r01 = 0d0
do i=1, n_unroll, unroll_size
    r12 = y(i) .le. threshold_y
    r00 = r00 + x(i) * r12

    r13 = y(i+1) .le. threshold_y
    r01 = r01 + x(i+1) * r13
end do

tmp_sum = r01 + r00
if (n_remain .eq. 1_8) then
    r12 = y(i) .le. threshold_y
    tmp_sum = tmp_sum + x(i) * r12
end if
