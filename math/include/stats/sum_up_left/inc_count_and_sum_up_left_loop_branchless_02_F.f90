unroll_size = 2
n_remain = mod(n_samples, unroll_size)
n_unroll = n_samples - n_remain

r03 = 0 ! sum
r08 = 0 ! sum
r04 = 0 ! counter
r09 = 0 ! counter
do i=1, n_unroll, unroll_size
    r00 = y(i)
    r01 = x(i)
    r02 = r00 .le. threshold_y
    r03 = r03 + r01 * r02
    r04 = r04 + r02

    r05 = y(i+1)
    r06 = x(i+1)
    r07 = r05 .le. threshold_y
    r08 = r08 + r06 * r07
    r09 = r09 + r07
end do

do i=n_unroll+1, n_samples, 1
    r00 = y(i)
    r01 = x(i)
    r02 = r00 .le. threshold_y
    r03 = r03 + r01 * r02
    r04 = r04 + r02
end do

tmp_sum = r03 + r08
tmp_cnt = r04 + r09