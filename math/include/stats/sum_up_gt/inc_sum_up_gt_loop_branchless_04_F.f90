unroll_size=4
n_remain = mod(n_samples, unroll_size)
n_unroll = n_samples - n_remain

r00 = 0d0
r01 = 0d0
r02 = 0d0
r03 = 0d0
do i=1, n_unroll, unroll_size
    r12 = y(i) .le. threshold_y
    r00 = r00 + x(i) * r12

    r13 = y(i+1) .le. threshold_y
    r01 = r01 + x(i+1) * r13

    r14 = y(i+2) .le. threshold_y
    r02 = r02 + x(i+2) * r14

    r15 = y(i+3) .le. threshold_y
    r03 = r03 + x(i+3) * r15
end do

tmp_sum = r00 + r01
r02     = r02 + r03
tmp_sum = tmp_sum + r02
if (n_remain .gt. 0_8) then
    do i=n_unroll+1, n_samples, 1
        r12 = y(i) .le. threshold_y
        tmp_sum = tmp_sum + x(i) * r12
    end do
end if
