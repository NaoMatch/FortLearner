unroll_size=8
n_remain = mod(n_samples, unroll_size)
n_unroll = n_samples - n_remain

tmp_sum = 0d0
r00 = 0d0
r01 = 0d0
r02 = 0d0
r03 = 0d0
r04 = 0d0
r05 = 0d0
r06 = 0d0
r07 = 0d0

do i=1, n_unroll, unroll_size
    r08 = y(i) .le. threshold_y
    r00 = r00 + x(i) * r08

    r09 = y(i+1) .le. threshold_y
    r01 = r01 + x(i+1) * r09

    r10 = y(i+2) .le. threshold_y
    r02 = r02 + x(i+2) * r10

    r11 = y(i+3) .le. threshold_y
    r03 = r03 + x(i+3) * r11

    r12 = y(i+4) .le. threshold_y
    r04 = r04 + x(i+4) * r12

    r13 = y(i+5) .le. threshold_y
    r05 = r05 + x(i+5) * r13

    r14 = y(i+6) .le. threshold_y
    r06 = r06 + x(i+6) * r14

    r15 = y(i+7) .le. threshold_y
    r07 = r07 + x(i+7) * r15
end do

r00 = r00 + r01
r02 = r02 + r03
r04 = r04 + r05
r06 = r06 + r07

r00 = r00 + r02
r04 = r04 + r06

tmp_sum = r00 + r04

if (n_remain .gt. 0_8) then
    do i=n_unroll+1, n_samples, 1
        r12 = y(i) .le. threshold_y
        tmp_sum = tmp_sum + x(i) * r12
    end do
end if
