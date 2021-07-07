unroll_size=8
n_remain = mod(n_samples, unroll_size)
n_unroll = n_samples - n_remain

tmp_sum = 0d0
do i=1, n_unroll, unroll_size
    r08 = y(i) .le. threshold_y
    tmp_sum = tmp_sum + x(i) * r08

    r09 = y(i+1) .le. threshold_y
    tmp_sum = tmp_sum + x(i+1) * r09

    r10 = y(i+2) .le. threshold_y
    tmp_sum = tmp_sum + x(i+2) * r10

    r11 = y(i+3) .le. threshold_y
    tmp_sum = tmp_sum + x(i+3) * r11

    r12 = y(i+4) .le. threshold_y
    tmp_sum = tmp_sum + x(i+4) * r12

    r13 = y(i+5) .le. threshold_y
    tmp_sum = tmp_sum + x(i+5) * r13

    r14 = y(i+6) .le. threshold_y
    tmp_sum = tmp_sum + x(i+6) * r14

    r15 = y(i+7) .le. threshold_y
    tmp_sum = tmp_sum + x(i+7) * r15
end do

if (n_remain .gt. 0_8) then
    do i=n_unroll+1, n_samples, 1
        r12 = y(i) .le. threshold_y
        tmp_sum = tmp_sum + x(i) * r12
    end do
end if
