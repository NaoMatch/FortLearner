
num_unroll = n - mod(n, 4)

r12 = 0d0
r13 = 0d0
r14 = 0d0
r15 = 0d0

do i=1, num_unroll, 4
    r00 = x(i)
    r01 = x(i+1)
    r02 = x(i+2)
    r03 = x(i+3)

    r04 = y(i)
    r05 = y(i+1)
    r06 = y(i+2)
    r07 = y(i+3)

    r08 = r00 <= v
    r09 = r01 <= v
    r10 = r02 <= v
    r11 = r03 <= v

    r12 = r12 + r04 * r08
    r13 = r13 + r05 * r09
    r14 = r14 + r06 * r10
    r15 = r15 + r07 * r11
end do

do i=num_unroll+1, n, 1
    factor = x(i) .le. v
    r15 = r15 + y(i) * factor
end do