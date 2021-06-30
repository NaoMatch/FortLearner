r12 = 0d0
r13 = 0d0
r14 = 0d0
r15 = 0d0

do i=1, num_unroll, 8
    r00 = x(i)
    r01 = x(i+1)
    r02 = x(i+2)
    r03 = x(i+3)
    r04 = x(i+4)
    r05 = x(i+5)
    r06 = x(i+6)
    r07 = x(i+7)

    r08 = r04 + r00
    r09 = r05 + r01
    r10 = r06 + r02
    r11 = r07 + r03

    r12 = r12 + r08
    r13 = r13 + r09
    r14 = r14 + r10
    r15 = r15 + r11
end do

do i=num_unroll+1, n, 1
    r15 = r15 + x(i)
end do
