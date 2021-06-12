r15 = 0d0

do i=1, num_unroll, 15
    r00 = x(i)
    r01 = x(i+1)
    r02 = x(i+2)
    r03 = x(i+3)
    r04 = x(i+4)
    r05 = x(i+5)
    r06 = x(i+6)
    r07 = x(i+7)
    r08 = x(i+8)
    r09 = x(i+9)
    r10 = x(i+10)
    r11 = x(i+11)
    r12 = x(i+12)
    r13 = x(i+13)
    r14 = x(i+14)

    r15 = r15 + r00
    r15 = r15 + r01
    r15 = r15 + r02
    r15 = r15 + r03
    r15 = r15 + r04
    r15 = r15 + r05
    r15 = r15 + r06
    r15 = r15 + r07
    r15 = r15 + r08
    r15 = r15 + r09
    r15 = r15 + r10
    r15 = r15 + r11
    r15 = r15 + r12
    r15 = r15 + r13
    r15 = r15 + r14
end do

do i=num_unroll+1, n, 1
    r15 = r15 + x(i)
end do
