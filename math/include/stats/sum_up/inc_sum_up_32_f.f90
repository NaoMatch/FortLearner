tmp = 0d0

do i=1, num_unroll, 32
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
    r15 = x(i+15)

    r00 = r00 + r01
    r02 = r02 + r03
    r04 = r04 + r05
    r06 = r06 + r07
    r08 = r08 + r09
    r10 = r10 + r11
    r12 = r12 + r13
    r14 = r14 + r15

    r00 = r00 + r02
    r04 = r04 + r06
    r08 = r08 + r10
    r12 = r12 + r14

    r00 = r00 + r04
    r08 = r08 + r12

    r00 = r00 + r08

    tmp = tmp + r00

    r00 = x(i+16)
    r01 = x(i+17)
    r02 = x(i+18)
    r03 = x(i+19)
    r04 = x(i+20)
    r05 = x(i+21)
    r06 = x(i+22)
    r07 = x(i+23)
    r08 = x(i+24)
    r09 = x(i+25)
    r10 = x(i+26)
    r11 = x(i+27)
    r12 = x(i+28)
    r13 = x(i+29)
    r14 = x(i+30)
    r15 = x(i+31)

    r00 = r00 + r01
    r02 = r02 + r03
    r04 = r04 + r05
    r06 = r06 + r07
    r08 = r08 + r09
    r10 = r10 + r11
    r12 = r12 + r13
    r14 = r14 + r15

    r00 = r00 + r02
    r04 = r04 + r06
    r08 = r08 + r10
    r12 = r12 + r14

    r00 = r00 + r04
    r08 = r08 + r12

    r00 = r00 + r08

    tmp = tmp + r00
end do
r15 = tmp

do i=num_unroll+1, n, 1
    r15 = r15 + x(i)
end do
