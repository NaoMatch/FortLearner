do j=1, c, 1
    r15 = 0d0

    do i=1, num_unroll, 15
        r00 = x(i,j)
        r01 = x(i+1,j)
        r02 = x(i+2,j)
        r03 = x(i+3,j)
        r04 = x(i+4,j)
        r05 = x(i+5,j)
        r06 = x(i+6,j)
        r07 = x(i+7,j)
        r08 = x(i+8,j)
        r09 = x(i+9,j)
        r10 = x(i+10,j)
        r11 = x(i+11,j)
        r12 = x(i+12,j)
        r13 = x(i+13,j)
        r14 = x(i+14,j)

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
        r15 = r15 + x(i,j)
    end do
    r(j) = r15
end do
