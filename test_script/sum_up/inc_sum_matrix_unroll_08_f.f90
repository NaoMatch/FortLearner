do j=1, c, 1
    r08 = 0d0
    r09 = 0d0
    r10 = 0d0
    r11 = 0d0
    r12 = 0d0
    r13 = 0d0
    r14 = 0d0
    r15 = 0d0
    do i=1, num_unroll, 8
        r00 = x(i  ,j)
        r01 = x(i+1,j)
        r02 = x(i+2,j)
        r03 = x(i+3,j)
        r04 = x(i+4,j)
        r05 = x(i+5,j)
        r06 = x(i+6,j)
        r07 = x(i+7,j)

        r08 = r08 + r00
        r09 = r09 + r01
        r10 = r10 + r02
        r11 = r11 + r03
        r12 = r12 + r04
        r13 = r13 + r05
        r14 = r14 + r06
        r15 = r15 + r07
    end do

    do i=num_unroll+1, n, 1
        r15 = r15 + x(i,j)
    end do
    r(j) = r15+r14+r13+r12+r11+r10+r09+r08
end do
