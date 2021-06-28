i_unroll=n-mod(n,8)
j_unroll=c-mod(c,4)

do j=1, j_unroll, 4
    r28 =0d0
    r29 =0d0
    r30 =0d0
    r31 =0d0

    do i=1, i_unroll, 8
        ! ----------------------------------------------
        r00 = x(i+0,j)
        r01 = x(i+1,j)
        r02 = x(i+2,j)
        r03 = x(i+3,j)
        r04 = x(i+4,j)
        r05 = x(i+5,j)
        r06 = x(i+6,j)
        r07 = x(i+7,j)

        r00 = r00 + r01
        r02 = r02 + r03
        r04 = r04 + r05
        r06 = r06 + r07
        r00 = r00 + r02
        r04 = r04 + r06
        r00 = r00 + r04
        r30 = r30 + r00

        ! ----------------------------------------------
        r08 = x(i+0,j+1)
        r09 = x(i+1,j+1)
        r10 = x(i+2,j+1)
        r11 = x(i+3,j+1)
        r12 = x(i+4,j+1)
        r13 = x(i+5,j+1)
        r14 = x(i+6,j+1)
        r15 = x(i+7,j+1)

        r08 = r08 + r09
        r10 = r10 + r11
        r12 = r12 + r13
        r14 = r14 + r15
        r08 = r08 + r10
        r12 = r12 + r14
        r08 = r08 + r12
        r31 = r31 + r08

        ! ----------------------------------------------
        r00 = x(i+0,j+2)
        r01 = x(i+1,j+2)
        r02 = x(i+2,j+2)
        r03 = x(i+3,j+2)
        r04 = x(i+4,j+2)
        r05 = x(i+5,j+2)
        r06 = x(i+6,j+2)
        r07 = x(i+7,j+2)

        r00 = r00 + r01
        r02 = r02 + r03
        r04 = r04 + r05
        r06 = r06 + r07
        r00 = r00 + r02
        r04 = r04 + r06
        r00 = r00 + r04
        r30 = r30 + r00

        ! ----------------------------------------------
        r08 = x(i+0,j+3)
        r09 = x(i+1,j+3)
        r10 = x(i+2,j+3)
        r11 = x(i+3,j+3)
        r12 = x(i+4,j+3)
        r13 = x(i+5,j+3)
        r14 = x(i+6,j+3)
        r15 = x(i+7,j+3)

        r08 = r08 + r09
        r10 = r10 + r11
        r12 = r12 + r13
        r14 = r14 + r15
        r08 = r08 + r10
        r12 = r12 + r14
        r08 = r08 + r12
        r31 = r31 + r08
    end do

    do i=i_unroll+1, n, 1
        r28 = r28 + x(i,j)
        r29 = r29 + x(i,j+1)
        r30 = r30 + x(i,j+2)
        r31 = r31 + x(i,j+3)
    end do

    r(j)   = r28
    r(j+1) = r29
    r(j+2) = r30
    r(j+3) = r31
end do

do j=j_unroll+1, c, 1
    r15 =0d0
    do i=1, i_unroll, 8
        r00 = x(i+0,j)
        r01 = x(i+1,j)
        r02 = x(i+2,j)
        r03 = x(i+3,j)
        r04 = x(i+4,j)
        r05 = x(i+5,j)
        r06 = x(i+6,j)
        r07 = x(i+7,j)

        r00 = r00 + r01
        r02 = r02 + r03
        r04 = r04 + r05
        r06 = r06 + r07
        r00 = r00 + r02
        r04 = r04 + r06
        r00 = r00 + r04
        r15 = r15 + r00
    end do

    do i=i_unroll+1, n, 1
        r15 = r15 + x(i,j)
    end do

    r(j) = r15
end do
