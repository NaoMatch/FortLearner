i_unroll=n-mod(n,4)
j_unroll=c-mod(c,8)

do j=1, j_unroll, 8
    r24 =0d0
    r25 =0d0
    r26 =0d0
    r27 =0d0
    r28 =0d0
    r29 =0d0
    r30 =0d0
    r31 =0d0
    !$omp sections
        !$omp section
        do i=1, i_unroll, 4
            ! ----------------------------------------------
            r00 = x(i+0,j)
            r01 = x(i+1,j)
            r02 = x(i+2,j)
            r03 = x(i+3,j)

            r00 = r00 + r01
            r02 = r02 + r03
            r00 = r00 + r02
            r24 = r24 + r00
        end do
        !$omp section
        do i=1, i_unroll, 4
            ! ----------------------------------------------
            r04 = x(i+0,j+1)
            r05 = x(i+1,j+1)
            r06 = x(i+2,j+1)
            r07 = x(i+3,j+1)

            r04 = r04 + r05
            r06 = r06 + r07
            r04 = r04 + r06
            r25 = r25 + r04
        end do
        !$omp section
        do i=1, i_unroll, 4
            ! ----------------------------------------------
            r08 = x(i+0,j+2)
            r09 = x(i+1,j+2)
            r10 = x(i+2,j+2)
            r11 = x(i+3,j+2)

            r08 = r08 + r09
            r10 = r10 + r11
            r08 = r08 + r10
            r26 = r26 + r08
        end do
        !$omp section
        do i=1, i_unroll, 4
            ! ----------------------------------------------
            r12 = x(i+0,j+3)
            r13 = x(i+1,j+3)
            r14 = x(i+2,j+3)
            r15 = x(i+3,j+3)

            r12 = r12 + r13
            r14 = r14 + r15
            r12 = r12 + r14
            r27 = r27 + r12
        end do
        !$omp section
        do i=1, i_unroll, 4
            ! ----------------------------------------------
            r00 = x(i+0,j+4)
            r01 = x(i+1,j+4)
            r02 = x(i+2,j+4)
            r03 = x(i+3,j+4)

            r00 = r00 + r01
            r02 = r02 + r03
            r00 = r00 + r02
            r28 = r28 + r00
        end do
        !$omp section
        do i=1, i_unroll, 4
            ! ----------------------------------------------
            r04 = x(i+0,j+5)
            r05 = x(i+1,j+5)
            r06 = x(i+2,j+5)
            r07 = x(i+3,j+5)

            r04 = r04 + r05
            r06 = r06 + r07
            r04 = r04 + r06
            r29 = r29 + r04
        end do
        !$omp section
        do i=1, i_unroll, 4
            ! ----------------------------------------------
            r08 = x(i+0,j+6)
            r09 = x(i+1,j+6)
            r10 = x(i+2,j+6)
            r11 = x(i+3,j+6)

            r08 = r08 + r09
            r10 = r10 + r11
            r08 = r08 + r10
            r30 = r30 + r08
        end do
        !$omp section
        do i=1, i_unroll, 4
            ! ----------------------------------------------
            r12 = x(i+0,j+7)
            r13 = x(i+1,j+7)
            r14 = x(i+2,j+7)
            r15 = x(i+3,j+7)

            r12 = r12 + r13
            r14 = r14 + r15
            r12 = r12 + r14
            r31 = r31 + r12
        end do
    !$omp end sections

    do i=i_unroll+1, n, 1
        r24 = r24 + x(i,j)
        r25 = r25 + x(i,j+1)
        r26 = r26 + x(i,j+2)
        r27 = r27 + x(i,j+3)
        r28 = r28 + x(i,j+4)
        r29 = r29 + x(i,j+5)
        r30 = r30 + x(i,j+6)
        r31 = r31 + x(i,j+7)
    end do

    r(j)   = r24
    r(j+1) = r25
    r(j+2) = r26
    r(j+3) = r27
    r(j+4) = r28
    r(j+5) = r29
    r(j+6) = r30
    r(j+7) = r31
end do

do j=j_unroll+1, c, 1
    r15 =0d0
    do i=1, i_unroll, 4
        r00 = x(i+0,j)
        r01 = x(i+1,j)
        r02 = x(i+2,j)
        r03 = x(i+3,j)

        r00 = r00 + r01
        r02 = r02 + r03
        r00 = r00 + r02
        r15 = r15 + r00
    end do

    do i=i_unroll+1, n, 1
        r15 = r15 + x(i,j)
    end do

    r(j) = r15
end do
