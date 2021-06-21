i_unroll=n-mod(n,16)
j_unroll=c-mod(c,2)

do j=1, j_unroll, 2
    tmp1 =0d0
    tmp2 =0d0

    !$omp sections
        !$omp section
        do i=1, i_unroll, 16
            ! ----------------------------------------------
            r00 = x(i+0, j)
            r01 = x(i+1, j)
            r02 = x(i+2, j)
            r03 = x(i+3, j)
            r04 = x(i+4, j)
            r05 = x(i+5, j)
            r06 = x(i+6, j)
            r07 = x(i+7, j)
            r08 = x(i+8, j)
            r09 = x(i+9, j)
            r10 = x(i+10,j)
            r11 = x(i+11,j)
            r12 = x(i+12,j)
            r13 = x(i+13,j)
            r14 = x(i+14,j)
            r15 = x(i+15,j)

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
            tmp1 = tmp1 + r00
        end do
        !$omp section
        do i=1, i_unroll, 16
            ! ----------------------------------------------
            r16 = x(i+0, j+1)
            r17 = x(i+1, j+1)
            r18 = x(i+2, j+1)
            r19 = x(i+3, j+1)
            r20 = x(i+4, j+1)
            r21 = x(i+5, j+1)
            r22 = x(i+6, j+1)
            r23 = x(i+7, j+1)
            r24 = x(i+8, j+1)
            r25 = x(i+9, j+1)
            r26 = x(i+10,j+1)
            r27 = x(i+11,j+1)
            r28 = x(i+12,j+1)
            r29 = x(i+13,j+1)
            r30 = x(i+14,j+1)
            r31 = x(i+15,j+1)

            r16 = r16 + r17
            r18 = r18 + r19
            r20 = r20 + r21
            r22 = r22 + r23
            r24 = r24 + r25
            r26 = r26 + r27
            r28 = r28 + r29
            r30 = r30 + r31

            r16 = r16 + r18
            r20 = r20 + r22
            r24 = r24 + r26
            r28 = r28 + r30

            r16 = r16 + r20
            r24 = r24 + r28

            r16 = r16 + r24
            tmp2 = tmp2 + r16
        end do
    !$omp end sections

    do i=i_unroll+1, n, 1
        tmp1 = tmp1 + x(i,j)
        tmp2 = tmp2 + x(i,j+1)
    end do

    r(j)   = tmp1
    r(j+1) = tmp2
end do

do j=j_unroll+1, c, 1
    r30 =0d0
    do i=1, i_unroll, 16
        r00 = x(i+0, j)
        r01 = x(i+1, j)
        r02 = x(i+2, j)
        r03 = x(i+3, j)
        r04 = x(i+4, j)
        r05 = x(i+5, j)
        r06 = x(i+6, j)
        r07 = x(i+7, j)
        r08 = x(i+8, j)
        r09 = x(i+9, j)
        r10 = x(i+10,j)
        r11 = x(i+11,j)
        r12 = x(i+12,j)
        r13 = x(i+13,j)
        r14 = x(i+14,j)
        r15 = x(i+15,j)

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
        r30 = r30 + r00
    end do

    do i=i_unroll+1, n, 1
        r30 = r30 + x(i,j)
    end do

    r(j) = r30
end do
