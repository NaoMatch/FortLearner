i_unroll=n-mod(n,32)
j_unroll=c-mod(c,2)

do j=1, j_unroll, 2
    tmp1 =0d0
    tmp2 =0d0

    !$omp parallel num_threads(2)
    !$omp do reduction(+:tmp1, tmp2)
    do i=1, i_unroll, 32
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
        r16 = x(i+16,j)
        r17 = x(i+17,j)
        r18 = x(i+18,j)
        r19 = x(i+19,j)
        r20 = x(i+20,j)
        r21 = x(i+21,j)
        r22 = x(i+22,j)
        r23 = x(i+23,j)
        r24 = x(i+24,j)
        r25 = x(i+25,j)
        r26 = x(i+26,j)
        r27 = x(i+27,j)
        r28 = x(i+28,j)
        r29 = x(i+29,j)
        r30 = x(i+30,j)
        r31 = x(i+31,j)


        r00 = r00 + r01
        r02 = r02 + r03
        r04 = r04 + r05
        r06 = r06 + r07
        r08 = r08 + r09
        r10 = r10 + r11
        r12 = r12 + r13
        r14 = r14 + r15
        r16 = r16 + r17
        r18 = r18 + r19
        r20 = r20 + r21
        r22 = r22 + r23
        r24 = r24 + r25
        r26 = r26 + r27
        r28 = r28 + r29
        r30 = r30 + r31

        r00 = r00 + r02
        r04 = r04 + r06
        r08 = r08 + r10
        r12 = r12 + r14
        r16 = r16 + r18
        r20 = r20 + r22
        r24 = r24 + r26
        r28 = r28 + r30

        r00 = r00 + r04
        r08 = r08 + r12
        r16 = r16 + r20
        r24 = r24 + r28

        r00 = r00 + r08
        r16 = r16 + r24

        r00 = r00 + r16
        tmp1 = tmp1 + r00

        ! ----------------------------------------------
        ! ----------------------------------------------
        r00 = x(i+0, j+1)
        r01 = x(i+1, j+1)
        r02 = x(i+2, j+1)
        r03 = x(i+3, j+1)
        r04 = x(i+4, j+1)
        r05 = x(i+5, j+1)
        r06 = x(i+6, j+1)
        r07 = x(i+7, j+1)
        r08 = x(i+8, j+1)
        r09 = x(i+9, j+1)
        r10 = x(i+10,j+1)
        r11 = x(i+11,j+1)
        r12 = x(i+12,j+1)
        r13 = x(i+13,j+1)
        r14 = x(i+14,j+1)
        r15 = x(i+15,j+1)
        r16 = x(i+16,j+1)
        r17 = x(i+17,j+1)
        r18 = x(i+18,j+1)
        r19 = x(i+19,j+1)
        r20 = x(i+20,j+1)
        r21 = x(i+21,j+1)
        r22 = x(i+22,j+1)
        r23 = x(i+23,j+1)
        r24 = x(i+24,j+1)
        r25 = x(i+25,j+1)
        r26 = x(i+26,j+1)
        r27 = x(i+27,j+1)
        r28 = x(i+28,j+1)
        r29 = x(i+29,j+1)
        r30 = x(i+30,j+1)
        r31 = x(i+31,j+1)


        r00 = r00 + r01
        r02 = r02 + r03
        r04 = r04 + r05
        r06 = r06 + r07
        r08 = r08 + r09
        r10 = r10 + r11
        r12 = r12 + r13
        r14 = r14 + r15
        r16 = r16 + r17
        r18 = r18 + r19
        r20 = r20 + r21
        r22 = r22 + r23
        r24 = r24 + r25
        r26 = r26 + r27
        r28 = r28 + r29
        r30 = r30 + r31

        r00 = r00 + r02
        r04 = r04 + r06
        r08 = r08 + r10
        r12 = r12 + r14
        r16 = r16 + r18
        r20 = r20 + r22
        r24 = r24 + r26
        r28 = r28 + r30

        r00 = r00 + r04
        r08 = r08 + r12
        r16 = r16 + r20
        r24 = r24 + r28

        r00 = r00 + r08
        r16 = r16 + r24

        r00 = r00 + r16
        tmp2 = tmp2 + r00
    end do
    !$omp end do 
    !$omp end parallel

    do i=i_unroll+1, n, 1
        tmp1 = tmp1 + x(i,j)
        tmp2 = tmp2 + x(i,j+1)
    end do

    r(j)   = tmp1
    r(j+1) = tmp2
end do

do j=j_unroll+1, c, 1
    tmp1 =0d0
    do i=1, i_unroll, 32
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
        r16 = x(i+16,j)
        r17 = x(i+17,j)
        r18 = x(i+18,j)
        r19 = x(i+19,j)
        r20 = x(i+20,j)
        r21 = x(i+21,j)
        r22 = x(i+22,j)
        r23 = x(i+23,j)
        r24 = x(i+24,j)
        r25 = x(i+25,j)
        r26 = x(i+26,j)
        r27 = x(i+27,j)
        r28 = x(i+28,j)
        r29 = x(i+29,j)
        r30 = x(i+30,j)
        r31 = x(i+31,j)


        r00 = r00 + r01
        r02 = r02 + r03
        r04 = r04 + r05
        r06 = r06 + r07
        r08 = r08 + r09
        r10 = r10 + r11
        r12 = r12 + r13
        r14 = r14 + r15
        r16 = r16 + r17
        r18 = r18 + r19
        r20 = r20 + r21
        r22 = r22 + r23
        r24 = r24 + r25
        r26 = r26 + r27
        r28 = r28 + r29
        r30 = r30 + r31

        r00 = r00 + r02
        r04 = r04 + r06
        r08 = r08 + r10
        r12 = r12 + r14
        r16 = r16 + r18
        r20 = r20 + r22
        r24 = r24 + r26
        r28 = r28 + r30

        r00 = r00 + r04
        r08 = r08 + r12
        r16 = r16 + r20
        r24 = r24 + r28

        r00 = r00 + r08
        r16 = r16 + r24

        r00 = r00 + r16
        tmp1 = tmp1 + r00
    end do

    do i=i_unroll+1, n, 1
        tmp1 = tmp1 + x(i,j)
    end do

    r(j) = tmp1
end do
