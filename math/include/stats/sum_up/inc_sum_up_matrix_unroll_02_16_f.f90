i_unroll=n-mod(n,2)
j_unroll=c-mod(c,16)

do j=1, j_unroll, 16
    r16 = 0d0
    r17 = 0d0
    r18 = 0d0
    r19 = 0d0
    r20 = 0d0
    r21 = 0d0
    r22 = 0d0
    r23 = 0d0
    r24 = 0d0
    r25 = 0d0
    r26 = 0d0
    r27 = 0d0
    r28 = 0d0
    r29 = 0d0
    r30 = 0d0
    r31 = 0d0
    do i=1, i_unroll, 2
        r00 = x(i+0,j+0)
        r01 = x(i+1,j+0)
        r00 = r00 + r01
        r16 = r16 + r00

        r02 = x(i+0,j+1)
        r03 = x(i+1,j+1)
        r02 = r02 + r03
        r17 = r17 + r02

        r04 = x(i+0,j+2)
        r05 = x(i+1,j+2)
        r04 = r04 + r05
        r18 = r18 + r04

        r06 = x(i+0,j+3)
        r07 = x(i+1,j+3)
        r06 = r06 + r07
        r19 = r19 + r06

        r08 = x(i+0,j+4)
        r09 = x(i+1,j+4)
        r08 = r08 + r09
        r20 = r20 + r08

        r10 = x(i+0,j+5)
        r11 = x(i+1,j+5)
        r10 = r10 + r11
        r21 = r21 + r10

        r12 = x(i+0,j+6)
        r13 = x(i+1,j+6)
        r12 = r12 + r13
        r22 = r22 + r12

        r14 = x(i+0,j+7)
        r15 = x(i+1,j+7)
        r14 = r14 + r15
        r23 = r23 + r14

        ! -------------------------------------------

        r00 = x(i+0,j+8)
        r01 = x(i+1,j+8)
        r00 = r00 + r01
        r24 = r24 + r00

        r02 = x(i+0,j+9)
        r03 = x(i+1,j+9)
        r02 = r02 + r03
        r25 = r25 + r02

        r04 = x(i+0,j+10)
        r05 = x(i+1,j+10)
        r04 = r04 + r05
        r26 = r26 + r04

        r06 = x(i+0,j+11)
        r07 = x(i+1,j+11)
        r06 = r06 + r07
        r27 = r27 + r06

        r08 = x(i+0,j+12)
        r09 = x(i+1,j+12)
        r08 = r08 + r09
        r28 = r28 + r08

        r10 = x(i+0,j+13)
        r11 = x(i+1,j+13)
        r10 = r10 + r11
        r29 = r29 + r10

        r12 = x(i+0,j+14)
        r13 = x(i+1,j+14)
        r12 = r12 + r13
        r30 = r30 + r12

        r14 = x(i+0,j+15)
        r15 = x(i+1,j+15)
        r14 = r14 + r15
        r31 = r31 + r14
    end do

    do i=i_unroll+1, n, 1
        r16 = r16 + x(i,j+0)
        r17 = r17 + x(i,j+1)
        r18 = r18 + x(i,j+2)
        r19 = r19 + x(i,j+3)
        r20 = r20 + x(i,j+4)
        r21 = r21 + x(i,j+5)
        r22 = r22 + x(i,j+6)
        r23 = r23 + x(i,j+7)
        r24 = r24 + x(i,j+8)
        r25 = r25 + x(i,j+9)
        r26 = r26 + x(i,j+10)
        r27 = r27 + x(i,j+11)
        r28 = r28 + x(i,j+12)
        r29 = r29 + x(i,j+13)
        r30 = r30 + x(i,j+14)
        r31 = r31 + x(i,j+15)
    end do

    r(j)    = r16
    r(j+1)  = r17
    r(j+2)  = r18
    r(j+3)  = r19
    r(j+4)  = r20
    r(j+5)  = r21
    r(j+6)  = r22
    r(j+7)  = r23
    r(j+8)  = r24
    r(j+9)  = r25
    r(j+10) = r26
    r(j+11) = r27
    r(j+12) = r28
    r(j+13) = r29
    r(j+14) = r30
    r(j+15) = r31
end do

do j=j_unroll+1, c, 1
    r15 =0d0
    do i=1, i_unroll, 2
        r00 = x(i+0,j)
        r01 = x(i+1,j)

        r00 = r00 + r01
        r15 = r15 + r00
    end do

    do i=i_unroll+1, n, 1
        r15 = r15 + x(i,j)
    end do

    r(j)   = r15
end do
