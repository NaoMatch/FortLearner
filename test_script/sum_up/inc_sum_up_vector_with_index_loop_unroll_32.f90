tmp = 0
n_ids_unroll = n_ids - mod(n_ids, 32)
do i=1, n_ids_unroll, 32
    r00 = x(ids(i))
    r01 = x(ids(i+1))
    r02 = x(ids(i+2))
    r03 = x(ids(i+3))
    r04 = x(ids(i+4))
    r05 = x(ids(i+5))
    r06 = x(ids(i+6))
    r07 = x(ids(i+7))
    r08 = x(ids(i+8))
    r09 = x(ids(i+9))
    r10 = x(ids(i+10))
    r11 = x(ids(i+11))
    r12 = x(ids(i+12))
    r13 = x(ids(i+13))
    r14 = x(ids(i+14))
    r15 = x(ids(i+15))
    r16 = x(ids(i+16))
    r17 = x(ids(i+17))
    r18 = x(ids(i+18))
    r19 = x(ids(i+19))
    r20 = x(ids(i+20))
    r21 = x(ids(i+21))
    r22 = x(ids(i+22))
    r23 = x(ids(i+23))
    r24 = x(ids(i+24))
    r25 = x(ids(i+25))
    r26 = x(ids(i+26))
    r27 = x(ids(i+27))
    r28 = x(ids(i+28))
    r29 = x(ids(i+29))
    r30 = x(ids(i+30))
    r31 = x(ids(i+31))

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

    tmp = tmp + r00

end do

r15 = tmp 
do i=n_ids_unroll+1, n_ids, 1
    r00 = x(ids(i))
    r15 = r15 + r00
end do
