i_unroll=n-mod(n,8)
j_unroll=c-mod(c,2)

do j=1, j_unroll, 2
    r12 =0d0
    r13 =0d0
    do i=1, i_unroll, 8
        r00 = x(i+0,j+0)
        r01 = x(i+1,j+0)
        r02 = x(i+2,j+0)
        r03 = x(i+3,j+0)

        r12 = r12 + r00
        r12 = r12 + r01
        r12 = r12 + r02
        r12 = r12 + r03

        r04 = x(i+4,j+0)
        r05 = x(i+5,j+0)
        r06 = x(i+6,j+0)
        r07 = x(i+7,j+0)

        r13 = r13 + r04
        r13 = r13 + r05
        r13 = r13 + r06
        r13 = r13 + r07

        r00 = x(i+0,j+1)
        r01 = x(i+1,j+1)
        r02 = x(i+2,j+1)
        r03 = x(i+3,j+1)

        r12 = r12 + r00
        r12 = r12 + r01
        r12 = r12 + r02
        r12 = r12 + r03

        r04 = x(i+4,j+1)
        r05 = x(i+5,j+1)
        r06 = x(i+6,j+1)
        r07 = x(i+7,j+1)

        r13 = r13 + r04
        r13 = r13 + r05
        r13 = r13 + r06
        r13 = r13 + r07
    end do

    do i=i_unroll+1, n, 1
        r12 = r12 + x(i,j)
        r13 = r13 + x(i,j+1)
    end do

    r(j)   = r12
    r(j+1) = r13
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

        r15 = r15 + r00
        r15 = r15 + r01
        r15 = r15 + r02
        r15 = r15 + r03
        r15 = r15 + r04
        r15 = r15 + r05
        r15 = r15 + r06
        r15 = r15 + r07
    end do

    do i=i_unroll+1, n, 1
        r15 = r15 + x(i,j)
    end do

    r(j)   = r15
end do
