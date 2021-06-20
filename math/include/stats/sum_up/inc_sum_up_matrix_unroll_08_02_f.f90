i_unroll=n-mod(n,8)
j_unroll=c-mod(c,2)

do j=1, j_unroll, 2
    r14 =0d0
    r15 =0d0

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
        r14 = r14 + r00

        ! ----------------------------------------------
        r00 = x(i+0,j+1)
        r01 = x(i+1,j+1)
        r02 = x(i+2,j+1)
        r03 = x(i+3,j+1)
        r04 = x(i+4,j+1)
        r05 = x(i+5,j+1)
        r06 = x(i+6,j+1)
        r07 = x(i+7,j+1)

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
        r14 = r14 + x(i,j)
        r15 = r15 + x(i,j+1)
    end do

    r(j)   = r14
    r(j+1) = r15
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
