i_unroll=n-mod(n,2)
j_unroll=c-mod(c,2)

do j=1, j_unroll, 2
    r14 =0d0
    r15 =0d0

    do i=1, i_unroll, 2
        r00 = x(i,  j)
        r01 = x(i+1,j)

        r00 = r00 + r01
        r14 = r14 + r00

        r02 = x(i,  j+1)
        r03 = x(i+1,j+1)

        r02 = r02 + r03
        r15 = r15 + r02
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
    do i=1, i_unroll, 2
        r00 = x(i,  j)
        r01 = x(i+1,j)

        r00 = r00 + r01
        r15 = r15 + r00
    end do

    do i=i_unroll+1, n, 1
        r15 = r15 + x(i,j)
    end do

    r(j)   = r15
end do
