do j=1, c, 1
    r15 = 0d0
    do i=1, num_unroll, 4
        r00 = x(i,j)
        r01 = x(i+1,j)
        r02 = x(i+2,j)
        r03 = x(i+3,j)

        r00 = r00 + r01
        r02 = r02 + r03
        r00 = r00 + r02
        r15 = r15 + r00
    end do

    do i=num_unroll+1, n, 1
        r15 = r15 + x(i,j)
    end do
    r(j) = r15
end do
