i_unroll=n-mod(n,2)
j_unroll=c-mod(c,4)

do j=1, j_unroll, 4
    r12 =0d0
    r13 =0d0
    r14 =0d0
    r15 =0d0

    !$omp sections
        !$omp section
        do i=1, i_unroll, 2
            r00 = x(i+0,j+0)
            r01 = x(i+1,j+0)

            r00 = r00 + r01
            r12 = r12 + r00
        end do
        !$omp section
        do i=1, i_unroll, 2
            r02 = x(i+0,j+1)
            r03 = x(i+1,j+1)

            r02 = r02 + r03
            r13 = r13 + r02
        end do
        !$omp section
        do i=1, i_unroll, 2
            r04 = x(i+0,j+2)
            r05 = x(i+1,j+2)

            r04 = r04 + r05
            r14 = r14 + r04
        end do
        !$omp section
        do i=1, i_unroll, 2
            r06 = x(i+0,j+3)
            r07 = x(i+1,j+3)

            r06 = r06 + r07
            r15 = r15 + r06
        end do
    !$omp end sections

    do i=i_unroll+1, n, 1
        r12 = r12 + x(i,j)
        r13 = r13 + x(i,j+1)
        r14 = r14 + x(i,j+2)
        r15 = r15 + x(i,j+3)
    end do

    r(j)   = r12
    r(j+1) = r13
    r(j+2) = r14
    r(j+3) = r15
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
