i_unroll=n-mod(n,4)
j_unroll=c-mod(c,4)

do j=1, j_unroll, 4
    r12 =0d0
    r13 =0d0
    r14 =0d0
    r15 =0d0

    !$omp parallel num_threads(2)
    !$omp do reduction(+:r12, r13, r14, r15)
    do i=1, i_unroll, 4
        r00 = x(i+0,j)
        r01 = x(i+1,j)
        r02 = x(i+2,j)
        r03 = x(i+3,j)

        r00 = r00 + r01
        r02 = r02 + r03
        r00 = r00 + r02
        r12 = r12 + r00

        r04 = x(i+0,j+1)
        r05 = x(i+1,j+1)
        r06 = x(i+2,j+1)
        r07 = x(i+3,j+1)

        r04 = r04 + r05
        r06 = r06 + r07
        r04 = r04 + r06
        r13 = r13 + r04

        r00 = x(i+0,j+2)
        r01 = x(i+1,j+2)
        r02 = x(i+2,j+2)
        r03 = x(i+3,j+2)

        r00 = r00 + r01
        r02 = r02 + r03
        r00 = r00 + r02
        r14 = r14 + r00

        r04 = x(i+0,j+3)
        r05 = x(i+1,j+3)
        r06 = x(i+2,j+3)
        r07 = x(i+3,j+3)

        r04 = r04 + r05
        r06 = r06 + r07
        r04 = r04 + r06
        r15 = r15 + r04
    end do
    !$omp end do 
    !$omp end parallel

    do i=i_unroll+1, n, 1
        r12 = r12 + x(i,j)
        r13 = r13 + x(i,j+1)
        r14 = r14 + x(i,j+2)
        r15 = r15 + x(i,j+3)
    end do

    r(j) = r12
    r(j+1) = r13
    r(j+2) = r14
    r(j+3) = r15
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

    r(j)   = r15
end do
