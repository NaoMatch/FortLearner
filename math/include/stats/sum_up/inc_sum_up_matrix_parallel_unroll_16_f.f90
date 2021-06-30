do j=1, c, 1
    tmp = 0d0

    !$omp parallel num_threads(2)
    !$omp do reduction(+:tmp)
    do i=1, num_unroll, 16
        r00 = x(i,j)
        r01 = x(i+1,j)
        r02 = x(i+2,j)
        r03 = x(i+3,j)
        r04 = x(i+4,j)
        r05 = x(i+5,j)
        r06 = x(i+6,j)
        r07 = x(i+7,j)
        r08 = x(i+8,j)
        r09 = x(i+9,j)
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

        tmp = tmp + r00
    end do
    !$omp end do 
    !$omp end parallel 
    r15 = tmp

    do i=num_unroll+1, n, 1
        r15 = r15 + x(i,j)
    end do

    r(j) = r15
end do
