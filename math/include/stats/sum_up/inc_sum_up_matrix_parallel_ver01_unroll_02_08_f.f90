i_unroll=n-mod(n,2)
j_unroll=c-mod(c,8)

do j=1, j_unroll, 8
    r08 = 0d0
    r09 = 0d0
    r10 = 0d0
    r11 = 0d0
    r12 = 0d0
    r13 = 0d0
    r14 = 0d0
    r15 = 0d0

    !$omp parallel num_threads(2)
    !$omp do reduction(+:r08,r09,r10,r11,r12,r13,r14,r15)
    do i=1, i_unroll, 2
        r00 = x(i+0,j+0)
        r01 = x(i+1,j+0)
        r00 = r00 + r01
        r08 = r08 + r00

        r02 = x(i+0,j+1)
        r03 = x(i+1,j+1)
        r02 = r02 + r03
        r09 = r09 + r02

        r04 = x(i+0,j+2)
        r05 = x(i+1,j+2)
        r04 = r04 + r05
        r10 = r10 + r04

        r06 = x(i+0,j+3)
        r07 = x(i+1,j+3)
        r06 = r06 + r07
        r11 = r11 + r06

        r00 = x(i+0,j+4)
        r01 = x(i+1,j+4)
        r00 = r00 + r01
        r12 = r12 + r00

        r02 = x(i+0,j+5)
        r03 = x(i+1,j+5)
        r02 = r02 + r03
        r13 = r13 + r02

        r04 = x(i+0,j+6)
        r05 = x(i+1,j+6)
        r04 = r04 + r05
        r14 = r14 + r04

        r06 = x(i+0,j+7)
        r07 = x(i+1,j+7)
        r06 = r06 + r07
        r15 = r15 + r06
    end do
    !$omp end do 
    !$omp end parallel

    do i=i_unroll+1, n, 1
        r08 = r08 + x(i,j+0)
        r09 = r09 + x(i,j+1)
        r10 = r10 + x(i,j+2)
        r11 = r11 + x(i,j+3)
        r12 = r12 + x(i,j+4)
        r13 = r13 + x(i,j+5)
        r14 = r14 + x(i,j+6)
        r15 = r15 + x(i,j+7)
    end do

    r(j)   = r08
    r(j+1) = r09
    r(j+2) = r10
    r(j+3) = r11
    r(j+4) = r12
    r(j+5) = r13
    r(j+6) = r14
    r(j+7) = r15
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
