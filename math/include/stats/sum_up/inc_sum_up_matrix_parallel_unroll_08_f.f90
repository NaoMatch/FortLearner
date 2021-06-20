do j=1, c, 1
    r15 = 0d0
    !$omp parallel num_threads(2)
    !$omp do reduction(+:r15)
    do i=1, num_unroll, 8
        r00 = x(i,j)
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
    !$omp end do 
    !$omp end parallel 

    do i=num_unroll+1, n, 1
        r15 = r15 + x(i,j)
    end do
    r(j) = r15
end do
