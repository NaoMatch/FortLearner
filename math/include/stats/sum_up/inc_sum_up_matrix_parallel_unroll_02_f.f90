do j=1, c, 1
    r15 = 0d0
    !$omp parallel num_threads(2)
    !$omp do reduction(+:r15)
    do i=1, num_unroll, 2
        r00 = x(i  ,j)
        r01 = x(i+1,j)

        r00 = r00 + r01
        r15 = r15 + r00
    end do
    !$omp end do 
    !$omp end parallel 

    do i=num_unroll+1, n, 1
        r15 = r15 + x(i,j)
    end do
    r(j) = r15
end do
