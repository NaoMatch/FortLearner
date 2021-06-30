do j=1, c, 1
    tmp = 0d0
    do i=1, n, 1
        tmp = tmp + x(i,j)
    end do
    r(j) = tmp
end do
