r00=0d0
do i=1, n, 1
    factor = x(i) .le. v
    r00 = r00 + y(i) * factor
end do
