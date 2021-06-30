r15 = 0d0

do i=1, num_unroll, 2
    r00 = x(i)
    r01 = x(i+1)

    r00 = r00 + r01
    r15 = r15 + r00
end do

do i=num_unroll+1, n, 1
    r15 = r15 + x(i)
end do
