r14 = 0d0
r15 = 0d0

do i=1, num_unroll, 2
    r00 = x(i)
    r01 = x(i+1)

    r14 = r14 + r00
    r15 = r15 + r01
end do

do i=num_unroll+1, n, 1
    r15 = r15 + x(i)
end do
