num_unroll = n - mod(n, 4)
do i=1, num_unroll, 4
    r00 = x(i)
    r01 = x(i+1)
    r02 = x(i+2)
    r03 = x(i+3)

    r14 = minval((/r14, r00/))
    r14 = minval((/r14, r01/))
    r14 = minval((/r14, r02/))
    r14 = minval((/r14, r03/))

    r15 = maxval((/r15, r00/))
    r15 = maxval((/r15, r01/))
    r15 = maxval((/r15, r02/))
    r15 = maxval((/r15, r03/))
end do

do i=num_unroll+1, n, 1
    tmp_x = x(i)
    r14 = minval((/tmp_x, r14/))
    r15 = maxval((/tmp_x, r15/))
end do

min = r14
max = r15
