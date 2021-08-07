min = huge(min)
max = - huge(max)
unroll_size = 4
n_remain = mod(n, unroll_size)
n_unroll = n - n_remain

do i=1, n_unroll, unroll_size
    r00 = vec(i)
    r01 = vec(i+1)
    r02 = vec(i+2)
    r03 = vec(i+3)
    min = minval((/min, r00/))
    max = maxval((/max, r00/))
    max = maxval((/max, r01/))
    min = minval((/min, r01/))
    min = minval((/min, r02/))
    max = maxval((/max, r02/))
    max = maxval((/max, r03/))
    min = minval((/min, r03/))
end do

do i=n_unroll+1, n, 1
    r00 = vec(i)
    min = minval((/min, r00/))
    max = maxval((/max, r00/))
end do
