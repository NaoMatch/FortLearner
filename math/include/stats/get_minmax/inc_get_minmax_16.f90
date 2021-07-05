min = huge(min)
max = - huge(max)
unroll_size = 16
n_remain = mod(n, unroll_size)
n_unroll = n - n_remain

do i=1, n_unroll, unroll_size
    r00 = vec(i)
    min = minval((/min, r00/))
    max = maxval((/max, r00/))

    r01 = vec(i+1)
    max = maxval((/max, r01/))
    min = minval((/min, r01/))

    r02 = vec(i+2)
    min = minval((/min, r02/))
    max = maxval((/max, r02/))

    r03 = vec(i+3)
    max = maxval((/max, r03/))
    min = minval((/min, r03/))

    r04 = vec(i+4)
    min = minval((/min, r04/))
    max = maxval((/max, r04/))

    r05 = vec(i+5)
    max = maxval((/max, r05/))
    min = minval((/min, r05/))

    r06 = vec(i+6)
    min = minval((/min, r06/))
    max = maxval((/max, r06/))

    r07 = vec(i+7)
    max = maxval((/max, r07/))
    min = minval((/min, r07/))

    r08 = vec(i+8)
    min = minval((/min, r08/))
    max = maxval((/max, r08/))

    r09 = vec(i+9)
    max = maxval((/max, r09/))
    min = minval((/min, r09/))

    r10 = vec(i+10)
    min = minval((/min, r10/))
    max = maxval((/max, r10/))

    r11 = vec(i+11)
    max = maxval((/max, r11/))
    min = minval((/min, r11/))

    r12 = vec(i+12)
    min = minval((/min, r12/))
    max = maxval((/max, r12/))

    r13 = vec(i+13)
    max = maxval((/max, r13/))
    min = minval((/min, r13/))

    r14 = vec(i+14)
    min = minval((/min, r14/))
    max = maxval((/max, r14/))

    r15 = vec(i+15)
    max = maxval((/max, r15/))
    min = minval((/min, r15/))
end do

do i=n_unroll+1, n, 1
    r00 = vec(i)
    min = minval((/min, r00/))
    max = maxval((/max, r00/))
end do