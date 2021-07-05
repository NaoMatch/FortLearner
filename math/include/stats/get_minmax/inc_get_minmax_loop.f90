min = huge(min)
max = - huge(max)

do i=1, n, 1
    r00 = vec(i)
    min = minval((/min, r00/))
    max = maxval((/max, r00/))
end do
