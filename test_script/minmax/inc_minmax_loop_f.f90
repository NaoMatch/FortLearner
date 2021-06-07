min = huge(min)
max = - min
do i=1, n, 1
    tmp_x = x(i)
    min = minval( (/min, tmp_x/) )
    max = maxval( (/max, tmp_x/) )
end do
