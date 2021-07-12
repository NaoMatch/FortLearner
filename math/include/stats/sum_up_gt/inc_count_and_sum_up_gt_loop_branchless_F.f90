
r03 = 0 ! sum
r04 = 0 ! counter
do i=1, n_samples, 1
    r00 = y(i)
    r01 = x(i)
    r02 = r00 .gt. threshold_y
    r03 = r03 + r01 * r02
    r04 = r04 + r02
end do

tmp_sum = r03
tmp_cnt = r04