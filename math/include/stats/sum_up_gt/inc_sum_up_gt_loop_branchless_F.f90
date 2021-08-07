tmp_sum = 0d0
do i=1, n_samples, 1
    factor = y(i) .le. threshold_y
    tmp_sum = tmp_sum + x(i) * factor
end do
