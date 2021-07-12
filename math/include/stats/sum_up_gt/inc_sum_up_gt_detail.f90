tmp_sum = zero
do n=1, n_samples, 1
    val_x = x(n)
    val_y = y(n)
    factor = val_y .le. threshold_y
    tmp_sum = tmp_sum + val_y * factor
end do
