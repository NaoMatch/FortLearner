tmp_sum = zero
do n=1, n_samples, 1
    val = vector(n)
    factor = val .le. threshold
    tmp_sum = tmp_sum + val * factor
end do
