tmp_count = 1
do i=2, n_samples, 1
    factor = vector(i-1) .ne. vector(i)
    tmp_count = tmp_count + factor
end do