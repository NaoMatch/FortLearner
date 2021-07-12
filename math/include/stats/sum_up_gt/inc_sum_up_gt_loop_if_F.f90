tmp_sum = 0d0
do i=1, n_samples, 1
    if (y(i) .le. threshold_y) then
        tmp_sum = tmp_sum + x(i)
    end if
end do
