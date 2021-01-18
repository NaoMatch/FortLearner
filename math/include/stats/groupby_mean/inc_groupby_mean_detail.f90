allocate(x_copy(n_samples))
allocate(y_copy(n_samples))
do n=1, n_samples, 1
    x_copy(n) = x(n)
    y_copy(n) = y(n)
end do
call pbucket_argsort(x_copy, y_copy, n_samples)

call groupby_sum(uniq_x, x_copy, sum_y, y_copy, n_samples)
call groupby_count(uniq_x, count_x, x_copy, n_samples)

n_unique_x = size(uniq_x)
allocate(stat_y(n_unique_x))
do u=1, n_unique_x, 1
    if ( count_x(u) .eq. zero_i ) then
        stat_y(u) = zero_r
    else
        stat_y(u) = real(sum_y(u), kind=kind(kind_r)) / real(count_x(u), kind=kind(kind_r))
    end if
end do
