if ( allocated(uniq_x) ) deallocate(uniq_x)
if ( allocated(stat_x) ) deallocate(stat_x)

allocate(x_copy(n_samples))
do n=1, n_samples, 1
    x_copy(n) = x(n)
end do
call quick_sort(x_copy, n_samples)

allocate(uniq_x(0))
allocate(positions(0))
uniq_x = [uniq_x, x_copy(1)]
do n=2, n_samples, 1
    if ( x_copy(n-1) .ne. x_copy(n) ) then
        uniq_x = [uniq_x, x_copy(n)]
        positions = [positions, n-1]
    end if
end do
positions = [positions, n_samples]

allocate(stat_x(0))
i_start = 1
do n=1, size(positions), 1
    sum_x = 0d0
    i_stop = positions(n)
    do i=i_start,  i_stop, 1
        sum_x = sum_x + x_copy(i) * x_copy(i)
    end do
    i_start = i_stop + 1
    stat_x = [stat_x, sum_x]
end do
