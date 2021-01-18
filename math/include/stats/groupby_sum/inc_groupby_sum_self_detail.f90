if ( allocated(uniq_x) ) deallocate(uniq_x)
if ( allocated(stat_x) ) deallocate(stat_x)

allocate(x_copy(n_samples))
do n=1, n_samples, 1
    x_copy(n) = x(n)
end do
call quick_sort(x_copy, n_samples)
n_unique = count_unique(x_copy, n_samples)

allocate(uniq_x(n_unique))
allocate(positions(n_unique))
uniq_x(1) = x_copy(1)
idx = 2
do n=2, n_samples, 1
    if ( x_copy(n-1) .ne. x_copy(n) ) then
        uniq_x(idx) = x_copy(n)
        positions(idx-1) = n-1
        idx = idx + 1
    end if
end do
positions(n_unique) = n_samples

allocate(stat_x(n_unique))        
i_start = 1
do n=1, size(positions), 1
    sum_x = 0d0
    i_stop = positions(n)
    do i=i_start,  i_stop, 1
        sum_x = sum_x + x_copy(i)
    end do
    stat_x(n) = sum_x
    i_start = i_stop + 1
end do
