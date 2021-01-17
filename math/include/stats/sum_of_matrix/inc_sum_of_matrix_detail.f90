if ( allocated(sum_mat) ) deallocate(sum_mat)

if ( dim .eq. one ) then
    allocate(sum_mat(n_columns))
elseif ( dim .eq. two ) then
    allocate(sum_mat(n_samples))
else
    stop "Variable 'dim' in 'sum_of_matrix' is 1 or 2."
end if

if ( dim .eq. one ) then
    do j=1, n_columns, 1
        tmp_sum = zero
        do i=1, n_samples, 1
            tmp_sum = tmp_sum + matrix(i,j)
        end do
        sum_mat(j) = tmp_sum
    end do
elseif (dim .eq. two) then
    do i=1, n_samples, 1
        tmp_sum = zero
        do j=1, n_columns, 1
            tmp_sum = tmp_sum + matrix(i,j)
        end do
        sum_mat(i) = tmp_sum
    end do
end if
