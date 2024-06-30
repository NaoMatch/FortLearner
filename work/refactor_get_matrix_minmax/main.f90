program main
    use mod_random
    use mod_timer
    use mod_stats
    implicit none
    
    integer(kind=8)        :: date_value1(8), date_value2(8)


    integer(kind=8) :: n_samples, n_columns
    real(kind=8), allocatable, target :: mat(:,:)
    real(kind=8), allocatable, target :: min_vals(:), max_vals(:)
    integer(kind=8), allocatable, target :: indices_diff(:), indices(:)
    integer(kind=8) :: i, iter, max_iter, n_jobs, n_indices
    type(c_ptr) :: min_vals_ptr, max_vals_ptr, mat_ptr, indices_diff_ptr, indices_ptr

    call fix_random_seed(10_8)

    max_iter = 50000
    n_samples = 1000*2 + 2
    n_columns = 255
    n_indices = 1000

    allocate(mat(n_columns, n_samples))
    allocate(min_vals(n_columns))
    allocate(max_vals(n_columns))
    allocate(indices_diff(n_indices))
    allocate(indices(n_indices))

    call random_number(mat)
    min_vals = huge(0d0)
    max_vals = -huge(0d0)
    do i=1, n_indices, 1
        indices(i) = i*2
    end do
    indices(:) = indices(:)
    ! print*, indices
    ! indices_diff(1) = 0
    indices_diff = 1
    mat(:,2001) = 1
    mat(:,2002) = -1

    min_vals_ptr = c_loc(min_vals)
    max_vals_ptr = c_loc(max_vals)
    mat_ptr = c_loc(mat)
    indices_diff_ptr = c_loc(indices_diff)
    indices_ptr = c_loc(indices)

    n_jobs = 4
    min_vals = huge(0d0)
    max_vals = -huge(0d0)
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call get_matrix_minmax(min_vals, max_vals, mat, indices, n_indices, n_samples, n_columns)
    end do
    call date_and_time(values=date_value2)
    print*, "Original: ", time_diff(date_value1, date_value2) / dble(max_iter)
    print*, '*********************************************************************************************'
    print*, min_vals(n_columns-3:)
    print*, max_vals(n_columns-3:)

    min_vals = huge(0d0)
    max_vals = -huge(0d0)
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call get_matrix_minmax_parallel(min_vals, max_vals, mat, indices, n_indices, n_samples, n_columns, n_jobs)
    end do
    call date_and_time(values=date_value2)
    print*, "Original: ", time_diff(date_value1, date_value2) / dble(max_iter)
    print*, '*********************************************************************************************'
    print*, min_vals(n_columns-3:)
    print*, max_vals(n_columns-3:)

    min_vals = huge(0d0)
    max_vals = -huge(0d0)
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call new_get_matrix_minmax(min_vals, max_vals, mat, indices, n_indices, n_samples, n_columns, n_jobs)
    end do
    call date_and_time(values=date_value2)
    print*, "Intrinsic Naive, SAMPLE -> COLUMN: ", time_diff(date_value1, date_value2) / dble(max_iter)
    print*, '*********************************************************************************************'
    print*, min_vals(n_columns-3:)
    print*, max_vals(n_columns-3:)

    min_vals = huge(0d0)
    max_vals = -huge(0d0)
    call date_and_time(values=date_value1)
    do iter=1, 100, 1
        ! call new_get_matrix_minmax(min_vals, max_vals, mat, indices, n_indices, n_samples, n_columns, n_jobs)
        min_vals = minval(mat(:,indices), dim=2)
        max_vals = maxval(mat(:,indices), dim=2)
    end do
    call date_and_time(values=date_value2)
    print*, "MINVAL/MAXVAL Naive, SAMPLE -> COLUMN: ", time_diff(date_value1, date_value2) / dble(100)
    print*, '*********************************************************************************************'
    print*, min_vals(n_columns-3:)
    print*, max_vals(n_columns-3:)

end program main