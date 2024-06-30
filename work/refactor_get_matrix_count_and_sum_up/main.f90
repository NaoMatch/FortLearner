program main
    use mod_random
    use mod_timer
    use mod_stats
    implicit none
    
    integer(kind=8)        :: date_value1(8), date_value2(8)

    integer(kind=8) :: n_samples, n_columns
    real(kind=8), allocatable, target :: mat(:,:)
    real(kind=8), allocatable, target :: y(:,:)
    real(kind=8), allocatable, target :: thr_vals(:)
    real(kind=8), allocatable, target :: sum_vals_r(:)
    integer(kind=8), allocatable, target :: cnt_vals_r(:)
    integer(kind=8), allocatable, target :: indices(:)
    integer(kind=8) :: i, iter, max_iter, n_jobs, n_indices
    type(c_ptr) :: min_vals_ptr, max_vals_ptr, mat_ptr, indices_diff_ptr, indices_ptr

    call fix_random_seed(10_8)

    n_jobs = 6
    max_iter = 10000
    ! max_iter = 1
    n_samples = 64*100 + 1
    n_columns = 256-8+2
    n_indices = 32*100

    allocate(mat(n_columns, n_samples))
    allocate(y(n_samples,1))
    allocate(sum_vals_r(n_columns))
    allocate(thr_vals(n_columns))
    allocate(cnt_vals_r(n_columns))
    allocate(indices(n_indices))

    call random_number(mat)
    call random_number(thr_vals)
    call random_number(y)
    do i=1, n_indices, 1
        indices(i) = i
    end do
    indices(:) = indices(:)
    ! print*, indices
    ! indices_diff(1) = 0
    mat(:,6401) = +100000000000000000000000d0
    y(6401,:) = -1000000000000000d0

    sum_vals_r = 0d0
    cnt_vals_r = 0_8
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call get_matrix_count_and_sum_up_gt(sum_vals_r, cnt_vals_r, thr_vals, mat, y(:,1), indices, n_indices, n_samples, n_columns)
    end do
    call date_and_time(values=date_value2)
    print*, "Original: ", time_diff(date_value1, date_value2) / dble(max_iter)
    print*, sum_vals_r(n_columns-3:)
    print*, cnt_vals_r(n_columns-3:)


    sum_vals_r = 0d0
    cnt_vals_r = 0_8
    call date_and_time(values=date_value1)
    do iter=1, max_iter, 1
        call new_get_matrix_count_and_sum_up_gt(sum_vals_r, cnt_vals_r, thr_vals, mat, y(:,1), indices, n_indices, &
            n_samples, n_columns, n_jobs)
    end do
    call date_and_time(values=date_value2)
    print*, "NEW_NAIVE_SIMD_UNROLL_I_8: ", time_diff(date_value1, date_value2) / dble(max_iter)
    print*, sum_vals_r(n_columns-3:)
    print*, cnt_vals_r(n_columns-3:)

end program main