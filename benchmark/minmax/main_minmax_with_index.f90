program main_minmax_with_index
    use mod_timer
    use mod_stats
    use mod_get_matrix_minmax
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)

    real(kind=8)              :: min_val, max_val
    real(kind=8), allocatable, target :: min_vals(:), max_vals(:)
    real(kind=8), allocatable, target :: min_vals_orig(:), max_vals_orig(:)
    real(kind=8), allocatable, target :: mat(:,:), mat_t(:,:), f(:)
    integer(kind=8), allocatable, target :: indices(:), indices_diff(:)
    integer(kind=8)           :: n_rows, n_cols, idx, n_indices
    integer(kind=8)           :: i, j, iter, max_iter, div
    type(c_ptr) :: min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_ptr, indices_diff_ptr

    n_rows = 9000000_8
    n_cols = 32_8
    div = 16_8
    n_indices = n_rows/div

    print*, "allocate"
    allocate(f(n_indices))
    allocate(indices(n_indices))
    allocate(indices_diff(n_indices))
    allocate(mat(n_rows, n_cols))
    allocate(mat_t(n_cols, n_rows))
    allocate(min_vals(n_cols), min_vals_orig(n_cols))
    allocate(max_vals(n_cols), max_vals_orig(n_cols))

    print*, "randomized"
    call random_number(mat)
    call date_and_time(values=date_value1)
    mat_t = transpose(mat)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    do i=1, n_indices, 1
        indices(i) = i*div
    end do


    ! call date_and_time(values=date_value1)
    call get_indices_diff(indices_diff, indices, n_indices)
    indices_diff_ptr = c_loc(indices_diff)
    ! call date_and_time(values=date_value2)
    ! print*, time_diff(date_value1, date_value2)

    do i=1, 10, 1
        print*, indices(i), indices_diff(i)
    end do



    ! stop

    min_vals_ptr = c_loc(min_vals)
    max_vals_ptr = c_loc(max_vals)
    mat_t_ptr = c_loc(mat_t)
    indices_ptr = c_loc(indices)

    print*, "loop call get_minmax"
    call date_and_time(values=date_value1)
    do i=1, n_cols, 1
        do j=1, n_indices, 1
            idx = indices(j)
            f(j) = mat(idx,i)
        end do
        call get_minmax_r8(min_val, max_val, f, n_indices)
        min_vals_orig(i) = min_val
        max_vals_orig(i) = max_val
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)

    print*, "loop call get_minmax_fast"
    call date_and_time(values=date_value1)
    do i=1, n_cols, 1
        do j=1, n_indices, 1
            idx = indices(j)
            f(j) = mat(idx,i)
        end do
        call get_minmax(min_val, max_val, f, n_indices)
        min_vals_orig(i) = min_val
        max_vals_orig(i) = max_val
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)

    print*, "call get_matrix_minmax_col_major_loop_with_index"
    min_vals = huge(min_vals(1))
    max_vals = -huge(max_vals(1))
    call date_and_time(values=date_value1)
    call get_matrix_minmax_col_major_loop_with_index(min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    print*, sum(min_vals-min_vals_orig)
    print*, sum(max_vals-max_vals_orig)

    print*, "call get_matrix_minmax_col_major_loop_with_index_02"
    min_vals = huge(min_vals(1))
    max_vals = -huge(max_vals(1))
    call date_and_time(values=date_value1)
    call get_matrix_minmax_col_major_loop_with_index_02(min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    print*, sum(min_vals-min_vals_orig)
    print*, sum(max_vals-max_vals_orig)

    print*, "call get_matrix_minmax_col_major_loop_with_index_04"
    min_vals = huge(min_vals(1))
    max_vals = -huge(max_vals(1))
    call date_and_time(values=date_value1)
    call get_matrix_minmax_col_major_loop_with_index_04(min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    print*, sum(min_vals-min_vals_orig)
    print*, sum(max_vals-max_vals_orig)

    print*, "call get_matrix_minmax_col_major_loop_with_index_08"
    min_vals = huge(min_vals(1))
    max_vals = -huge(max_vals(1))
    call date_and_time(values=date_value1)
    call get_matrix_minmax_col_major_loop_with_index_08(min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    print*, sum(min_vals-min_vals_orig)
    print*, sum(max_vals-max_vals_orig)

    print*, "call get_matrix_minmax_loop_C"
    min_vals = huge(min_vals(1))
    max_vals = -huge(max_vals(1))
    call date_and_time(values=date_value1)
    call get_matrix_minmax_loop_C(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_ptr, n_indices, n_rows, n_cols)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    print*, sum(min_vals-min_vals_orig)
    print*, sum(max_vals-max_vals_orig)


    print*, "call get_matrix_minmax_loop_02_C"
    min_vals = huge(min_vals(1))
    max_vals = -huge(max_vals(1))
    call date_and_time(values=date_value1)
    call get_matrix_minmax_loop_02_C(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_ptr, n_indices, n_rows, n_cols)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    print*, sum(min_vals-min_vals_orig)
    print*, sum(max_vals-max_vals_orig)


    print*, "call get_matrix_minmax_loop_04_C"
    min_vals = huge(min_vals(1))
    max_vals = -huge(max_vals(1))
    call date_and_time(values=date_value1)
    call get_matrix_minmax_loop_04_C(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_ptr, n_indices, n_rows, n_cols)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    print*, sum(min_vals-min_vals_orig)
    print*, sum(max_vals-max_vals_orig)


    print*, "call get_matrix_minmax_loop_08_C"
    min_vals = huge(min_vals(1))
    max_vals = -huge(max_vals(1))
    call date_and_time(values=date_value1)
    call get_matrix_minmax_loop_08_C(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_ptr, n_indices, n_rows, n_cols)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    print*, sum(min_vals-min_vals_orig)
    print*, sum(max_vals-max_vals_orig)


    print*, "call get_matrix_minmax_loop_04_A"
    min_vals = huge(min_vals(1))
    max_vals = -huge(max_vals(1))
    call date_and_time(values=date_value1)
    call get_matrix_minmax_loop_04_A(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_diff_ptr, n_indices, n_rows, n_cols)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    print*, sum(min_vals-min_vals_orig)
    print*, sum(max_vals-max_vals_orig)


    print*, "call get_matrix_minmax_loop_08_A"
    min_vals = huge(min_vals(1))
    max_vals = -huge(max_vals(1))
    call date_and_time(values=date_value1)
    call get_matrix_minmax_loop_08_A(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_diff_ptr, n_indices, n_rows, n_cols)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    print*, sum(min_vals-min_vals_orig)
    print*, sum(max_vals-max_vals_orig)


    print*, "call get_matrix_minmax_loop_08z_A"
    min_vals = huge(min_vals(1))
    max_vals = -huge(max_vals(1))
    call date_and_time(values=date_value1)
    call get_matrix_minmax_loop_08z_A(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_diff_ptr, n_indices, n_rows, n_cols)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    print*, sum(min_vals-min_vals_orig)
    print*, sum(max_vals-max_vals_orig)


    print*, "call get_matrix_minmax_loop_16_A"
    min_vals = huge(min_vals(1))
    max_vals = -huge(max_vals(1))
    call date_and_time(values=date_value1)
    call get_matrix_minmax_loop_16_A(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_diff_ptr, n_indices, n_rows, n_cols)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    print*, sum(min_vals-min_vals_orig)
    print*, sum(max_vals-max_vals_orig)


    print*, "call get_matrix_minmax_loop_16z_A"
    min_vals = huge(min_vals(1))
    max_vals = -huge(max_vals(1))
    call date_and_time(values=date_value1)
    call get_matrix_minmax_loop_16z_A(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_diff_ptr, n_indices, n_rows, n_cols)
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2)
    print*, sum(min_vals-min_vals_orig)
    print*, sum(max_vals-max_vals_orig)










    print*, "end"
contains

end program main_minmax_with_index
