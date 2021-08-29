program main_minmax_with_index_parallel
    use mod_random
    use mod_sort
    use mod_timer
    use mod_stats
    implicit none
    
    integer(kind=8) :: date_value1(8), date_value2(8)

    real(kind=8)              :: st, en
    real(kind=8)              :: min_val, max_val
    real(kind=8), allocatable, target :: min_vals(:), max_vals(:)
    real(kind=8), allocatable, target :: min_vals_orig(:), max_vals_orig(:)
    real(kind=8), allocatable, target :: mat(:,:), mat_t(:,:), f(:), y(:)
    real(kind=8), allocatable, target :: thresholds(:)
    integer(kind=8), allocatable, target :: indices(:), indices_diff(:), indices_full(:)
    integer(kind=8)           :: n_rows, n_cols, idx, n_indices
    integer(kind=8)           :: i, j, iter, max_iter, div, n_types, iter_types, n_iter, n_iter_base
    type(c_ptr) :: min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_ptr, indices_diff_ptr, thresholds_ptr

    integer(kind=8), allocatable :: n_rows_test(:), n_cols_test(:), n_divs_test(:)
    real(kind=8), allocatable :: times(:)
    character(len=60), allocatable :: types(:)
    character(len=30) :: hoge
    integer(kind=8)   :: r, c, d, n_thds
    real(kind=8)   :: rate
    character(len=256) :: filename_r8

    n_rows_test = (/100, 1000, 10000, 100000, 1000000, 10000000/)
    n_cols_test = (/4,    8,     16,     32,      64/)
    n_divs_test = (/1,    2,     4,      8,       16/)
    n_thds = 4
    ! n_cols_test = n_cols_test + 15
    n_rows_test = n_rows_test(5:1:-1)
    n_cols_test = n_cols_test(5:1:-1)
    n_divs_test = n_divs_test(5:1:-1)

    n_iter_base = 500000000_8
    ! n_iter_base = 50000000_8
    ! n_iter_base = 1
    n_types = 99
    allocate(times(n_types))
    allocate(types(n_types))
    hoge = "hoge fuga piyo"
    types = hoge
    types(1)  = "call get_minmax_slow_loop                              :"
    types(2)  = "call get_matrix_minmax_with_index_parallel_01_C_r8     :"
    types(3)  = "call get_matrix_minmax_with_index_parallel_02_C_r8     :"
    types(4)  = "call get_matrix_minmax_with_index_parallel_04_C_r8     :"
    types(5)  = "call get_matrix_minmax_with_index_parallel_08_C_r8     :"
    types(6)  = "call get_matrix_minmax_with_index_parallel_16_C_r8     :"
    types(7)  = "call get_matrix_minmax_with_index_parallel_02x_A_r8    :"
    types(8)  = "call get_matrix_minmax_with_index_parallel_04x_A_r8    :"
    types(9)  = "call get_matrix_minmax_with_index_parallel_04y_A_r8    :"
    types(10) = "call get_matrix_minmax_with_index_parallel_08y_A_r8    :"
    types(11) = "call get_matrix_minmax_with_index_parallel_08z_A_r8    :"
    types(12) = "call get_matrix_minmax_with_index_parallel_16y_A_r8    :"
    types(13) = "call get_matrix_minmax_with_index_parallel_16z_A_r8    :"
    types(14) = "call get_matrix_minmax_with_index_parallel_32z_A_r8    :"
    types(15) = "call get_matrix_minmax_with_index_parallel_01_F_r8     :"
    types(16) = "call get_matrix_minmax_with_index_parallel_02_F_r8     :"
    types(17) = "call get_matrix_minmax_with_index_parallel_04_F_r8     :"
    types(18) = "call get_matrix_minmax_with_index_parallel_08_F_r8     :"
    types(19) = "call get_matrix_minmax_with_index_parallel_18_F_r8     :"

    do c=1, size(n_cols_test), 1
        n_cols = n_cols_test(c)

        allocate(min_vals(n_cols), min_vals_orig(n_cols))
        allocate(max_vals(n_cols), max_vals_orig(n_cols))

        min_vals_ptr = c_loc(min_vals)
        max_vals_ptr = c_loc(max_vals)

        do r=1, size(n_rows_test), 1
            n_rows = n_rows_test(r)
            allocate(indices_full(n_rows))
            do i=1, n_rows, 1
                indices_full(i) = i
            end do
            call permutation(indices_full, n_rows)

            allocate(mat(n_rows, n_cols))
            allocate(mat_t(n_cols, n_rows))
            call random_number(mat)
            mat_t = transpose(mat)
            mat_t_ptr = c_loc(mat_t)

            do d=1, size(n_divs_test), 1
                write (filename_r8, '("time_get_matrix_minmax_with_index_r8_c", i4.4, "_r", i8.8, "_d", i4.4, ".txt")') & 
                    n_cols, n_rows, n_divs_test(d)

                n_indices = n_rows / dble(n_divs_test(d))
                allocate(y(n_indices))
                call random_number(y)

                allocate(f(n_indices))
                allocate(indices(n_indices), indices_diff(n_indices))
                indices = indices_full(1:n_indices)
                call quick_sort(indices, n_indices)
                indices_ptr = c_loc(indices)
                
                call get_indices_diff(indices_diff, indices, n_indices)
                indices_diff_ptr = c_loc(indices_diff)

                min_vals_orig = minval(mat(indices,:), dim=1)
                max_vals_orig = maxval(mat(indices,:), dim=1)

                n_iter=maxval((/n_iter_base/n_rows/n_cols*n_divs_test(d), 1_8/))

                print*, '================================================================================================'
                do iter_types=1, n_types, 1
                    if (types(iter_types) .eq. hoge)  cycle
                    call date_and_time(values=date_value1)
                    min_vals = huge(0d0)
                    max_vals = -huge(0d0)
                    do iter=1, n_iter, 1
                        select case(iter_types)
                            case  (1)
                                call get_matrix_minmax_r8(min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols)
                            case  (2)
                                call get_matrix_minmax_with_index_parallel_01_C_r8(min_vals_ptr, max_vals_ptr, mat_t_ptr, &
                                    indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            case  (3)
                                call get_matrix_minmax_with_index_parallel_02_C_r8(min_vals_ptr, max_vals_ptr, mat_t_ptr, &
                                    indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            case  (4)
                                call get_matrix_minmax_with_index_parallel_04_C_r8(min_vals_ptr, max_vals_ptr, mat_t_ptr, &
                                    indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            case  (5)
                                call get_matrix_minmax_with_index_parallel_08_C_r8(min_vals_ptr, max_vals_ptr, mat_t_ptr, &
                                    indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            case  (6)
                                call get_matrix_minmax_with_index_parallel_16_C_r8(min_vals_ptr, max_vals_ptr, mat_t_ptr, &
                                    indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            case  (7)
                                call get_matrix_minmax_with_index_parallel_02x_A_r8(min_vals_ptr, max_vals_ptr, mat_t_ptr, &
                                    indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            case  (8)
                                call get_matrix_minmax_with_index_parallel_04x_A_r8(min_vals_ptr, max_vals_ptr, mat_t_ptr, &
                                    indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            case  (9)
                                call get_matrix_minmax_with_index_parallel_04y_A_r8(min_vals_ptr, max_vals_ptr, mat_t_ptr, &
                                    indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            case  (10)
                                call get_matrix_minmax_with_index_parallel_08y_A_r8(min_vals_ptr, max_vals_ptr, mat_t_ptr, &
                                    indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            case  (11)
                                call get_matrix_minmax_with_index_parallel_08z_A_r8(min_vals_ptr, max_vals_ptr, mat_t_ptr, &
                                    indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            case  (12)
                                call get_matrix_minmax_with_index_parallel_16y_A_r8(min_vals_ptr, max_vals_ptr, mat_t_ptr, &
                                    indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            case  (13)
                                call get_matrix_minmax_with_index_parallel_16z_A_r8(min_vals_ptr, max_vals_ptr, mat_t_ptr, &
                                    indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            case  (14)
                                call get_matrix_minmax_with_index_parallel_32z_A_r8(min_vals_ptr, max_vals_ptr, mat_t_ptr, &
                                    indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            case  (15)
                                call get_matrix_minmax_with_index_parallel_01_F_r8(min_vals, max_vals, mat_t, &
                                    indices, n_indices, n_rows, n_cols, n_thds)
                            case  (16)
                                call get_matrix_minmax_with_index_parallel_02_F_r8(min_vals, max_vals, mat_t, &
                                    indices, n_indices, n_rows, n_cols, n_thds)
                            case  (17)
                                call get_matrix_minmax_with_index_parallel_04_F_r8(min_vals, max_vals, mat_t, &
                                    indices, n_indices, n_rows, n_cols, n_thds)
                            case  (18)
                                call get_matrix_minmax_with_index_parallel_08_F_r8(min_vals, max_vals, mat_t, &
                                    indices, n_indices, n_rows, n_cols, n_thds)
                            case  (19)
                                call get_matrix_minmax_with_index_parallel_16_F_r8(min_vals, max_vals, mat_t, &
                                    indices, n_indices, n_rows, n_cols, n_thds)
                        end select
                    end do
                    call date_and_time(values=date_value2)
                    times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
                    rate = 1/dble(n_divs_test(d))*100
                    print "(A, i15, i15, f12.4, f30.15, f30.15, f30.15)", types(iter_types), n_cols, n_rows, rate, &
                        real(times(iter_types)), sum(min_vals-min_vals_orig), sum(max_vals-max_vals_orig)
                end do
                    ! stop





                deallocate(y, f, indices, indices_diff)
            end do




            deallocate(indices_full)
            deallocate(mat, mat_t)
        end do
        deallocate(min_vals, min_vals_orig)
        deallocate(max_vals, max_vals_orig)
    end do


    stop "HOGE"







    

end program main_minmax_with_index_parallel
