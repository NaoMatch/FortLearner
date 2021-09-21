program main_minmax_with_index
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
    integer(kind=8)   :: r, c, d
    real(kind=8)   :: rate
    character(len=256) :: filename_r8

    n_rows_test = (/100, 1000, 10000, 100000, 1000000, 10000000/)
    n_cols_test = (/4,    8,     16,     32,      64/)
    n_divs_test = (/1,    2,     4,      8,       16/)
    ! n_cols_test = n_cols_test + 15

    n_iter_base = 500000000_8
    ! n_iter_base = 1
    n_types = 99
    allocate(times(n_types))
    allocate(types(n_types))
    hoge = "hoge fuga piyo"
    types = hoge
    types(1)  = "call get_minmax_slow_loop                              :"
    types(2)  = "call get_minmax_fast_loop                              :"
    types(3)  = "call get_matrix_minmax_col_major_loop_with_index       :"
    types(4)  = "call get_matrix_minmax_col_major_loop_with_index_02    :"
    types(5)  = "call get_matrix_minmax_col_major_loop_with_index_04    :"
    types(6)  = "call get_matrix_minmax_col_major_loop_with_index_08    :"
    types(7)  = "call get_matrix_minmax_loop_C                          :"
    types(8)  = "call get_matrix_minmax_loop_02_C                       :"
    types(9)  = "call get_matrix_minmax_loop_04_C                       :"
    types(10) = "call get_matrix_minmax_loop_08_C                       :"
    types(11) = "call get_matrix_minmax_loop_02_A                       :"
    types(12) = "call get_matrix_minmax_loop_04_A                       :"
    types(13) = "call get_matrix_minmax_loop_04x_A                      :"
    types(14) = "call get_matrix_minmax_loop_08_A                       :"
    types(15) = "call get_matrix_minmax_loop_08z_A                      :"
    types(16) = "call get_matrix_minmax_loop_16_A                       :"
    types(17) = "call get_matrix_minmax_loop_16z_A                      :"

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
                            case  (1) !get_minmax_slow_loop
                                do i=1, n_cols, 1
                                    do j=1, n_indices, 1
                                        idx = indices(j)
                                        f(j) = mat(idx,i)
                                    end do
                                    call get_minmax_r8(min_val, max_val, f, n_indices)
                                    min_vals(i) = min_val
                                    max_vals(i) = max_val
                                end do
                            case  (2) !get_minmax_fast_loop
                                do i=1, n_cols, 1
                                    do j=1, n_indices, 1
                                        idx = indices(j)
                                        f(j) = mat(idx,i)
                                    end do
                                    call get_minmax(min_val, max_val, f, n_indices)
                                    min_vals(i) = min_val
                                    max_vals(i) = max_val
                                end do
                            case  (3)
                                call get_matrix_minmax_col_major_loop_with_index(min_vals, max_vals, mat_t, indices, & 
                                    n_indices, n_rows, n_cols)
                            case  (4)
                                call get_matrix_minmax_col_major_loop_with_index_02(min_vals, max_vals, mat_t, indices, & 
                                    n_indices, n_rows, n_cols)
                            case  (5)
                                call get_matrix_minmax_col_major_loop_with_index_04(min_vals, max_vals, mat_t, indices, & 
                                    n_indices, n_rows, n_cols)
                            case  (6)
                                call get_matrix_minmax_col_major_loop_with_index_08(min_vals, max_vals, mat_t, indices, & 
                                    n_indices, n_rows, n_cols)
                            case  (7)
                                call get_matrix_minmax_loop_C(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_ptr, & 
                                    n_indices, n_rows, n_cols)
                            case  (8)
                                call get_matrix_minmax_loop_02_C(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_ptr, & 
                                    n_indices, n_rows, n_cols)
                            case  (9)
                                if (n_cols<4) exit
                                call get_matrix_minmax_loop_04_C(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_ptr, & 
                                    n_indices, n_rows, n_cols)
                            case (10)
                                if (n_cols<8) exit
                                call get_matrix_minmax_loop_08_C(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_ptr, & 
                                    n_indices, n_rows, n_cols)
                            case (11)
                                call get_matrix_minmax_loop_02_A(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_diff_ptr, & 
                                    n_indices, n_rows, n_cols)
                            case (12)
                                if (n_cols<4) exit
                                call get_matrix_minmax_loop_04_A(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_diff_ptr, & 
                                    n_indices, n_rows, n_cols)
                            case (13)
                                if (n_cols<4) exit
                                call get_matrix_minmax_loop_04x_A(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_diff_ptr, & 
                                    n_indices, n_rows, n_cols)
                            case (14)
                                if (n_cols<8) exit
                                call get_matrix_minmax_loop_08_A(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_diff_ptr, & 
                                    n_indices, n_rows, n_cols)
                            case (15)
                                if (n_cols<8) exit
                                call get_matrix_minmax_loop_08z_A(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_diff_ptr, & 
                                    n_indices, n_rows, n_cols)
                            case (16)
                                if (n_cols<16) exit
                                call get_matrix_minmax_loop_16_A(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_diff_ptr, & 
                                    n_indices, n_rows, n_cols)
                            case (17)
                                if (n_cols<16) exit
                                call get_matrix_minmax_loop_16z_A(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_diff_ptr, & 
                                    n_indices, n_rows, n_cols)
                        end select
                    end do
                    call date_and_time(values=date_value2)
                    times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
                    rate = 1/dble(n_divs_test(d))*100
                    print "(A, i15, i15, f12.4, f30.15, f30.15, f30.15)", types(iter_types), n_cols, n_rows, rate, &
                        real(times(iter_types)), sum(min_vals-min_vals_orig), sum(max_vals-max_vals_orig)
                end do





                deallocate(y, f, indices, indices_diff)
            end do




            deallocate(indices_full)
            deallocate(mat, mat_t)
        end do
        deallocate(min_vals, min_vals_orig)
        deallocate(max_vals, max_vals_orig)
    end do


    stop "HOGE"







end program main_minmax_with_index
