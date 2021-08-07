program main_sum_up_gt_with_index
    use mod_random
    use mod_sort
    use mod_timer
    use mod_stats
    use mod_matrix_sum_up_gt_with_index
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)

    real(kind=8)              :: st, en
    real(kind=8)              :: sum_gt
    real(kind=8), allocatable, target :: min_vals(:), max_vals(:), thr_vals(:)
    real(kind=8), allocatable, target :: sum_vals(:)
    real(kind=8), allocatable, target :: sum_vals_orig(:)
    real(kind=8), allocatable, target :: mat(:,:), mat_t(:,:), f(:), y(:), y_subset(:), fff(:,:)
    real(kind=8), allocatable, target :: thresholds(:)
    integer(kind=8), allocatable, target :: indices(:), indices_diff(:), indices_full(:), cnt_vals(:), cnt_vals_orig(:)
    integer(kind=8)           :: n_rows, n_cols, idx, n_idxs, cnt_gt
    integer(kind=8)           :: i, j, iter, max_iter, div, n_types, iter_types, n_iter, n_iter_base
    type(c_ptr) :: cnt_vals_ptr, sum_vals_ptr, mat_t_ptr, indices_ptr, indices_diff_ptr, thr_vals_ptr, y_ptr

    integer(kind=8), allocatable :: n_rows_test(:), n_cols_test(:), n_divs_test(:)
    real(kind=8), allocatable :: times(:)
    character(len=60), allocatable :: types(:)
    character(len=30) :: hoge
    integer(kind=8)   :: r, c, d, ccc, n_iter_min
    real(kind=8)   :: rate
    character(len=256) :: filename_r8
    logical(kind=4) :: is_stop

    n_rows_test = (/10000000, 10000, 100000, 1000000, 10000000/)
    n_cols_test = (/4,    8,     16,     32,      64/) * 10
    n_divs_test = (/1,    2,     4,      8,       16/)
    n_cols_test = n_cols_test
    is_stop = .false.

    n_iter_base = 500000000_8
    ! n_iter_base = 28_8
    n_iter_min = 28_8
    ! n_iter_base = 5000000_8
    ! n_iter_base = 1; is_stop = .true.
    n_types = 99
    allocate(times(n_types))
    allocate(types(n_types))
    hoge = "hoge fuga piyo"
    types = hoge
    types(1)  = "call count_and_sum_up_fast                      :"
    types(2)  = "call count_and_sum_up_loop_with_index           :"
    types(3)  = "call count_and_sum_up_loop_with_index_02        :"
    types(4)  = "call count_and_sum_up_loop_with_index_04        :"
    types(5)  = "call count_and_sum_up_loop_with_index_08        :"
    types(6)  = "call get_matrix_sum_up_gt_with_index_C          :"
    types(7)  = "call get_matrix_sum_up_gt_with_index_C_02       :"
    types(8)  = "call get_matrix_sum_up_gt_with_index_C_04       :"
    types(9)  = "call get_matrix_sum_up_gt_with_index_C_08       :"
    ! types(10) = "call get_matrix_sum_up_gt_with_index_A_02       :"
    ! types(11) = "call get_matrix_sum_up_gt_with_index_A_04       :"
    ! types(12) = "call get_matrix_sum_up_gt_with_index_A_04x      :"
    ! types(13) = "call get_matrix_sum_up_gt_with_index_A_08       :"
    ! types(14) = "call get_matrix_sum_up_gt_with_index_A_16       :"
    types(15) = "call get_matrix_sum_up_gt_with_index_A_02_ver02 :"
    types(16) = "call get_matrix_sum_up_gt_with_index_A_04_ver02 :"
    types(17) = "call get_matrix_sum_up_gt_with_index_A_08_ver02 :"
    types(18) = "call get_matrix_sum_up_gt_with_index_A_16_ver02 :"
    types(19) = "call get_matrix_sum_up_gt_with_index_A_04_02    :"
    types(20) = "call get_matrix_sum_up_gt_with_index_A_04_04    :"
    types(21) = "call get_matrix_sum_up_gt_with_index_A_08_02    :"
    types(22) = "call get_matrix_sum_up_gt_with_index_A_08_04    :"

    do c=1, size(n_cols_test), 1
        n_cols = n_cols_test(c)

        allocate(min_vals(n_cols))
        allocate(max_vals(n_cols))
        allocate(thr_vals(n_cols))
        allocate(sum_vals(n_cols), sum_vals_orig(n_cols))
        allocate(cnt_vals(n_cols), cnt_vals_orig(n_cols))

        sum_vals_ptr = c_loc(sum_vals)
        cnt_vals_ptr = c_loc(cnt_vals)

        do r=1, size(n_rows_test), 1
            n_rows = n_rows_test(r)
            allocate(y(n_rows))
            call random_number(y)
            y_ptr = c_loc(y)
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


                n_idxs = n_rows / dble(n_divs_test(d))

                allocate(f(n_idxs))
                allocate(fff(n_idxs, n_cols))
                allocate(y_subset(n_idxs))
                allocate(indices(n_idxs), indices_diff(n_idxs))
                indices = indices_full(1:n_idxs)
                call quick_sort(indices, n_idxs)
                indices_ptr = c_loc(indices)
                
                call get_indices_diff(indices_diff, indices, n_idxs)
                ! indices_diff = [indices_diff]
                indices_diff_ptr = c_loc(indices_diff)

                n_iter=maxval((/n_iter_base/n_rows/n_cols*n_divs_test(d), 1_8/))

                min_vals = minval(mat(indices, :), dim=1)
                max_vals = maxval(mat(indices, :), dim=1)
                call random_number(thr_vals)
                thr_vals = (max_vals-min_vals) * thr_vals + min_vals
                thr_vals_ptr = c_loc(thr_vals)

                do ccc=1, n_cols, 1
                    f = mat(indices,ccc)
                    fff(:,ccc) = mat(indices,ccc)
                    y_subset = y(indices)
                    sum_vals_orig(ccc) = sum( pack(y_subset, mask=f>thr_vals(ccc)))
                    cnt_vals_orig(ccc) = size(pack(f, mask=f>thr_vals(ccc)))
                end do

                print*, '================================================================================================'
                do iter_types=1, n_types, 1
                    if (types(iter_types) .eq. hoge)  cycle
                    call date_and_time(values=date_value1)
                    sum_vals = 0
                    cnt_vals = 0
                    do iter=1, n_iter, 1
                        select case(iter_types)
                            case  (1) 
                                do i=1, n_cols, 1
                                    ! do j=1, n_idxs, 1
                                    !     idx = indices(j)
                                    !     f(j) = mat(idx,i)
                                    ! end do
                                    ! call count_and_sum_up_gt(sum_gt, cnt_gt, y_subset, f, thr_vals(i), n_idxs)
                                    call count_and_sum_up_gt(sum_gt, cnt_gt, y_subset, fff(:,i), thr_vals(i), n_idxs)
                                    sum_vals(i) = sum_gt
                                    cnt_vals(i) = cnt_gt
                                end do
                            case  (2) 
                                call get_matrix_sum_up_gt_with_index(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
                                    n_idxs, n_rows, n_cols)
                            case  (3) 
                                call get_matrix_sum_up_gt_with_index_02(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
                                    n_idxs, n_rows, n_cols)
                            case  (4) 
                                call get_matrix_sum_up_gt_with_index_04(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
                                    n_idxs, n_rows, n_cols)
                            case  (5) 
                                call get_matrix_sum_up_gt_with_index_08(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
                                    n_idxs, n_rows, n_cols)
                            case  (6) 
                                call get_matrix_sum_up_gt_with_index_C(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                                    mat_t_ptr, y_ptr, indices_ptr, & 
                                    n_idxs, n_rows, n_cols)
                            case  (7) 
                                call get_matrix_sum_up_gt_with_index_C_02(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                                    mat_t_ptr, y_ptr, indices_ptr, & 
                                    n_idxs, n_rows, n_cols)
                            case  (8) 
                                call get_matrix_sum_up_gt_with_index_C_04(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                                    mat_t_ptr, y_ptr, indices_ptr, & 
                                    n_idxs, n_rows, n_cols)
                            case  (9) 
                                call get_matrix_sum_up_gt_with_index_C_08(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                                    mat_t_ptr, y_ptr, indices_ptr, & 
                                    n_idxs, n_rows, n_cols)
                            case (10) 
                                call get_matrix_sum_up_gt_with_index_A_02(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                                    mat_t_ptr, y_ptr, indices_diff_ptr, & 
                                    n_idxs, n_rows, n_cols)
                            case (11) 
                                call get_matrix_sum_up_gt_with_index_A_04(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                                    mat_t_ptr, y_ptr, indices_diff_ptr, & 
                                    n_idxs, n_rows, n_cols)
                            case (12) 
                                call get_matrix_sum_up_gt_with_index_A_04x(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                                    mat_t_ptr, y_ptr, indices_diff_ptr, & 
                                    n_idxs, n_rows, n_cols)
                            case (13) 
                                if (n_cols<8) exit
                                call get_matrix_sum_up_gt_with_index_A_08(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                                    mat_t_ptr, y_ptr, indices_diff_ptr, & 
                                    n_idxs, n_rows, n_cols)
                            case (14) 
                                if (n_cols<16) exit
                                call get_matrix_sum_up_gt_with_index_A_16(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                                    mat_t_ptr, y_ptr, indices_diff_ptr, & 
                                    n_idxs, n_rows, n_cols)
                            case (15) 
                                if (n_cols<2) exit
                                call get_matrix_sum_up_gt_with_index_A_02_ver02(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                                    mat_t_ptr, y_ptr, indices_diff_ptr, & 
                                    n_idxs, n_rows, n_cols)
                            case (16) 
                                if (n_cols<4) exit
                                call get_matrix_sum_up_gt_with_index_A_04_ver02(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                                    mat_t_ptr, y_ptr, indices_diff_ptr, & 
                                    n_idxs, n_rows, n_cols)
                            case (17) 
                                if (n_cols<8) exit
                                call get_matrix_sum_up_gt_with_index_A_08_ver02(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                                    mat_t_ptr, y_ptr, indices_diff_ptr, & 
                                    n_idxs, n_rows, n_cols)
                            case (18) 
                                if (n_cols<16) exit
                                call get_matrix_sum_up_gt_with_index_A_16_ver02(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                                    mat_t_ptr, y_ptr, indices_diff_ptr, & 
                                    n_idxs, n_rows, n_cols)
                            case (19) 
                                if (n_cols<4) exit
                                call get_matrix_sum_up_gt_with_index_A_04_02(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                                    mat_t_ptr, y_ptr, indices_diff_ptr, & 
                                    n_idxs, n_rows, n_cols)
                            case (20) 
                                if (n_cols<4) exit
                                call get_matrix_sum_up_gt_with_index_A_04_04(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                                    mat_t_ptr, y_ptr, indices_diff_ptr, & 
                                    n_idxs, n_rows, n_cols)
                            case (21) 
                                if (n_cols<4) exit
                                call get_matrix_sum_up_gt_with_index_A_08_02(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                                    mat_t_ptr, y_ptr, indices_diff_ptr, & 
                                    n_idxs, n_rows, n_cols)
                            case (22) 
                                if (n_cols<4) exit
                                call get_matrix_sum_up_gt_with_index_A_08_04(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                                    mat_t_ptr, y_ptr, indices_diff_ptr, & 
                                    n_idxs, n_rows, n_cols)
                        end select
                    end do
                    call date_and_time(values=date_value2)
                    times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
                    rate = 1/dble(n_divs_test(d))*100
                    print "(A, i15, i15, f12.4, f30.15, f30.15, i15)", types(iter_types), n_cols, n_rows, rate, &
                        real(times(iter_types)), sum(sum_vals-sum_vals_orig), sum(cnt_vals-cnt_vals_orig)
                    ! print*, sum_vals
                    ! print*, sum_vals_orig
                    ! print*, sum_vals/sum_vals_orig
                    ! print*, sum_vals_orig/sum_vals
                    ! print*, cnt_vals
                    ! print*, cnt_vals_orig
                end do

                deallocate(y_subset, f, fff, indices, indices_diff)
                if (is_stop) stop "HOGEHOGE"
            end do




            deallocate(indices_full)
            deallocate(mat, mat_t)
            deallocate(y)
        end do
        deallocate(sum_vals, sum_vals_orig)
        deallocate(cnt_vals, cnt_vals_orig)
        deallocate(min_vals)
        deallocate(max_vals)
        deallocate(thr_vals)
    end do


end program main_sum_up_gt_with_index
