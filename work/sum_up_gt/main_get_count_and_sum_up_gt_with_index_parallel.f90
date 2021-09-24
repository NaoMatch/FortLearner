program main_get_count_and_sum_up_gt_with_index_parallel
    !$ use omp_lib
    use mod_random
    use mod_sort
    use mod_timer
    use mod_stats
    implicit none
    
    integer(kind=8) :: date_value1(8), date_value2(8)

    real(kind=8)              :: st, en
    real(kind=8)              :: min_val, max_val
    real(kind=8), allocatable, target :: sum_vals(:), thr_vals(:)
    real(kind=8), allocatable, target :: min_vals(:), max_vals(:)
    real(kind=8), allocatable, target :: min_vals_orig(:), max_vals_orig(:)
    real(kind=8), allocatable, target :: mat(:,:), mat_t(:,:), f(:), y(:)
    real(kind=8), allocatable, target :: thresholds(:)
    integer(kind=8), allocatable, target :: indices(:), indices_diff(:), indices_full(:), cnt_vals(:)
    integer(kind=8)           :: n_rows, n_cols, idx, n_indices, n_idxs
    integer(kind=8)           :: i, j, iter, max_iter, div, n_types, iter_types, n_iter, n_iter_base
    type(c_ptr) :: min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_ptr, indices_diff_ptr, thresholds_ptr, y_ptr
    type(c_ptr) :: sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr

    integer(kind=8), allocatable :: n_rows_test(:), n_cols_test(:), n_divs_test(:)
    real(kind=8), allocatable :: times(:)
    character(len=60), allocatable :: types(:)
    character(len=30) :: hoge
    integer(kind=8)   :: r, c, d, n_thds 
    real(kind=8)   :: rate
    character(len=256) :: filename_r8

    n_rows_test = (/100, 1000, 10000, 100000, 1000000/)
    n_cols_test = (/4,    8,     16,     32,      64/)
    n_divs_test = (/1,    2,     4,      8,       16/)
    n_thds = 1
    ! n_cols_test = n_cols_test + 15
    n_rows_test = n_rows_test(size(n_rows_test):1:-1)
    n_cols_test = n_cols_test(size(n_cols_test):1:-1)
    n_divs_test = n_divs_test(size(n_divs_test):1:-1)

    n_iter_base = 500000000000_8
    ! n_iter_base = 50000000_8
    ! n_iter_base = 1
    n_types = 99
    n_thds = 4
    allocate(times(n_types))
    allocate(types(n_types))
    hoge = "hoge fuga piyo"
    types = hoge
    types(1)  = "call get_matrix_count_and_sum_up_gt                           :"
    types(2)  = "call get_matrix_count_and_sum_up_gt_01_F_parallel_r8          :"
    ! types(3)  = "call get_matrix_count_and_sum_up_gt_02_F_parallel_r8          :"
    ! types(4)  = "call get_matrix_count_and_sum_up_gt_04_F_parallel_r8          :"
    ! types(5)  = "call get_matrix_count_and_sum_up_gt_08_F_parallel_r8          :"
    ! types(6)  = "call get_matrix_count_and_sum_up_gt_16_F_parallel_r8          :"
    ! types(7)  = "call get_matrix_count_and_sum_up_gt_with_index_parallel_01_C  :"
    ! types(8)  = "call get_matrix_count_and_sum_up_gt_with_index_parallel_02_C  :"
    ! types(9)  = "call get_matrix_count_and_sum_up_gt_with_index_parallel_04_C  :"
    ! types(10) = "call get_matrix_count_and_sum_up_gt_with_index_parallel_08_C  :"
    ! types(11) = "call get_matrix_count_and_sum_up_gt_with_index_parallel_16_C  :"
    ! types(12) = "call get_matrix_count_and_sum_up_gt_with_index_parallel_02x_A :"
    ! types(13) = "call get_matrix_count_and_sum_up_gt_with_index_parallel_04x_A :"
    ! types(14) = "call get_matrix_count_and_sum_up_gt_with_index_parallel_04y_A :"
    ! types(15) = "call get_matrix_count_and_sum_up_gt_with_index_parallel_08y_A :"
    ! types(16) = "call get_matrix_count_and_sum_up_gt_with_index_parallel_08z_A :"
    ! types(17) = "call get_matrix_count_and_sum_up_gt_with_index_parallel_16y_A :"
    ! types(18) = "call get_matrix_count_and_sum_up_gt_with_index_parallel_16z_A :"
    ! types(19) = "call get_matrix_count_and_sum_up_gt_with_index_parallel_32y_A :"
    ! types(20) = "call get_matrix_count_and_sum_up_gt_with_index_parallel_32z_A :"

    do c=1, size(n_cols_test), 1
        n_cols = n_cols_test(c)

        allocate(sum_vals(n_cols))
        allocate(cnt_vals(n_cols))
        allocate(thr_vals(n_cols))
        call random_number(thr_vals)
        sum_vals_ptr = c_loc(sum_vals)
        cnt_vals_ptr = c_loc(cnt_vals)
        thr_vals_ptr = c_loc(thr_vals)


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


            allocate(y(n_rows))
            call random_number(y)
            y_ptr = c_loc(y)

            do d=1, size(n_divs_test), 1
                write (filename_r8, '("time_get_matrix_minmax_with_index_r8_c", i4.4, "_r", i8.8, "_d", i4.4, ".txt")') & 
                    n_cols, n_rows, n_divs_test(d)

                n_indices = minval((/n_rows / dble(n_divs_test(d)), 1d0/))

                allocate(indices(n_indices))
                indices = indices_full(1:n_indices)
                call quick_sort(indices, n_indices)
                indices_ptr = c_loc(indices)

                n_iter=maxval((/n_iter_base/n_rows/n_cols*n_divs_test(d), 1_8/))
                print*, n_iter

                print*, '================================================================================================'
                do iter_types=1, n_types, 1
                    if (types(iter_types) .eq. hoge)  cycle
                    call date_and_time(values=date_value1)
                    st = omp_get_wtime()
                    sum_vals = 0
                    cnt_vals = 0
                    do iter=1, n_iter, 1
                        select case(iter_types)
                            case  (1)
                                call get_matrix_count_and_sum_up_r8(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
                                        n_indices, n_rows, n_cols)
                            ! case  (2)
                            !     call get_matrix_count_and_sum_up_gt_with_index_02(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
                            !             n_indices, n_rows, n_cols)
                            case  (2)
                                call get_matrix_count_and_sum_up_gt_01_F_parallel_r8(sum_vals, cnt_vals, thr_vals, mat_t, y, & 
                                        indices, n_indices, n_rows, n_cols, n_thds)
                            ! case  (3)
                            !     call get_matrix_count_and_sum_up_gt_02_F_parallel_r8(sum_vals, cnt_vals, thr_vals, mat_t, y, & 
                            !             indices, n_indices, n_rows, n_cols, n_thds)
                            ! case  (4)
                            !     call get_matrix_count_and_sum_up_gt_04_F_parallel_r8(sum_vals, cnt_vals, thr_vals, mat_t, y, & 
                            !             indices, n_indices, n_rows, n_cols, n_thds)
                            ! case  (5)
                            !     call get_matrix_count_and_sum_up_gt_08_F_parallel_r8(sum_vals, cnt_vals, thr_vals, mat_t, y, & 
                            !             indices, n_indices, n_rows, n_cols, n_thds)
                            ! case  (6)
                            !     call get_matrix_count_and_sum_up_gt_16_F_parallel_r8(sum_vals, cnt_vals, thr_vals, mat_t, y, & 
                            !             indices, n_indices, n_rows, n_cols, n_thds)
                            ! case  (7)
                            !     call get_matrix_count_and_sum_up_gt_with_index_parallel_01_C(& 
                            !             sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, mat_t_ptr, y_ptr, & 
                            !             indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            ! case  (8)
                            !     call get_matrix_count_and_sum_up_gt_with_index_parallel_02_C(& 
                            !             sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, mat_t_ptr, y_ptr, & 
                            !             indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            ! case  (9)
                            !     call get_matrix_count_and_sum_up_gt_with_index_parallel_04_C(& 
                            !             sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, mat_t_ptr, y_ptr, & 
                            !             indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            ! case  (10)
                            !     call get_matrix_count_and_sum_up_gt_with_index_parallel_08_C(& 
                            !             sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, mat_t_ptr, y_ptr, & 
                            !             indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            ! case  (11)
                            !     call get_matrix_count_and_sum_up_gt_with_index_parallel_16_C(& 
                            !             sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, mat_t_ptr, y_ptr, & 
                            !             indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            ! case  (12)
                            !     call get_matrix_count_and_sum_up_gt_with_index_parallel_02x_A(& 
                            !             sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, mat_t_ptr, y_ptr, & 
                            !             indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            ! case  (13)
                            !     call get_matrix_count_and_sum_up_gt_with_index_parallel_04x_A(& 
                            !             sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, mat_t_ptr, y_ptr, & 
                            !             indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            ! case  (14)
                            !     call get_matrix_count_and_sum_up_gt_with_index_parallel_04y_A(& 
                            !             sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, mat_t_ptr, y_ptr, & 
                            !             indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            ! case  (15)
                            !     call get_matrix_count_and_sum_up_gt_with_index_parallel_08y_A(& 
                            !             sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, mat_t_ptr, y_ptr, & 
                            !             indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            ! case  (16)
                            !     call get_matrix_count_and_sum_up_gt_with_index_parallel_08z_A(& 
                            !             sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, mat_t_ptr, y_ptr, & 
                            !             indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            ! case  (17)
                            !     call get_matrix_count_and_sum_up_gt_with_index_parallel_16y_A(& 
                            !             sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, mat_t_ptr, y_ptr, & 
                            !             indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            ! case  (18)
                            !     call get_matrix_count_and_sum_up_gt_with_index_parallel_16z_A(& 
                            !             sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, mat_t_ptr, y_ptr, & 
                            !             indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            ! case  (19)
                            !     call get_matrix_count_and_sum_up_gt_with_index_parallel_32y_A(& 
                            !             sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, mat_t_ptr, y_ptr, & 
                            !             indices_ptr, n_indices, n_rows, n_cols, n_thds)
                            ! case  (20)
                            !     call get_matrix_count_and_sum_up_gt_with_index_parallel_32z_A(& 
                            !             sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, mat_t_ptr, y_ptr, & 
                            !             indices_ptr, n_indices, n_rows, n_cols, n_thds)
                        end select
                    end do
                    en = omp_get_wtime()
                    call date_and_time(values=date_value2)
                    times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
                    rate = 1/dble(n_divs_test(d))*100
                    print "(A, i15, i15, f12.4, f20.10, f20.10, i15, f20.10)", types(iter_types), n_cols, n_rows, rate, &
                        real(times(iter_types)), sum(sum_vals), sum(cnt_vals), (en-st)/dble(n_iter)
                end do
                deallocate(indices)
            end do

            deallocate(indices_full)
            deallocate(mat, mat_t)
            deallocate(y)
        end do
        deallocate(sum_vals)
        deallocate(cnt_vals)
        deallocate(thr_vals)
    end do


    stop "HOGE"

end program main_get_count_and_sum_up_gt_with_index_parallel
