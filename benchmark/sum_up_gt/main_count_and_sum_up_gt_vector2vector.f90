program main_count_and_sum_up_gt_vector2vector
    use iso_c_binding
    use mod_random
    use mod_sort
    use mod_timer
    use mod_stats
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)

    real(kind=8)              :: st, en
    real(kind=8)              :: sum_gt, cov_val, avg1, avg2
    real(kind=8), allocatable, target    :: sum_vals(:)
    real(kind=8), allocatable, target    :: sum_vals_orig(:)
    real(kind=8), allocatable, target    :: x_vals(:), thr_vals(:)
    integer(kind=8), allocatable, target :: cnt_vals(:)
    integer(kind=8)           :: n_samples
    integer(kind=8)           :: i, j, iter, max_iter, div, n_types, iter_types, n_iter, n_iter_base
    type(c_ptr) :: vec_ptr

    integer(kind=8), allocatable :: n_samples_test(:), n_cols_test(:), n_divs_test(:)
    real(kind=8), allocatable :: times(:)
    character(len=60), allocatable :: types(:)
    character(len=30) :: hoge
    integer(kind=8)   :: r, c, d, ccc, n_iter_min
    real(kind=8)   :: rate, y_val
    character(len=256) :: filename_r8
    logical(kind=4) :: is_stop

    ! n_samples_test = (/100, 1000, 10000, 100000, 1000000, 10000000/)+31
    allocate(n_samples_test(0))
    do i=20, 96, 4
        n_samples_test = [n_samples_test, int(2**((dble(i)/8)), kind=8)]
    end do

    is_stop = .false.
    n_iter_base = 500000000_8
    ! n_iter_base = 1; is_stop = .true.
    n_types = 99
    allocate(times(n_types))
    allocate(types(n_types))
    hoge = "hoge fuga piyo"
    types = hoge
    types(1)  = "call count_and_sum_up_gt_vector2vector_01_F_r8    :"
    types(2)  = "call count_and_sum_up_gt_vector2vector_02_F_r8    :"
    types(3)  = "call count_and_sum_up_gt_vector2vector_04_F_r8    :"
    types(4)  = "call count_and_sum_up_gt_vector2vector_08_F_r8    :"
    types(5)  = "call count_and_sum_up_gt_vector2vector_16_F_r8    :"
    types(6)  = "call count_and_sum_up_gt_vector2vector_01_C_r8    :"
    types(7)  = "call count_and_sum_up_gt_vector2vector_02_C_r8    :"
    types(8)  = "call count_and_sum_up_gt_vector2vector_04_C_r8    :"
    types(9)  = "call count_and_sum_up_gt_vector2vector_08_C_r8    :"
    types(10) = "call count_and_sum_up_gt_vector2vector_16_C_r8    :"
    types(11) = "call count_and_sum_up_gt_vector2vector_02x_A_r8   :"
    types(12) = "call count_and_sum_up_gt_vector2vector_04x_A_r8   :"
    types(13) = "call count_and_sum_up_gt_vector2vector_04y_A_r8   :"
    types(14) = "call count_and_sum_up_gt_vector2vector_08y_A_r8   :"
    types(15) = "call count_and_sum_up_gt_vector2vector_08z_A_r8   :"
    types(16) = "call count_and_sum_up_gt_vector2vector_16y_A_r8   :"
    types(17) = "call count_and_sum_up_gt_vector2vector_16z_A_r8   :"
    types(18) = "call count_and_sum_up_gt_vector2vector_32y_A_r8   :"
    types(19) = "call count_and_sum_up_gt_vector2vector_32z_A_r8   :"

    do r=1, size(n_samples_test), 1
        n_samples = n_samples_test(r)
        ! n_samples = 32_8 + maxval((/32_8, n_samples/))
        ! n_samples = n_samples_test(r)
        allocate(x_vals(n_samples))
        allocate(thr_vals(n_samples))
        allocate(sum_vals(n_samples))
        allocate(cnt_vals(n_samples))
        call random_number(x_vals)
        call random_number(thr_vals)
        call random_number(y_val)

        n_iter = maxval((/n_iter_base/n_samples, 1_8/))

        print*, '================================================================================================'
        do iter_types=1, n_types, 1
            if (types(iter_types) .eq. hoge)  cycle
            sum_vals = 0
            cnt_vals = 0
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case  (1)
                        call count_and_sum_up_gt_vector2vector_01_F_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case  (2)
                        call count_and_sum_up_gt_vector2vector_02_F_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case  (3)
                        call count_and_sum_up_gt_vector2vector_04_F_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case  (4)
                        call count_and_sum_up_gt_vector2vector_08_F_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case  (5)
                        call count_and_sum_up_gt_vector2vector_16_F_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case  (6)
                        call count_and_sum_up_gt_vector2vector_01_C_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case  (7)
                        call count_and_sum_up_gt_vector2vector_02_C_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case  (8)
                        call count_and_sum_up_gt_vector2vector_04_C_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case  (9)
                        call count_and_sum_up_gt_vector2vector_08_C_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case (10)
                        call count_and_sum_up_gt_vector2vector_16_C_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case (11)
                        call count_and_sum_up_gt_vector2vector_02x_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case (12)
                        call count_and_sum_up_gt_vector2vector_04x_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case (13)
                        call count_and_sum_up_gt_vector2vector_04y_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case (14)
                        call count_and_sum_up_gt_vector2vector_08y_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case (15)
                        call count_and_sum_up_gt_vector2vector_08z_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case (16)
                        call count_and_sum_up_gt_vector2vector_16y_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case (17)
                        call count_and_sum_up_gt_vector2vector_16z_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case (18)
                        call count_and_sum_up_gt_vector2vector_32y_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                    case (19)
                        call count_and_sum_up_gt_vector2vector_32z_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n_samples)
                end select
            end do
            call date_and_time(values=date_value2)
            times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print "(A, i15, f30.15, f30.15, i15)", types(iter_types), n_samples, &
                real(times(iter_types)), sum(sum_vals), sum(cnt_vals)
        end do

        deallocate(x_vals, thr_vals, sum_vals, cnt_vals)
        if (is_stop) stop "HOGEHOGE"
    end do

end program main_count_and_sum_up_gt_vector2vector
