program main
    use iso_c_binding
    use mod_random
    use mod_sort
    use mod_timer
    use mod_stats
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)

    real(kind=8)              :: st, en
    real(kind=8)              :: sum_gt, cov_val, avg1, avg2
    real(kind=8), allocatable, target :: min_vals(:), max_vals(:), thr_vals(:)
    real(kind=8), allocatable, target :: sum_vals(:)
    real(kind=8), allocatable, target :: sum_vals_orig(:)
    real(kind=8), allocatable, target :: vec1(:), vec2(:)
    integer(kind=8)           :: n_samples
    integer(kind=8)           :: i, j, iter, max_iter, div, n_types, iter_types, n_iter, n_iter_base
    type(c_ptr) :: vec_ptr

    integer(kind=8), allocatable :: n_samples_test(:), n_cols_test(:), n_divs_test(:)
    real(kind=8), allocatable :: times(:)
    character(len=60), allocatable :: types(:)
    character(len=30) :: hoge
    integer(kind=8)   :: r, c, d, ccc, n_iter_min
    real(kind=8)   :: rate
    character(len=256) :: filename_r8
    logical(kind=4) :: is_stop

    ! n_samples_test = (/100, 1000, 10000, 100000, 1000000, 10000000/)+31
    allocate(n_samples_test(0))
    do i=20, 96, 4
        n_samples_test = [n_samples_test, int(2**((dble(i)/4)), kind=8)]
    end do

    n_iter_base = 500000000_8
    ! n_iter_base = 1; is_stop = .true.
    is_stop = .false.
    n_types = 99
    allocate(times(n_types))
    allocate(types(n_types))
    hoge = "hoge fuga piyo"
    types = hoge
    types(1)  = "call covariance_naive           :"
    types(2)  = "call covariance_loop_F          :"
    types(3)  = "call covariance_loop_02_F       :"
    types(4)  = "call covariance_loop_04_F       :"
    types(5)  = "call covariance_loop_08_F       :"
    types(6)  = "call covariance_loop_16_F       :"
    types(7)  = "call covariance_loop_C          :"
    types(8)  = "call covariance_loop_02_C       :"
    types(9)  = "call covariance_loop_04_C       :"
    types(10) = "call covariance_loop_08_C       :"
    types(11) = "call covariance_loop_16_C       :"
    types(12) = "call covariance_loop_02x_A      :"
    types(13) = "call covariance_loop_04x_A      :"
    types(14) = "call covariance_loop_04y_A      :"
    types(15) = "call covariance_loop_08x_A      :"
    types(16) = "call covariance_loop_08y_A      :"
    types(17) = "call covariance_loop_08z_A      :"
    types(18) = "call covariance_loop_16y_A      :"
    types(19) = "call covariance_loop_16z_A      :"
    types(20) = "call covariance_loop_32y_A      :"
    types(21) = "call covariance_loop_32z_A      :"

    do r=1, size(n_samples_test), 1
        n_samples = n_samples_test(r)-mod(n_samples_test(r),16)
        n_samples = n_samples_test(r)
        allocate(vec1(n_samples))
        allocate(vec2(n_samples))
        call random_number(vec1)
        call random_number(vec2)

        n_iter = maxval((/n_iter_base/n_samples, 1_8/))

        print*, '================================================================================================'
        do iter_types=1, n_types, 1
            if (types(iter_types) .eq. hoge)  cycle
            call date_and_time(values=date_value1)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case  (1)
                        avg1 = mean(vec1, n_samples)
                        avg2 = mean(vec2, n_samples)
                        cov_val = sum((vec1-avg1)*(vec2-avg2))/dble(n_samples)
                    case  (2); cov_val = covariance_loop_F(vec1, vec2, n_samples)
                    case  (3); cov_val = covariance_loop_02_F(vec1, vec2, n_samples)
                    case  (4); cov_val = covariance_loop_04_F(vec1, vec2, n_samples)
                    case  (5); cov_val = covariance_loop_08_F(vec1, vec2, n_samples)
                    case  (6); cov_val = covariance_loop_16_F(vec1, vec2, n_samples)
                    case  (7); cov_val = covariance_loop_C(vec1, vec2, n_samples)
                    case  (8); cov_val = covariance_loop_02_C(vec1, vec2, n_samples)
                    case  (9); cov_val = covariance_loop_04_C(vec1, vec2, n_samples)
                    case (10); cov_val = covariance_loop_08_C(vec1, vec2, n_samples)
                    case (11); cov_val = covariance_loop_16_C(vec1, vec2, n_samples)
                    case (12); cov_val = covariance_loop_02x_A(vec1, vec2, n_samples)
                    case (13); cov_val = covariance_loop_04x_A(vec1, vec2, n_samples)
                    case (14); cov_val = covariance_loop_04y_A(vec1, vec2, n_samples)
                    case (15); cov_val = covariance_loop_08x_A(vec1, vec2, n_samples)
                    case (16); cov_val = covariance_loop_08y_A(vec1, vec2, n_samples)
                    case (17); cov_val = covariance_loop_08z_A(vec1, vec2, n_samples)
                    case (18); cov_val = covariance_loop_16y_A(vec1, vec2, n_samples)
                    case (19); cov_val = covariance_loop_16z_A(vec1, vec2, n_samples)
                    case (20); cov_val = covariance_loop_32y_A(vec1, vec2, n_samples)
                    case (21); cov_val = covariance_loop_32z_A(vec1, vec2, n_samples)
                end select
            end do
            call date_and_time(values=date_value2)
            times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            ! print "(A, i8, f30.15, f30.15)", types(iter_types), n_samples, &
            !     real(times(iter_types)), var_val
            print "(A, i15, f30.15, f30.15)", types(iter_types), n_samples, &
                real(times(iter_types)), cov_val
        end do

        deallocate(vec1, vec2)
        if (is_stop) stop "HOGEHOGE"
    end do


end program main
