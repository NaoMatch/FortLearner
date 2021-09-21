program main
    use mod_random
    use mod_sort
    use mod_timer
    use mod_stats
    use mod_linalg
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)

    real(kind=8)              :: st, en, alpha
    real(kind=8)              :: sum_val, avg_val, var_val, var_val_orig
    real(kind=8), allocatable, target :: min_vals(:), max_vals(:), thr_vals(:)
    real(kind=8), allocatable, target :: sum_vals(:)
    real(kind=8), allocatable, target :: sum_vals_orig(:)
    real(kind=8), allocatable, target :: x(:), y(:), y_orig(:)
    integer(kind=8)           :: n_samples
    integer(kind=8)           :: i, j, iter, max_iter, div, n_types, iter_types, n_iter, n_iter_base
    type(c_ptr)               :: x_ptr, y_ptr, a_ptr

    integer(kind=8), allocatable :: n_samples_test(:), n_cols_test(:), n_divs_test(:)
    real(kind=8), allocatable :: times(:)
    character(len=60), allocatable :: types(:)
    character(len=30) :: hoge
    integer(kind=8)   :: r, c, d, ccc, n_iter_min
    real(kind=8)   :: rate
    character(len=256) :: filename_r8
    logical(kind=4) :: is_stop

    ! n_samples_test = (/100, 1000, 10000, 100000, 1000000, 10000000/)+57
    allocate(n_samples_test(0))
    do i=20, 96, 4
        n_samples_test = [n_samples_test, int(2**((dble(i)/4)), kind=8)]
    end do

    n_iter_base = 5000000000_8
    ! n_iter_base = 1; is_stop = .true.
    is_stop = .false.
    n_types = 99
    allocate(times(n_types))
    allocate(types(n_types))
    hoge = "hoge fuga piyo"
    types = hoge
    types(1)  = "call ax_plus_y_naive                :"
    types(2)  = "call ax_plus_y_01_F                 :"
    types(3)  = "call ax_plus_y_02_F                 :"
    types(4)  = "call ax_plus_y_04_F                 :"
    types(5)  = "call ax_plus_y_08_F                 :"
    types(6)  = "call ax_plus_y_16_F                 :"
    types(7)  = "call ax_plus_y_01_C                 :"
    types(8)  = "call ax_plus_y_02_C                 :"
    types(9)  = "call ax_plus_y_04_C                 :"
    types(10) = "call ax_plus_y_08_C                 :"
    types(11) = "call ax_plus_y_02x_A                :"
    types(12) = "call ax_plus_y_04x_A                :"
    types(13) = "call ax_plus_y_04y_A                :"
    types(14) = "call ax_plus_y_08x_A                :"
    types(15) = "call ax_plus_y_08y_A                :"
    types(16) = "call ax_plus_y_08z_A                :"
    types(17) = "call ax_plus_y_16y_A                :"
    types(18) = "call ax_plus_y_16z_A                :"
    types(19) = "call ax_plus_y_32z_A                :"
    ! types(19) = "call ax_plus_y_56z_A                :"

    do r=1, size(n_samples_test), 1
        n_samples = n_samples_test(r)-mod(n_samples_test(r),32)
        n_samples = n_samples_test(r)
        allocate(x(n_samples))
        allocate(y(n_samples))
        allocate(y_orig(n_samples))
        call random_number(x)
        call random_number(alpha)
        
        y_orig = y
        n_iter = maxval((/n_iter_base/n_samples, 1_8/))
        sum_val = sum(alpha * x + y)

        print*, '================================================================================================'
        do iter_types=1, n_types, 1
            if (types(iter_types) .eq. hoge)  cycle
            call date_and_time(values=date_value1)
            y = y_orig
            x_ptr = c_loc(x)
            y_ptr = c_loc(y)
            do iter=1, n_iter, 1
                select case(iter_types)
                    case  (1); y = alpha * x + y
                    case  (2); call ax_plus_y_01_F_r8(alpha, x, y, n_samples)
                    case  (3); call ax_plus_y_02_F_r8(alpha, x, y, n_samples)
                    case  (4); call ax_plus_y_04_F_r8(alpha, x, y, n_samples)
                    case  (5); call ax_plus_y_08_F_r8(alpha, x, y, n_samples)
                    case  (6); call ax_plus_y_16_F_r8(alpha, x, y, n_samples)
                    case  (7); call ax_plus_y_01_C(alpha, x, y, n_samples)
                    case  (8); call ax_plus_y_02_C(alpha, x, y, n_samples)
                    case  (9); call ax_plus_y_04_C(alpha, x, y, n_samples)
                    case (10); call ax_plus_y_08_C(alpha, x, y, n_samples)
                    case (11); call ax_plus_y_02x_A(alpha, x_ptr, y_ptr, n_samples)
                    case (12); call ax_plus_y_04x_A(alpha, x_ptr, y_ptr, n_samples)
                    case (13); call ax_plus_y_04y_A(alpha, x_ptr, y_ptr, n_samples)
                    case (14); call ax_plus_y_08x_A(alpha, x_ptr, y_ptr, n_samples)
                    case (15); call ax_plus_y_08y_A(alpha, x_ptr, y_ptr, n_samples)
                    case (16); call ax_plus_y_08z_A(alpha, x, y, n_samples)
                    case (17); call ax_plus_y_16y_A(alpha, x, y, n_samples)
                    case (18); call ax_plus_y_16z_A(alpha, x, y, n_samples)
                    case (19); call ax_plus_y_32z_A(alpha, x, y, n_samples)
                    case (20); call ax_plus_y_56z_A(alpha, x, y, n_samples)
                end select
            end do
            call date_and_time(values=date_value2)
            times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            print "(A, i15, f30.15, f30.15, f30.15)", types(iter_types), n_samples, &
                real(times(iter_types)), sum(x), sum(y)
        end do

        deallocate(x, y, y_orig)
        if (is_stop) stop "HOGEHOGE"
    end do


end program main
