program main_variance_vector
    use mod_random
    use mod_sort
    use mod_timer
    use mod_stats
    use mod_variance
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)

    real(kind=8)              :: st, en
    real(kind=8)              :: sum_gt, avg_val, var_val, var_val_orig
    real(kind=8), allocatable, target :: min_vals(:), max_vals(:), thr_vals(:)
    real(kind=8), allocatable, target :: sum_vals(:)
    real(kind=8), allocatable, target :: sum_vals_orig(:)
    real(kind=8), allocatable, target :: vec(:)
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

    n_samples_test = (/100, 1000, 10000, 100000, 1000000, 10000000/)+15

    n_iter_base = 500000000_8
    ! n_iter_base = 1; is_stop = .true.
    is_stop = .false.
    n_types = 99
    allocate(times(n_types))
    allocate(types(n_types))
    hoge = "hoge fuga piyo"
    types = hoge
    types(1)  = "call variance_naive                      :"
    types(2)  = "call variance_current_best               :"
    types(3)  = "call variance_02_F_r8                    :"
    types(4)  = "call variance_04_F_r8                    :"
    types(5)  = "call variance_08_F_r8                    :"
    types(6)  = "call variance_16_F_r8                    :"
    types(7)  = "call variance_02_C_r8                    :"
    types(8)  = "call variance_04_C_r8                    :"
    types(9)  = "call variance_08_C_r8                    :"
    types(10) = "call variance_16_C_r8                    :"
    types(11) = "call variance_02_A_r8                    :"
    types(12) = "call variance_04_A_r8                    :"
    types(13) = "call variance_08_A_r8                    :"
    types(14) = "call variance_08z_A_r8                   :"
    types(15) = "call variance_16_A_r8                    :"
    types(16) = "call variance_16z_A_r8                   :"

    do r=1, size(n_samples_test), 1
        n_samples = n_samples_test(r)-mod(n_samples_test(r),16)
        n_samples = n_samples_test(r)
        allocate(vec(n_samples))
        call random_number(vec)
        ! vec = 10000*vec
        ! vec = 10
        ! vec(n_samples) = 10000
        vec_ptr = c_loc(vec)
        
        write (filename_r8, '("time_get_matrix_minmax_with_index_r8_n", i8.8, ".txt")') n_samples

        n_iter = maxval((/n_iter_base/n_samples, 1_8/))
        avg_val=mean(vec, n_samples)
        var_val_orig = variance(vec, n_samples, avg_val)

        print*, '================================================================================================'
        do iter_types=1, n_types, 1
            if (types(iter_types) .eq. hoge)  cycle
            call date_and_time(values=date_value1)
            var_val=0
            do iter=1, n_iter, 1
                select case(iter_types)
                    case  (1); avg_val=mean(vec, n_samples); var_val = sum((vec-avg_val)**2d0)/dble(n_samples)
                    case  (2); var_val = variance(vec, n_samples)
                    case  (3); var_val = variance_02_F_r8(vec, n_samples)
                    case  (4); var_val = variance_04_F_r8(vec, n_samples)
                    case  (5); var_val = variance_08_F_r8(vec, n_samples)
                    case  (6); var_val = variance_16_F_r8(vec, n_samples)
                    case  (7); var_val = variance_02_C_r8(vec, n_samples)
                    case  (8); var_val = variance_04_C_r8(vec, n_samples)
                    case  (9); var_val = variance_08_C_r8(vec, n_samples)
                    case (10); var_val = variance_16_C_r8(vec, n_samples)
                    case (11); var_val = variance_02_A_r8(vec, n_samples)
                    case (12); var_val = variance_04_A_r8(vec, n_samples)
                    case (13); var_val = variance_08_A_r8(vec, n_samples)
                    case (14); var_val = variance_08z_A_r8(vec, n_samples)
                    case (15); var_val = variance_16_A_r8(vec, n_samples)
                    case (16); var_val = variance_16z_A_r8(vec, n_samples)
                end select
            end do
            call date_and_time(values=date_value2)
            times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            ! print "(A, i8, f30.15, f30.15)", types(iter_types), n_samples, &
            !     real(times(iter_types)), var_val
            print "(A, i15, f30.15, f30.15, f30.15, f30.15)", types(iter_types), n_samples, &
                real(times(iter_types)), var_val, sum(vec), sum(vec**2d0)
        end do

        deallocate(vec)
        if (is_stop) stop "HOGEHOGE"
    end do


end program main_variance_vector
