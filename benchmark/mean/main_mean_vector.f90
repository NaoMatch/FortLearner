program main_mean_vector
    use iso_c_binding
    use mod_random
    use mod_sort
    use mod_timer
    use mod_stats, only: mean, sum_up
    implicit none

    integer(kind=8) :: date_value1(8), date_value2(8)

    real(kind=8)              :: st, en
    real(kind=8)              :: sum_gt, avg_val, var_val, var_val_orig, sum_val
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

    n_iter_base = 5000000000_8
    ! n_iter_base = 1; is_stop = .true.
    is_stop = .false.
    n_types = 99
    allocate(times(n_types))
    allocate(types(n_types))
    hoge = "hoge fuga piyo"
    types = hoge
    types(1)  = "call mean_naive               :"
    types(2)  = "call mean_current_fast        :"
    types(3)  = "call mean_use_sum_up          :"

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

        print*, '================================================================================================'
        do iter_types=1, n_types, 1
            if (types(iter_types) .eq. hoge)  cycle
            call date_and_time(values=date_value1)
            var_val=0
            do iter=1, n_iter, 1
                select case(iter_types)
                    case  (1); avg_val = sum(vec)/dble(n_samples)
                    case  (2); avg_val = mean(vec, n_samples)
                    case  (3); sum_val = sum_up(vec, n_samples); avg_val=sum_val/dble(n_samples)
                end select
            end do
            call date_and_time(values=date_value2)
            times(iter_types) = time_diff(date_value1, date_value2)/dble(n_iter)
            ! print "(A, i8, f30.15, f30.15)", types(iter_types), n_samples, &
            !     real(times(iter_types)), var_val
            print "(A, i15, f30.15, f30.15, f30.15)", types(iter_types), n_samples, &
                real(times(iter_types)), avg_val, sum(vec)/n_samples
        end do

        deallocate(vec)
        if (is_stop) stop "HOGEHOGE"
    end do


end program main_mean_vector
