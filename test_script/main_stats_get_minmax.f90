program main_stats_get_minmax
    use mod_timer
    use mod_stats, only: get_minmax
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)
    
    integer(kind=8)           :: n_samples, vec_size(5), i
    real(kind=8)              :: min_val_fast, min_val_naive
    real(kind=8)              :: max_val_fast, max_val_naive
    real(kind=8), allocatable :: vec(:)


    vec_size = (/100, 1000, 10000, 100000, 1000000/)

    print*, "           n_samples                 fast                     naive       diff"
    do i=1, 5, 1
        n_samples = vec_size(i)
        allocate(vec(n_samples))
        call random_number(vec)

        call get_minmax(min_val_fast, max_val_fast, vec, n_samples)
        min_val_naive = minval(vec)
        max_val_naive = maxval(vec)

        print*, "MIN: ", n_samples, min_val_fast, min_val_naive, min_val_fast-min_val_naive
        print*, "MAX: ", n_samples, max_val_fast, max_val_naive, max_val_fast-max_val_naive

        deallocate(vec)
    end do


end program main_stats_get_minmax
