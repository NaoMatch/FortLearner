program main_stats_variance_vector
    use mod_timer
    use mod_stats, only: variance, variance_fast
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)
    
    integer(kind=8)           :: n_samples, vec_size(5), i
    real(kind=8)              :: var_val_fast, var_val_naive
    real(kind=8), allocatable :: vec(:)


    vec_size = (/100, 1000, 10000, 100000, 1000000/)

    print*, "           n_samples                 fast                     naive       diff"
    do i=1, 5, 1
        n_samples = vec_size(i)
        allocate(vec(n_samples))
        call random_number(vec)

        var_val_fast  = variance_fast(vec, n_samples)
        var_val_naive = variance(vec, n_samples)

        print*, n_samples, var_val_fast, var_val_naive, var_val_fast-var_val_naive

        deallocate(vec)
    end do


end program main_stats_variance_vector
