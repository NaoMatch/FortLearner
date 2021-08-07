program main_stats_mean_vector
    use mod_timer
    use mod_stats, only: mean
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)
    
    integer(kind=8)           :: n_samples, vec_size(5), i
    real(kind=8)              :: avg_val_fast, avg_val_naive
    real(kind=8), allocatable :: vec(:)


    vec_size = (/100, 1000, 10000, 100000, 1000000/)

    print*, "           n_samples                 fast                     naive       diff"
    do i=1, 5, 1
        n_samples = vec_size(i)
        allocate(vec(n_samples))
        call random_number(vec)

        avg_val_fast = mean(vec, n_samples)
        avg_val_naive = sum(vec)/dble(n_samples)

        print*, n_samples, avg_val_fast, avg_val_naive, avg_val_fast-avg_val_naive

        deallocate(vec)
    end do


end program main_stats_mean_vector
