program main_stats_covariance
    use mod_timer
    use mod_stats, only: covariance_fast, mean
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)
    
    integer(kind=8)           :: n_samples, vec_size(5), i
    real(kind=8)              :: cov_val_fast, cov_val_naive
    real(kind=8)              :: avg1, avg2
    real(kind=8), allocatable :: vec1(:), vec2(:)


    vec_size = (/100, 1000, 10000, 100000, 1000000/)

    print*, "           n_samples                 fast                     naive       diff"
    do i=1, 5, 1
        n_samples = vec_size(i)
        allocate(vec1(n_samples))
        allocate(vec2(n_samples))
        call random_number(vec1)
        call random_number(vec2)

        cov_val_fast  = covariance_fast(vec1, vec2, n_samples)

        avg1 = mean(vec1, n_samples)
        avg2 = mean(vec2, n_samples)
        cov_val_naive = sum(vec1*vec2)/n_samples - avg1*avg2

        print*, n_samples, cov_val_fast, cov_val_naive, cov_val_fast-cov_val_naive

        deallocate(vec1, vec2)
    end do


end program main_stats_covariance
