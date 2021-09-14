program main_stats_mean_matrix
    use mod_timer
    use mod_stats, only: mean
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)
    
    integer(kind=8)           :: n_columns, n_samples, vec_size(5), i
    real(kind=8), allocatable :: mat(:,:)
    real(kind=8), allocatable :: avg_vals_fast(:), avg_vals_naive(:)


    vec_size = (/100, 1000, 10000, 100000, 1000000/)

    print*, "           n_samples                 fast                     naive         diff"
    do i=1, 5, 1
        n_columns = 50
        n_samples = vec_size(i)
        allocate(avg_vals_fast(n_columns))
        allocate(avg_vals_naive(n_columns))
        allocate(mat(n_samples, n_columns))
        call random_number(mat)

        avg_vals_fast = mean(mat, n_samples, n_columns)
        avg_vals_naive = sum(mat, dim=1)/dble(n_samples)

        print*, n_samples, sum(avg_vals_fast), sum(avg_vals_naive), sum(avg_vals_fast-avg_vals_naive)

        deallocate(mat)
        deallocate(avg_vals_fast, avg_vals_naive)
    end do


end program main_stats_mean_matrix
