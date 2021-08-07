program main_stats_sum_up_matrix
    use mod_timer
    use mod_stats, only: sum_up_matrix
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)
    
    integer(kind=8)           :: n_columns, n_samples, vec_size(5), i
    real(kind=8), allocatable :: mat(:,:)
    real(kind=8), allocatable :: sum_vals_fast(:), sum_vals_naive(:)


    vec_size = (/100, 1000, 10000, 100000, 1000000/)

    print*, "           n_samples                 diff"
    do i=1, 5, 1
        n_columns = 50
        n_samples = vec_size(i)
        allocate(sum_vals_fast(n_columns))
        allocate(sum_vals_naive(n_columns))
        allocate(mat(n_samples, n_columns))
        call random_number(mat)

        call sum_up_matrix(sum_vals_fast, mat, n_samples, n_columns)
        sum_vals_naive = sum(mat, dim=1)

        print*, n_samples, sum(sum_vals_fast-sum_vals_naive)

        deallocate(mat)
        deallocate(sum_vals_fast, sum_vals_naive)
    end do


end program main_stats_sum_up_matrix
