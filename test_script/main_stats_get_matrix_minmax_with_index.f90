program main_stats_get_matrix_minmax_with_index
    use mod_timer
    use mod_random
    use mod_stats, only: get_matrix_minmax_r8, get_matrix_minmax, get_minmax_hybrid_r8, get_matrix_minmax_col_major_loop_with_index
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)
    
    integer(kind=8)           :: n_samples, mat_size(5), n_columns, col_size(5), n_indices, idx_size(5), i, j, k, l
    real(kind=8), allocatable :: min_vals_fast(:), min_vals_naive(:)
    real(kind=8), allocatable :: max_vals_fast(:), max_vals_naive(:)
    real(kind=8), allocatable :: mat(:,:), mat_t(:,:), vec(:)
    integer(kind=8), allocatable :: idx(:), idx_full(:)

    mat_size = (/100, 1000, 10000, 100000, 1000000/)
    col_size = (/4, 8, 16, 32, 64/)
    idx_size = (/1, 2, 4,  8,  16/)

    print*, "           n_samples                 fast                     naive       diff"
    do i=1, 5, 1
        n_samples = mat_size(i)
        n_indices = int(n_samples/idx_size(i))

        do k=1, 5, 1
            n_columns = col_size(k)
            allocate(mat(n_samples, n_columns))
            allocate(mat_t(n_columns, n_samples))
            allocate(idx_full(n_samples))
            allocate(idx(n_indices))
            allocate(vec(n_indices))
            allocate(min_vals_fast(n_columns), min_vals_naive(n_columns), & 
                    max_vals_fast(n_columns), max_vals_naive(n_columns))
            call random_number(mat_t)
            call random_number(mat)
            do j=1, n_samples, 1
                idx_full(j) = j
            end do
            call permutation(idx_full, n_samples)
            idx = idx_full(1:n_indices)

            mat_t = transpose(mat)
            min_vals_fast = huge(0d0)
            max_vals_fast = -huge(0d0)
            call get_matrix_minmax_r8(min_vals_fast, max_vals_fast, mat_t, idx, & 
                n_indices, n_samples, n_columns)
            do l=1, n_columns, 1
                do j=1, n_indices, 1
                    vec(j) = mat_t(l,idx(j))
                end do
                call get_minmax_hybrid_r8(min_vals_naive(l), max_vals_naive(l), vec, n_indices)
            end do
            print*, n_samples, sum(min_vals_fast), sum(min_vals_naive), sum(min_vals_naive-min_vals_fast)
            print*, n_samples, sum(max_vals_fast), sum(max_vals_naive), sum(max_vals_naive-max_vals_fast)

        deallocate(mat, mat_t, vec, idx_full, idx, min_vals_fast, min_vals_naive, max_vals_fast, max_vals_naive)
        end do
    end do


end program main_stats_get_matrix_minmax_with_index
