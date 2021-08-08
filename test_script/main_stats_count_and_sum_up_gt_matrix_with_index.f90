program main_stats_count_and_sum_up_gt_matrix_with_index
    use mod_timer
    use mod_random
    use mod_stats, only: get_matrix_count_and_sum_up_gt, count_and_sum_up_gt
    implicit none
    integer(kind=8) :: date_value1(8), date_value2(8)
    
    integer(kind=8)           :: n_samples, mat_size(5), n_columns, col_size(5), n_indices, idx_size(5), i, j, k, l
    real(kind=8), allocatable :: sum_vals_fast(:), sum_vals_naive(:)
    integer(kind=8), allocatable :: cnt_vals_fast(:), cnt_vals_naive(:)
    real(kind=8), allocatable :: thr_vals(:)
    real(kind=8), allocatable :: mat(:,:), mat_t(:,:), vec(:), y_subset(:), y(:)
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
            allocate(y(n_samples))
            allocate(idx(n_indices))
            allocate(vec(n_indices))
            allocate(y_subset(n_indices))
            allocate(sum_vals_fast(n_columns), sum_vals_naive(n_columns), & 
                    cnt_vals_fast(n_columns), cnt_vals_naive(n_columns), thr_vals(n_columns))
            call random_number(mat)
            call random_number(y)
            do j=1, n_samples, 1
                idx_full(j) = j
            end do
            call permutation(idx_full, n_samples)
            idx = idx_full(1:n_indices)
            thr_vals = 0.5d0

            mat_t = transpose(mat)
            sum_vals_fast = 0
            cnt_vals_fast = 0
            call get_matrix_count_and_sum_up_gt(sum_vals_fast, cnt_vals_fast, thr_vals, mat_t, y, idx, & 
                n_indices, n_samples, n_columns)

            sum_vals_naive = 0
            cnt_vals_naive = 0
            do l=1, n_columns, 1
                do j=1, n_indices, 1
                    vec(j) = mat_t(l,idx(j))
                    y_subset(j) = y(idx(j))
                end do
                call count_and_sum_up_gt(sum_vals_naive(l), cnt_vals_naive(l), y_subset, vec, thr_vals(l), n_indices)
            end do
            print*, sum(sum_vals_fast-sum_vals_naive), sum(cnt_vals_fast-cnt_vals_naive)

        deallocate(mat, mat_t, vec, y_subset, y, idx_full, idx, sum_vals_fast, sum_vals_naive, & 
            cnt_vals_fast, cnt_vals_naive, thr_vals)
        end do
    end do


end program main_stats_count_and_sum_up_gt_matrix_with_index
