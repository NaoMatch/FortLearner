subroutine get_matrix_count_and_sum_up_gt_01_F_parallel_r8(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
    n_indices, n_rows, n_cols, n_thds)
    implicit none
    real(kind=8), intent(inout), target    :: sum_vals(n_cols)
    integer(kind=8), intent(inout), target :: cnt_vals(n_cols)
    real(kind=8), intent(inout), target    :: thr_vals(n_cols)
    real(kind=8), intent(in)   , target    :: mat_t(n_cols, n_rows), y(n_rows)
    integer(kind=8), intent(in), target    :: indices(n_indices)
    integer(kind=8), intent(in)            :: n_indices, n_rows, n_cols, n_thds
    integer(kind=8)            :: i, idx
    real(kind=8)            :: y_val


    sum_vals=0
    cnt_vals=0
    call omp_set_num_threads(n_thds)
    !$omp parallel
    !$omp do reduction(+:sum_vals) reduction(+:cnt_vals) private(idx, i, y_val)
    do i=1, n_indices, 1
        idx = indices(i)
        y_val = y(idx)
        call count_and_sum_up_gt_vector2vector_01_F_r8(mat_t(1:n_cols,idx), thr_vals, sum_vals, cnt_vals, y_val, n_cols)
    end do
    !$omp end do
    !$omp end parallel
end subroutine get_matrix_count_and_sum_up_gt_01_F_parallel_r8

subroutine get_matrix_count_and_sum_up_gt_02_F_parallel_r8(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
    n_indices, n_rows, n_cols, n_thds)
    implicit none
    real(kind=8), intent(inout), target    :: sum_vals(n_cols)
    integer(kind=8), intent(inout), target :: cnt_vals(n_cols)
    real(kind=8), intent(inout), target    :: thr_vals(n_cols)
    real(kind=8), intent(in)   , target    :: mat_t(n_cols, n_rows), y(n_rows)
    integer(kind=8), intent(in), target    :: indices(n_indices)
    integer(kind=8), intent(in)            :: n_indices, n_rows, n_cols, n_thds
    integer(kind=8)            :: i, idx
    real(kind=8)            :: y_val


    sum_vals=0
    cnt_vals=0
    call omp_set_num_threads(n_thds)
    !$omp parallel
    !$omp do reduction(+:sum_vals) reduction(+:cnt_vals) private(idx, i, y_val)
    do i=1, n_indices, 1
        idx = indices(i)
        y_val = y(idx)
        call count_and_sum_up_gt_vector2vector_02_F_r8(mat_t(1:n_cols,idx), thr_vals, sum_vals, cnt_vals, y_val, n_cols)
    end do
    !$omp end do
    !$omp end parallel
end subroutine get_matrix_count_and_sum_up_gt_02_F_parallel_r8

subroutine get_matrix_count_and_sum_up_gt_04_F_parallel_r8(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
    n_indices, n_rows, n_cols, n_thds)
    implicit none
    real(kind=8), intent(inout), target    :: sum_vals(n_cols)
    integer(kind=8), intent(inout), target :: cnt_vals(n_cols)
    real(kind=8), intent(inout), target    :: thr_vals(n_cols)
    real(kind=8), intent(in)   , target    :: mat_t(n_cols, n_rows), y(n_rows)
    integer(kind=8), intent(in), target    :: indices(n_indices)
    integer(kind=8), intent(in)            :: n_indices, n_rows, n_cols, n_thds
    integer(kind=8)            :: i, idx
    real(kind=8)            :: y_val


    sum_vals=0
    cnt_vals=0
    call omp_set_num_threads(n_thds)
    !$omp parallel
    !$omp do reduction(+:sum_vals) reduction(+:cnt_vals) private(idx, i, y_val)
    do i=1, n_indices, 1
        idx = indices(i)
        y_val = y(idx)
        call count_and_sum_up_gt_vector2vector_04_F_r8(mat_t(1:n_cols,idx), thr_vals, sum_vals, cnt_vals, y_val, n_cols)
    end do
    !$omp end do
    !$omp end parallel
end subroutine get_matrix_count_and_sum_up_gt_04_F_parallel_r8

subroutine get_matrix_count_and_sum_up_gt_08_F_parallel_r8(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
    n_indices, n_rows, n_cols, n_thds)
    implicit none
    real(kind=8), intent(inout), target    :: sum_vals(n_cols)
    integer(kind=8), intent(inout), target :: cnt_vals(n_cols)
    real(kind=8), intent(inout), target    :: thr_vals(n_cols)
    real(kind=8), intent(in)   , target    :: mat_t(n_cols, n_rows), y(n_rows)
    integer(kind=8), intent(in), target    :: indices(n_indices)
    integer(kind=8), intent(in)            :: n_indices, n_rows, n_cols, n_thds
    integer(kind=8)            :: i, idx
    real(kind=8)            :: y_val


    sum_vals=0
    cnt_vals=0
    call omp_set_num_threads(n_thds)
    !$omp parallel
    !$omp do reduction(+:sum_vals) reduction(+:cnt_vals) private(idx, i, y_val)
    do i=1, n_indices, 1
        idx = indices(i)
        y_val = y(idx)
        call count_and_sum_up_gt_vector2vector_08_F_r8(mat_t(1:n_cols,idx), thr_vals, sum_vals, cnt_vals, y_val, n_cols)
    end do
    !$omp end do
    !$omp end parallel
end subroutine get_matrix_count_and_sum_up_gt_08_F_parallel_r8

subroutine get_matrix_count_and_sum_up_gt_16_F_parallel_r8(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
    n_indices, n_rows, n_cols, n_thds)
    implicit none
    real(kind=8), intent(inout), target    :: sum_vals(n_cols)
    integer(kind=8), intent(inout), target :: cnt_vals(n_cols)
    real(kind=8), intent(inout), target    :: thr_vals(n_cols)
    real(kind=8), intent(in)   , target    :: mat_t(n_cols, n_rows), y(n_rows)
    integer(kind=8), intent(in), target    :: indices(n_indices)
    integer(kind=8), intent(in)            :: n_indices, n_rows, n_cols, n_thds
    integer(kind=8)            :: i, idx
    real(kind=8)            :: y_val


    sum_vals=0
    cnt_vals=0
    call omp_set_num_threads(n_thds)
    !$omp parallel
    !$omp do reduction(+:sum_vals) reduction(+:cnt_vals) private(idx, i, y_val)
    do i=1, n_indices, 1
        idx = indices(i)
        y_val = y(idx)
        call count_and_sum_up_gt_vector2vector_16_F_r8(mat_t(1:n_cols,idx), thr_vals, sum_vals, cnt_vals, y_val, n_cols)
    end do
    !$omp end do
    !$omp end parallel
end subroutine get_matrix_count_and_sum_up_gt_16_F_parallel_r8
