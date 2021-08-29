subroutine get_matrix_minmax_with_index_parallel_01_F_r8( & 
            min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols, n_thds)
        real(kind=8), intent(inout), target :: min_vals(n_cols), max_vals(n_cols)
        real(kind=8), intent(in)   , target :: mat_t(n_cols, n_rows)
        integer(kind=8), intent(in), target :: indices(n_indices)
        integer(kind=8), intent(in)         :: n_indices, n_rows, n_cols, n_thds
        integer(kind=8) :: idx, i

        call omp_set_num_threads(n_thds)
        !$omp parallel
        !$omp do reduction(min:min_vals) reduction(max:max_vals) private(idx, i)
        do i=1, n_indices, 1
            idx = indices(i)
            call get_minmax_vector2vector_01_F_r8(mat_t(:,idx), min_vals, max_vals, n_cols)
        end do
        !$omp end do
        !$omp end parallel
end subroutine get_matrix_minmax_with_index_parallel_01_F_r8

subroutine get_matrix_minmax_with_index_parallel_02_F_r8( & 
            min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols, n_thds)
        real(kind=8), intent(inout), target :: min_vals(n_cols), max_vals(n_cols)
        real(kind=8), intent(in)   , target :: mat_t(n_cols, n_rows)
        integer(kind=8), intent(in), target :: indices(n_indices)
        integer(kind=8), intent(in)         :: n_indices, n_rows, n_cols, n_thds
        integer(kind=8) :: idx, i

        call omp_set_num_threads(n_thds)
        !$omp parallel
        !$omp do reduction(min:min_vals) reduction(max:max_vals) private(idx, i)
        do i=1, n_indices, 1
            idx = indices(i)
            call get_minmax_vector2vector_02_F_r8(mat_t(:,idx), min_vals, max_vals, n_cols)
        end do
        !$omp end do
        !$omp end parallel
end subroutine get_matrix_minmax_with_index_parallel_02_F_r8

subroutine get_matrix_minmax_with_index_parallel_04_F_r8( & 
            min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols, n_thds)
        real(kind=8), intent(inout), target :: min_vals(n_cols), max_vals(n_cols)
        real(kind=8), intent(in)   , target :: mat_t(n_cols, n_rows)
        integer(kind=8), intent(in), target :: indices(n_indices)
        integer(kind=8), intent(in)         :: n_indices, n_rows, n_cols, n_thds
        integer(kind=8) :: idx, i

        call omp_set_num_threads(n_thds)
        !$omp parallel
        !$omp do reduction(min:min_vals) reduction(max:max_vals) private(idx, i)
        do i=1, n_indices, 1
            idx = indices(i)
            call get_minmax_vector2vector_04_F_r8(mat_t(:,idx), min_vals, max_vals, n_cols)
        end do
        !$omp end do
        !$omp end parallel
end subroutine get_matrix_minmax_with_index_parallel_04_F_r8

subroutine get_matrix_minmax_with_index_parallel_08_F_r8( & 
            min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols, n_thds)
        real(kind=8), intent(inout), target :: min_vals(n_cols), max_vals(n_cols)
        real(kind=8), intent(in)   , target :: mat_t(n_cols, n_rows)
        integer(kind=8), intent(in), target :: indices(n_indices)
        integer(kind=8), intent(in)         :: n_indices, n_rows, n_cols, n_thds
        integer(kind=8) :: idx, i

        call omp_set_num_threads(n_thds)
        !$omp parallel
        !$omp do reduction(min:min_vals) reduction(max:max_vals) private(idx, i)
        do i=1, n_indices, 1
            idx = indices(i)
            call get_minmax_vector2vector_08_F_r8(mat_t(:,idx), min_vals, max_vals, n_cols)
        end do
        !$omp end do
        !$omp end parallel
end subroutine get_matrix_minmax_with_index_parallel_08_F_r8

subroutine get_matrix_minmax_with_index_parallel_16_F_r8( & 
            min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols, n_thds)
        real(kind=8), intent(inout), target :: min_vals(n_cols), max_vals(n_cols)
        real(kind=8), intent(in)   , target :: mat_t(n_cols, n_rows)
        integer(kind=8), intent(in), target :: indices(n_indices)
        integer(kind=8), intent(in)         :: n_indices, n_rows, n_cols, n_thds
        integer(kind=8) :: idx, i

        call omp_set_num_threads(n_thds)
        !$omp parallel
        !$omp do reduction(min:min_vals) reduction(max:max_vals) private(idx, i)
        do i=1, n_indices, 1
            idx = indices(i)
            call get_minmax_vector2vector_16_F_r8(mat_t(:,idx), min_vals, max_vals, n_cols)
        end do
        !$omp end do
        !$omp end parallel
end subroutine get_matrix_minmax_with_index_parallel_16_F_r8

