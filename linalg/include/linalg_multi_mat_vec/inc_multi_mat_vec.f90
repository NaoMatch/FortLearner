subroutine multi_mat_vec_r8(matrix, input_vector, output_vector, n_rows, n_cols, parallel)
    implicit none
    real(kind=8), intent(in)      :: matrix(n_rows, n_cols)
    real(kind=8), intent(in)      :: input_vector(n_cols)
    real(kind=8), intent(inout)   :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols
    logical(kind=4), optional :: parallel

    integer(kind=8) :: i, j, k, n_cols_unroll
    real(kind=8)      :: tmp_out, tmp_input, buffer_input(cache_size_multi_mat_vec)
    logical(kind=4)   :: parallel_opt=f_
    output_vector = 0

    include "./include/linalg_multi_mat_vec/inc_multi_mat_vec_detail.f90"
end subroutine multi_mat_vec_r8

subroutine multi_mat_vec_i4(matrix, input_vector, output_vector, n_rows, n_cols, parallel)
    implicit none
    integer(kind=4), intent(in)      :: matrix(n_rows, n_cols)
    integer(kind=4), intent(in)      :: input_vector(n_cols)
    integer(kind=4), intent(inout)   :: output_vector(n_rows)
    integer(kind=4), intent(in) :: n_rows, n_cols
    logical(kind=4), optional :: parallel

    integer(kind=4) :: i, j, k, n_cols_unroll
    integer(kind=4)      :: tmp_out, tmp_input, buffer_input(cache_size_multi_mat_vec)
    logical(kind=4)   :: parallel_opt=f_
    output_vector = 0

    include "./include/linalg_multi_mat_vec/inc_multi_mat_vec_detail.f90"
end subroutine multi_mat_vec_i4

subroutine multi_mat_vec_i8(matrix, input_vector, output_vector, n_rows, n_cols, parallel)
    implicit none
    integer(kind=8), intent(in)      :: matrix(n_rows, n_cols)
    integer(kind=8), intent(in)      :: input_vector(n_cols)
    integer(kind=8), intent(inout)   :: output_vector(n_rows)
    integer(kind=8), intent(in) :: n_rows, n_cols
    logical(kind=4), optional :: parallel

    integer(kind=8) :: i, j, k, n_cols_unroll
    integer(kind=8)      :: tmp_out, tmp_input, buffer_input(cache_size_multi_mat_vec)
    logical(kind=4)   :: parallel_opt=f_
    output_vector = 0

    include "./include/linalg_multi_mat_vec/inc_multi_mat_vec_detail.f90"
end subroutine multi_mat_vec_i8
