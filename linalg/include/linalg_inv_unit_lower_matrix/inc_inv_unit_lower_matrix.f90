subroutine inv_unit_lower_matrix_r8(matrix_inv, matrix_unit_lower, n_dim)
    implicit none
    real(kind=8), intent(inout) :: matrix_inv(n_dim, n_dim)
    real(kind=8), intent(inout) :: matrix_unit_lower(n_dim, n_dim)
    integer(kind=8), intent(in) :: n_dim

    integer(kind=8) :: i, j

    include "./include/linalg_inv_unit_lower_matrix/inc_inv_unit_lower_matrix_detail.f90"
end subroutine inv_unit_lower_matrix_r8

subroutine inv_unit_lower_matrix_i4(matrix_inv, matrix_unit_lower, n_dim)
    implicit none
    real(kind=4), intent(inout) :: matrix_inv(n_dim, n_dim)
    integer(kind=4), intent(inout) :: matrix_unit_lower(n_dim, n_dim)
    integer(kind=4), intent(in) :: n_dim

    integer(kind=4) :: i, j

    include "./include/linalg_inv_unit_lower_matrix/inc_inv_unit_lower_matrix_detail.f90"
end subroutine inv_unit_lower_matrix_i4

subroutine inv_unit_lower_matrix_i8(matrix_inv, matrix_unit_lower, n_dim)
    implicit none
    real(kind=8), intent(inout) :: matrix_inv(n_dim, n_dim)
    integer(kind=8), intent(inout) :: matrix_unit_lower(n_dim, n_dim)
    integer(kind=8), intent(in) :: n_dim

    integer(kind=8) :: i, j

    include "./include/linalg_inv_unit_lower_matrix/inc_inv_unit_lower_matrix_detail.f90"
end subroutine inv_unit_lower_matrix_i8
