function supremum_eigen_value_r8(matrix, n_dim)
    implicit none
    real(kind=8), intent(in) :: matrix(n_dim, n_dim)
    integer(kind=8), intent(in) :: n_dim
    real(kind=8) :: supremum_eigen_value_r8
    real(kind=8) :: tmp
    include "./include/linalg_supremum_eigen_value/inc_supremum_eigen_value_detail.f90"
    supremum_eigen_value_r8 = tmp
end function supremum_eigen_value_r8

function supremum_eigen_value_i4(matrix, n_dim)
    implicit none
    integer(kind=4), intent(in) :: matrix(n_dim, n_dim)
    integer(kind=4), intent(in) :: n_dim
    integer(kind=4) :: supremum_eigen_value_i4
    integer(kind=4) :: tmp
    include "./include/linalg_supremum_eigen_value/inc_supremum_eigen_value_detail.f90"
    supremum_eigen_value_i4 = tmp
end function supremum_eigen_value_i4

function supremum_eigen_value_i8(matrix, n_dim)
    implicit none
    integer(kind=8), intent(in) :: matrix(n_dim, n_dim)
    integer(kind=8), intent(in) :: n_dim
    integer(kind=8) :: supremum_eigen_value_i8
    integer(kind=8) :: tmp
    include "./include/linalg_supremum_eigen_value/inc_supremum_eigen_value_detail.f90"
    supremum_eigen_value_i8 = tmp
end function supremum_eigen_value_i8
