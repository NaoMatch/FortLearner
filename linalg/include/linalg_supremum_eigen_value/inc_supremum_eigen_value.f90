function supremum_eigen_value_real64(matrix, n_dim)
    implicit none
    real(kind=8), intent(in) :: matrix(n_dim, n_dim)
    integer(kind=8), intent(in) :: n_dim
    real(kind=8) :: supremum_eigen_value_real64
    real(kind=8) :: tmp
    include "./include/linalg_supremum_eigen_value/inc_supremum_eigen_value_detail.f90"
    supremum_eigen_value_real64 = tmp
end function supremum_eigen_value_real64

function supremum_eigen_value_int32(matrix, n_dim)
    implicit none
    integer(kind=4), intent(in) :: matrix(n_dim, n_dim)
    integer(kind=4), intent(in) :: n_dim
    integer(kind=4) :: supremum_eigen_value_int32
    integer(kind=4) :: tmp
    include "./include/linalg_supremum_eigen_value/inc_supremum_eigen_value_detail.f90"
    supremum_eigen_value_int32 = tmp
end function supremum_eigen_value_int32

function supremum_eigen_value_int64(matrix, n_dim)
    implicit none
    integer(kind=8), intent(in) :: matrix(n_dim, n_dim)
    integer(kind=8), intent(in) :: n_dim
    integer(kind=8) :: supremum_eigen_value_int64
    integer(kind=8) :: tmp
    include "./include/linalg_supremum_eigen_value/inc_supremum_eigen_value_detail.f90"
    supremum_eigen_value_int64 = tmp
end function supremum_eigen_value_int64
