subroutine get_abs_maxloc_symmat_r8(loc, val, matrix, n_dim)
    implicit none
    real(kind=8), intent(in)         :: matrix(n_dim, n_dim)
    integer(kind=8), intent(in)    :: n_dim
    integer(kind=8), intent(inout) :: loc(2)
    real(kind=8), intent(inout)      :: val
    integer(kind=8)                :: i, j
    real(kind=8)                     :: tmp_absmax, tmp_absval
    include "./include/common/get_abs_maxloc/inc_get_abs_maxloc_symmat_detail.f90"
end subroutine get_abs_maxloc_symmat_r8

subroutine get_abs_maxloc_symmat_i4(loc, val, matrix, n_dim)
    implicit none
    integer(kind=4), intent(in)         :: matrix(n_dim, n_dim)
    integer(kind=4), intent(in)    :: n_dim
    integer(kind=4), intent(inout) :: loc(2)
    integer(kind=4), intent(inout)      :: val
    integer(kind=4)                :: i, j
    integer(kind=4)                     :: tmp_absmax, tmp_absval
    include "./include/common/get_abs_maxloc/inc_get_abs_maxloc_symmat_detail.f90"
end subroutine get_abs_maxloc_symmat_i4

subroutine get_abs_maxloc_symmat_i8(loc, val, matrix, n_dim)
    implicit none
    integer(kind=8), intent(in)         :: matrix(n_dim, n_dim)
    integer(kind=8), intent(in)    :: n_dim
    integer(kind=8), intent(inout) :: loc(2)
    integer(kind=8), intent(inout)      :: val
    integer(kind=8)                :: i, j
    integer(kind=8)                     :: tmp_absmax, tmp_absval
    include "./include/common/get_abs_maxloc/inc_get_abs_maxloc_symmat_detail.f90"
end subroutine get_abs_maxloc_symmat_i8
