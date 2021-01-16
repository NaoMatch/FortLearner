subroutine rotate_from_both_side_r8(mat, n_dim, loc, c_theta, s_theta)
    implicit none
    real(kind=8), intent(inout) :: mat(n_dim, n_dim)
    integer(kind=8), intent(in) :: n_dim
    integer(kind=8), intent(in) :: loc(2)
    real(kind=8), intent(in)    :: c_theta, s_theta

    integer(kind=8) :: i, p, q, posi
    real(kind=8)    :: mat_p, mat_q
    include "./include/linalg_rotate_from_both_side/inc_rotate_from_both_side_detail.f90"
end subroutine rotate_from_both_side_r8
