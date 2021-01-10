    subroutine random_rotation_r8(rotation_matrix, n_dim)
        implicit none
        real(kind=8), intent(inout) :: rotation_matrix(n_dim, n_dim)
        integer(kind=8), intent(in) :: n_dim

        real(kind=8) :: x(n_dim), y(n_dim) ! y = rotation_matrix * x
        real(kind=8) :: xx(n_dim) ! y = rotation_matrix * x
        real(kind=8) :: c_theta, s_theta, x_norm, y_norm
        integer(kind=8) :: loc(2), i
        real(kind=8) :: rotation_matrix_y(n_dim, n_dim), y_pre, y_c, y_s
        real(kind=8) :: one, two
        one = 1d0
        two = 2d0
        include "./include/linalg_rotation_matrix/inc_rotation_matrix_detail.f90"
    end subroutine random_rotation_r8
