subroutine rand_normal_1d_r8(array, num)
    implicit none
    real(kind=8), intent(inout) :: array(num)
    integer(kind=8), intent(in) :: num
    real(kind=8), allocatable :: tmp(:)
    real(kind=8) :: two
    allocate(tmp(num))
    include "./include/random/rand_normal/inc_rand_normal_detail.f90"
end subroutine rand_normal_1d_r8

subroutine rand_normal_2d_r8(array, n_rows, n_cols)
    implicit none
    real(kind=8), intent(inout) :: array(n_rows, n_cols)
    integer(kind=8), intent(in) :: n_rows, n_cols
    real(kind=8), allocatable :: tmp(:,:)
    real(kind=8) :: two
    allocate(tmp(n_rows, n_cols))
    include "./include/random/rand_normal/inc_rand_normal_detail.f90"
end subroutine rand_normal_2d_r8

subroutine rand_normal_r8(array)
    implicit none
    real(kind=8), intent(inout) :: array
    real(kind=8) :: tmp
    real(kind=8) :: two
    include "./include/random/rand_normal/inc_rand_normal_detail.f90"
end subroutine rand_normal_r8
