subroutine rand_normal_real64_1d(vector, num)
    implicit none
    real(kind=8), intent(inout) :: vector(num)
    integer(kind=8), intent(in) :: num
    real(kind=8), allocatable :: tmp(:)
    real(kind=8) :: two
    allocate(tmp(num))
    two = 2d0
    include "./include/random_rand_normal/inc_rand_normal_detail.f90"
end subroutine rand_normal_real64_1d
