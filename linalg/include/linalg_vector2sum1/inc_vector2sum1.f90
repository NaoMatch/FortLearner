subroutine vector2sum1_r8(vector, n_samples)
    implicit none
    real(kind=8), intent(inout) :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    real(kind=8)                :: sum_vec
    integer(kind=8)             :: n
    include "./include/linalg_vector2sum1/inc_vector2sum1_detail.f90"
end subroutine vector2sum1_r8
