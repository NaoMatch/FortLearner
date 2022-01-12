subroutine align_left_r8(vector, is_same, n_samples)
    implicit none
    real(kind=8), intent(inout) :: vector(n_samples)
    logical(kind=4), intent(inout) :: is_same(n_samples)
    integer(kind=8), intent(in) :: n_samples

    real(kind=8) :: pivot, tmp
    integer(kind=8) :: i, j, idx, n_th_new
    include "./include/sort/align_left/inc_align_left_detail.f90"
end subroutine align_left_r8
