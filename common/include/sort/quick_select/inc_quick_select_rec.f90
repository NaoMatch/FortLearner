recursive subroutine quick_select_rec_i8(vector, n_samples, n_th)
    implicit none
    integer(kind=8), intent(inout) :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8), intent(in) :: n_th

    integer(kind=8) :: pivot, tmp
    integer(kind=8) :: i, j, idx, n_th_new
    include "./include/sort/quick_select/inc_quick_select_rec_detail.f90"
end subroutine quick_select_rec_i8
