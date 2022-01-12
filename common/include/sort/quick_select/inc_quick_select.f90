subroutine quick_select_r8(vector, n_samples, n_th, left_align)
    implicit none
    real(kind=8), intent(inout)  :: vector(n_samples)
    integer(kind=8), intent(in)  :: n_samples
    integer(kind=8), intent(in)  :: n_th
    logical(kind=4), intent(in)  :: left_align
    logical(kind=4), allocatable :: is_same(:)
    include "./include/sort/quick_select/inc_quick_select_detail.f90"
end subroutine quick_select_r8
