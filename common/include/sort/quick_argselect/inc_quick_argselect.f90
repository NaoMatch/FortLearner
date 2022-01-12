recursive subroutine quick_argselect_r8_i8(vector1, vector2, n_samples, n_th, left_align)
    implicit none
    real(kind=8), intent(inout) :: vector1(n_samples)
    integer(kind=8), intent(inout) :: vector2(n_samples)
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8), intent(in) :: n_th
    logical(kind=4), intent(in) :: left_align

    real(kind=8) :: pivot, tmp1
    integer(kind=8) :: tmp2
    integer(kind=8) :: i, j, idx
    logical(kind=4), allocatable :: is_same(:)
    include "./include/sort/quick_argselect/inc_quick_argselect_detail.f90"
end subroutine quick_argselect_r8_i8

recursive subroutine quick_argselect_i8_r8(vector1, vector2, n_samples, n_th, left_align)
    implicit none
    integer(kind=8), intent(inout) :: vector1(n_samples)
    real(kind=8), intent(inout) :: vector2(n_samples)
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8), intent(in) :: n_th
    logical(kind=4), intent(in) :: left_align

    integer(kind=8) :: pivot, tmp1
    real(kind=8) :: tmp2
    integer(kind=8) :: i, j, idx
    logical(kind=4), allocatable :: is_same(:)
    include "./include/sort/quick_argselect/inc_quick_argselect_detail.f90"
end subroutine quick_argselect_i8_r8

recursive subroutine quick_argselect_i8_i8(vector1, vector2, n_samples, n_th, left_align)
    implicit none
    integer(kind=8), intent(inout) :: vector1(n_samples)
    integer(kind=8), intent(inout) :: vector2(n_samples)
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8), intent(in) :: n_th
    logical(kind=4), intent(in) :: left_align

    integer(kind=8) :: pivot, tmp1
    integer(kind=8) :: tmp2
    integer(kind=8) :: i, j, idx
    logical(kind=4), allocatable :: is_same(:)
    include "./include/sort/quick_argselect/inc_quick_argselect_detail.f90"
end subroutine quick_argselect_i8_i8

