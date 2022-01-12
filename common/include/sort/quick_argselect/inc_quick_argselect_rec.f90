recursive subroutine quick_argselect_rec_r8_i8(vector1, vector2, n_samples, n_th)
    implicit none
    real(kind=8), intent(inout) :: vector1(n_samples)
    integer(kind=8), intent(inout) :: vector2(n_samples)
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8), intent(in) :: n_th

    real(kind=8) :: pivot, tmp1
    integer(kind=8) :: tmp2
    integer(kind=8) :: i, j, idx
    include "./include/sort/quick_argselect/inc_quick_argselect_rec_detail.f90"
end subroutine quick_argselect_rec_r8_i8

recursive subroutine quick_argselect_rec_i8_r8(vector1, vector2, n_samples, n_th)
    implicit none
    integer(kind=8), intent(inout) :: vector1(n_samples)
    real(kind=8), intent(inout) :: vector2(n_samples)
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8), intent(in) :: n_th

    integer(kind=8) :: pivot, tmp1
    real(kind=8) :: tmp2
    integer(kind=8) :: i, j, idx
    include "./include/sort/quick_argselect/inc_quick_argselect_rec_detail.f90"
end subroutine quick_argselect_rec_i8_r8

recursive subroutine quick_argselect_rec_i8_i8(vector1, vector2, n_samples, n_th)
    implicit none
    integer(kind=8), intent(inout) :: vector1(n_samples)
    integer(kind=8), intent(inout) :: vector2(n_samples)
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8), intent(in) :: n_th

    integer(kind=8) :: pivot, tmp1
    integer(kind=8) :: tmp2
    integer(kind=8) :: i, j, idx
    include "./include/sort/quick_argselect/inc_quick_argselect_rec_detail.f90"
end subroutine quick_argselect_rec_i8_i8

