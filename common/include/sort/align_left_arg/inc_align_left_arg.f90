subroutine align_left_arg_r8_i8(vector1, vector2, is_same, n_samples)
    implicit none
    real(kind=8), intent(inout) :: vector1(n_samples)
    integer(kind=8), intent(inout) :: vector2(n_samples)
    logical(kind=4), intent(inout) :: is_same(n_samples)
    integer(kind=8), intent(in) :: n_samples

    real(kind=8) :: pivot, tmp1
    integer(kind=8) :: tmp2
    integer(kind=8) :: i, j, idx, n_th_new
    include "./include/sort/align_left_arg/inc_align_left_arg_detail.f90"
end subroutine align_left_arg_r8_i8

subroutine align_left_arg_i8_i8(vector1, vector2, is_same, n_samples)
    implicit none
    integer(kind=8), intent(inout) :: vector1(n_samples)
    integer(kind=8), intent(inout) :: vector2(n_samples)
    logical(kind=4), intent(inout) :: is_same(n_samples)
    integer(kind=8), intent(in) :: n_samples

    integer(kind=8) :: pivot, tmp1
    integer(kind=8) :: tmp2
    integer(kind=8) :: i, j, idx, n_th_new
    include "./include/sort/align_left_arg/inc_align_left_arg_detail.f90"
end subroutine align_left_arg_i8_i8

subroutine align_left_arg_i8_r8(vector1, vector2, is_same, n_samples)
    implicit none
    integer(kind=8), intent(inout) :: vector1(n_samples)
    real(kind=8), intent(inout) :: vector2(n_samples)
    logical(kind=4), intent(inout) :: is_same(n_samples)
    integer(kind=8), intent(in) :: n_samples

    integer(kind=8) :: pivot, tmp1
    real(kind=8) :: tmp2
    integer(kind=8) :: i, j, idx, n_th_new
    include "./include/sort/align_left_arg/inc_align_left_arg_detail.f90"
end subroutine align_left_arg_i8_r8
