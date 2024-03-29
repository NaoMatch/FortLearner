function is_sorted_r8(vector, n_samples, ascending)
    implicit none
    real(kind=8), intent(in)    :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    logical(kind=8), optional   :: ascending
    logical(kind=8)             :: is_sorted_r8

    logical(kind=8) :: ascending_opt, tmp_result
    integer(kind=8) :: i

    include "./include/common/is_sorted/inc_is_sorted_detail.f90"
    is_sorted_r8 = tmp_result
end function is_sorted_r8

function is_sorted_i4(vector, n_samples, ascending)
    implicit none
    integer(kind=4), intent(in) :: vector(n_samples)
    integer(kind=4), intent(in) :: n_samples
    logical(kind=4), optional   :: ascending
    logical(kind=4)             :: is_sorted_i4

    logical(kind=4) :: ascending_opt, tmp_result
    integer(kind=4) :: i

    include "./include/common/is_sorted/inc_is_sorted_detail.f90"
    is_sorted_i4 = tmp_result
end function is_sorted_i4

function is_sorted_i8(vector, n_samples, ascending)
    implicit none
    integer(kind=8), intent(in) :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    logical(kind=8), optional   :: ascending
    logical(kind=8)             :: is_sorted_i8

    logical(kind=8) :: ascending_opt, tmp_result
    integer(kind=8) :: i

    include "./include/common/is_sorted/inc_is_sorted_detail.f90"
    is_sorted_i8 = tmp_result
end function is_sorted_i8
