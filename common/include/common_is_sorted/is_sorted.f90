function is_sorted_real64(vector, num, ascending)
    implicit none
    real(kind=8), intent(in)    :: vector(num)
    integer(kind=8), intent(in) :: num
    logical(kind=8), optional   :: ascending
    logical(kind=8)             :: is_sorted_real64

    logical(kind=8) :: ascending_opt, tmp_result
    integer(kind=8) :: i

    include "./include/common_is_sorted/is_sorted_detail.f90"
    is_sorted_real64 = tmp_result
end function is_sorted_real64

function is_sorted_int32(vector, num, ascending)
    implicit none
    integer(kind=4), intent(in)    :: vector(num)
    integer(kind=4), intent(in) :: num
    logical(kind=4), optional   :: ascending
    logical(kind=4)             :: is_sorted_int32

    logical(kind=4) :: ascending_opt, tmp_result
    integer(kind=4) :: i

    include "./include/common_is_sorted/is_sorted_detail.f90"
    is_sorted_int32 = tmp_result
end function is_sorted_int32

function is_sorted_int64(vector, num, ascending)
    implicit none
    integer(kind=8), intent(in)    :: vector(num)
    integer(kind=8), intent(in) :: num
    logical(kind=8), optional   :: ascending
    logical(kind=8)             :: is_sorted_int64

    logical(kind=8) :: ascending_opt, tmp_result
    integer(kind=8) :: i

    include "./include/common_is_sorted/is_sorted_detail.f90"
    is_sorted_int64 = tmp_result
end function is_sorted_int64
