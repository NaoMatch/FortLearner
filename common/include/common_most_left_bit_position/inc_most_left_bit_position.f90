function most_left_bit_position_int64(val)
    implicit none
    integer(kind=8), intent(in) :: val
    integer(kind=8) :: most_left_bit_position_int64
    integer(kind=8) :: i, i_left, tmp
    i_left = 62
    include "./include/common_most_left_bit_position/inc_most_left_bit_position_detail.f90"
    most_left_bit_position_int64 = tmp
end function most_left_bit_position_int64
