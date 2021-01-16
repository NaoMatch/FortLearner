function most_left_bit_position_i8(val)
    implicit none
    integer(kind=8), intent(in) :: val
    integer(kind=8) :: most_left_bit_position_i8
    integer(kind=8) :: i, i_left, tmp
    i_left = 62
    include "./include/common/most_left_bit_position/inc_most_left_bit_position_detail.f90"
    most_left_bit_position_i8 = tmp
end function most_left_bit_position_i8
