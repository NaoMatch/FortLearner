pure function get_int_digit_i8(num) result(num_digit)
    implicit none
    integer(kind=8),intent(in) :: num
    integer(kind=8) :: num_digit
    include "./include/common/get_int_digit/inc_get_int_digit_detail.f90"
end function get_int_digit_i8
