pure function get_int_digit_i8(num) result(num_digit)
    implicit none
    integer(kind=8),intent(in) :: num
    integer(kind=8) :: num_digit
    if (num .lt. 0_8) then
        num_digit = int(log10(dble(abs(num)))) + 1
        num_digit = num_digit + 1
    else
        num_digit = int(log10(dble(num))) + 1
    end if
end function get_int_digit_i8
