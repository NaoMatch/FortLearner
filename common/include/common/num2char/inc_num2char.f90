function num2char_i4(num)
    character(:), allocatable   :: num2char_i4
    integer(kind=4), intent(in) :: num
    integer(kind=4) :: num_digit

    num_digit = get_int_digit(num)
    allocate(character(num_digit)::num2char_i4)
    write (num2char_i4, '(i0)') num
end function num2char_i4


function num2char_i8(num)
    character(:), allocatable   :: num2char_i8
    integer(kind=8), intent(in) :: num
    integer(kind=8) :: num_digit

    num_digit = get_int_digit(num)
    allocate(character(num_digit)::num2char_i8)
    write (num2char_i8, '(i0)') num
end function num2char_i8

function num2char_r8(num) result(char)
    implicit none
    real(kind=8), intent(in) :: num
    character(:), allocatable :: char
    character(20) :: tmp
    character(:), allocatable :: char_digit
    integer(kind=8) :: digit
    character(1) :: sign

    digit = get_real_digit(abs(num))
    char_digit = num2char(digit)

    write (tmp, '(f20.18)') abs(num) * 10d0**(-digit)
    sign = " "
    if (num<0d0) sign = "-"
    char = sign // tmp // "d" // char_digit
end function num2char_r8
