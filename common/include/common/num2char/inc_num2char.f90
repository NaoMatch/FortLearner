function num2char_r8(num)
    character(:), allocatable   :: num2char_r8
    real(kind=8), intent(in) :: num
    integer(kind=8) :: num_digit

    allocate(character(10)::num2char_r8)
    write (num2char_r8, '(E10.3e2)') num
end function num2char_r8


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
