elemental function log2_r8(x)
    implicit none
    real(kind=8), intent(in) :: x
    real(kind=8) :: log2_r8
    log2_r8 = log(x)/log(2d0)
end function log2_r8

elemental function log2_i4(x)
    implicit none
    integer(kind=4), intent(in) :: x
    real(kind=4) :: log2_i4
    log2_i4 = log(x+0.0)/log(2.0)
end function log2_i4

elemental function log2_i8(x)
    implicit none
    integer(kind=8), intent(in) :: x
    real(kind=8) :: log2_i8
    log2_i8 = log(x+0d0)/log(2d0)
end function log2_i8
