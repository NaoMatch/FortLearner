elemental function log2_real64(x)
    implicit none
    real(kind=8), intent(in) :: x
    real(kind=8) :: log2_real64
    log2_real64 = log(x)/log(2d0)
end function log2_real64

elemental function log2_int32(x)
    implicit none
    integer(kind=4), intent(in) :: x
    real(kind=4) :: log2_int32
    log2_int32 = log(x+0.0)/log(2.0)
end function log2_int32

elemental function log2_int64(x)
    implicit none
    integer(kind=8), intent(in) :: x
    real(kind=8) :: log2_int64
    log2_int64 = log(x+0d0)/log(2d0)
end function log2_int64
