elemental function sigmoid_r8(x)
    real(kind=8), intent(in) :: x
    real(kind=8)             :: sigmoid_r8
    sigmoid_r8 = 1d0 / (1d0 + exp(-x))
end function sigmoid_r8
