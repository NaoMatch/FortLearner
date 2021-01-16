function binary_search_right_r8(vector, n_samples, value)
    implicit none
    integer(kind=8) :: binary_search_right_r8
    real(kind=8), intent(in)    :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    real(kind=8), intent(in)    :: value

    integer(kind=8) :: lo, hi, mid
    include "./include/common/binary_search/inc_binary_search_right_detail.f90"
    binary_search_right_r8 = lo
end function binary_search_right_r8

function binary_search_right_i4(vector, n_samples, value)
    implicit none
    integer(kind=4)             :: binary_search_right_i4
    integer(kind=4), intent(in) :: vector(n_samples)
    integer(kind=4), intent(in) :: n_samples
    integer(kind=4), intent(in) :: value

    integer(kind=4) :: lo, hi, mid
    include "./include/common/binary_search/inc_binary_search_right_detail.f90"
    binary_search_right_i4 = lo
end function binary_search_right_i4

function binary_search_right_i8(vector, n_samples, value)
    implicit none
    integer(kind=8)            :: binary_search_right_i8
    integer(kind=8), intent(in) :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8), intent(in) :: value

    integer(kind=8) :: lo, hi, mid
    include "./include/common/binary_search/inc_binary_search_right_detail.f90"
    binary_search_right_i8 = lo
end function binary_search_right_i8
