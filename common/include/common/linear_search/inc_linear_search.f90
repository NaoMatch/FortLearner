function linear_search_r8(vector, n_samples, value)
    implicit none
    integer(kind=8)             :: linear_search_r8
    real(kind=8), intent(in)    :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    real(kind=8), intent(in)    :: value
    integer(kind=8) :: i, tmp
    include "./include/common/linear_search/inc_linear_search_detail.f90"
    linear_search_r8 = tmp
end function linear_search_r8

function linear_search_i4(vector, n_samples, value)
    implicit none
    integer(kind=4)             :: linear_search_i4
    integer(kind=4), intent(in) :: vector(n_samples)
    integer(kind=4), intent(in) :: n_samples
    integer(kind=4), intent(in) :: value
    integer(kind=4) :: i, tmp
    include "./include/common/linear_search/inc_linear_search_detail.f90"
    linear_search_i4 = tmp
end function linear_search_i4

function linear_search_i8(vector, n_samples, value)
    implicit none
    integer(kind=8)             :: linear_search_i8
    integer(kind=8), intent(in) :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8), intent(in) :: value
    integer(kind=8) :: i, tmp
    include "./include/common/linear_search/inc_linear_search_detail.f90"
    linear_search_i8 = tmp
end function linear_search_i8
