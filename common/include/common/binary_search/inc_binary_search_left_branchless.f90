function binary_search_left_branchless_r8(vector, n_samples, value) result(idx)
    implicit none
    integer(kind=8)             :: idx
    real(kind=8), intent(in)    :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    real(kind=8), intent(in)    :: value

    integer(kind=8) :: lo, hi, mid, step, step_, flg
    include "./include/common/binary_search/inc_binary_search_left_branchless_detail.f90"
end function binary_search_left_branchless_r8

function binary_search_left_branchless_i4(vector, n_samples, value) result(idx)
    implicit none
    integer(kind=4)             :: idx
    integer(kind=4), intent(in) :: vector(n_samples)
    integer(kind=4), intent(in) :: n_samples
    integer(kind=4), intent(in) :: value

    integer(kind=4) :: lo, hi, mid, step, step_, flg
    include "./include/common/binary_search/inc_binary_search_left_branchless_detail.f90"
end function binary_search_left_branchless_i4

function binary_search_left_branchless_i8(vector, n_samples, value) result(idx)
    implicit none
    integer(kind=8)             :: idx
    integer(kind=8), intent(in) :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8), intent(in) :: value

    integer(kind=8) :: lo, hi, mid, step, step_, flg
    include "./include/common/binary_search/inc_binary_search_left_branchless_detail.f90"
end function binary_search_left_branchless_i8
