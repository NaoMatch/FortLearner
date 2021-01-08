subroutine permutation_real64(vector, num)
    implicit none
    real(kind=8), intent(inout) :: vector(num)
    integer(kind=8), intent(in) :: num
    integer(kind=8)             :: tmp, i, j
    integer(kind=8)             :: randpos
    real(kind=8)                :: r, rand_vals(num)
    include "./include/random_permutation/inc_permutation_detail.f90"
end subroutine permutation_real64

subroutine permutation_int32(vector, num)
    implicit none
    integer(kind=4), intent(inout) :: vector(num)
    integer(kind=4), intent(in) :: num
    integer(kind=4)             :: tmp, i, j
    integer(kind=8)             :: randpos
    real(kind=4)                :: r, rand_vals(num)
    include "./include/random_permutation/inc_permutation_detail.f90"
end subroutine permutation_int32

subroutine permutation_int64(vector, num)
    implicit none
    integer(kind=8), intent(inout) :: vector(num)
    integer(kind=8), intent(in) :: num
    integer(kind=8)             :: tmp, i, j
    integer(kind=8)             :: randpos
    real(kind=8)                :: r, rand_vals(num)
    include "./include/random_permutation/inc_permutation_detail.f90"
end subroutine permutation_int64
