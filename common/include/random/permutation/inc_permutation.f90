subroutine permutation_r8(vector, num)
    implicit none
    real(kind=8), intent(inout) :: vector(num)
    integer(kind=8), intent(in) :: num
    integer(kind=8)             :: tmp, i, j
    integer(kind=8)             :: randpos, one
    real(kind=8)                :: r, rand_vals(num)
    include "./include/random/permutation/inc_permutation_detail.f90"
end subroutine permutation_r8

subroutine permutation_i4(vector, num)
    implicit none
    integer(kind=4), intent(inout) :: vector(num)
    integer(kind=4), intent(in) :: num
    integer(kind=4)             :: tmp, i, j
    integer(kind=8)             :: randpos, one
    real(kind=4)                :: r, rand_vals(num)
    include "./include/random/permutation/inc_permutation_detail.f90"
end subroutine permutation_i4

subroutine permutation_i8(vector, num)
    implicit none
    integer(kind=8), intent(inout) :: vector(num)
    integer(kind=8), intent(in) :: num
    integer(kind=8)             :: tmp, i, j
    integer(kind=8)             :: randpos, one
    real(kind=8)                :: r, rand_vals(num)
    include "./include/random/permutation/inc_permutation_detail.f90"
end subroutine permutation_i8
