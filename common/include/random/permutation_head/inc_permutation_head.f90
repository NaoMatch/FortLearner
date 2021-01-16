subroutine permutation_head_r8(vector, num, n_head)
    implicit none
    real(kind=8), intent(inout) :: vector(num)
    integer(kind=8), intent(in) :: num
    integer(kind=8), intent(in) :: n_head
    integer(kind=8)             :: tmp, i, j
    integer(kind=8)             :: randpos, one
    real(kind=4)                :: r, rand_vals(n_head)
    include "./include/random/permutation_head/inc_permutation_head_detail.f90"
end subroutine permutation_head_r8

subroutine permutation_head_i4(vector, num, n_head)
    implicit none
    integer(kind=4), intent(inout) :: vector(num)
    integer(kind=4), intent(in) :: num
    integer(kind=4), intent(in) :: n_head
    integer(kind=4)             :: tmp, i, j
    integer(kind=4)             :: randpos, one
    real(kind=4)                :: r, rand_vals(n_head)
    include "./include/random/permutation_head/inc_permutation_head_detail.f90"
end subroutine permutation_head_i4

subroutine permutation_head_i8(vector, num, n_head)
    implicit none
    integer(kind=8), intent(inout) :: vector(num)
    integer(kind=8), intent(in) :: num
    integer(kind=8), intent(in) :: n_head
    integer(kind=8)             :: tmp, i, j
    integer(kind=8)             :: randpos, one
    real(kind=8)                :: r, rand_vals(n_head)
    include "./include/random/permutation_head/inc_permutation_head_detail.f90"
end subroutine permutation_head_i8
