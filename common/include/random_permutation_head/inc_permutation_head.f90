subroutine permutation_head_real64(vector, num, n_head)
    implicit none
    real(kind=8), intent(inout) :: vector(num)
    integer(kind=8), intent(in) :: num
    integer(kind=8), intent(in) :: n_head
    integer(kind=8)             :: tmp, i, j
    integer(kind=8)             :: randpos
    real(kind=4)                :: r, rand_vals(n_head)
    include "./include/random_permutation_head/inc_permutation_head_detail.f90"
end subroutine permutation_head_real64

subroutine permutation_head_int32(vector, num, n_head)
    implicit none
    integer(kind=4), intent(inout) :: vector(num)
    integer(kind=4), intent(in) :: num
    integer(kind=4), intent(in) :: n_head
    integer(kind=4)             :: tmp, i, j
    integer(kind=8)             :: randpos
    real(kind=4)                :: r, rand_vals(n_head)
    include "./include/random_permutation_head/inc_permutation_head_detail.f90"
end subroutine permutation_head_int32

subroutine permutation_head_int64(vector, num, n_head)
    implicit none
    integer(kind=8), intent(inout) :: vector(num)
    integer(kind=8), intent(in) :: num
    integer(kind=8), intent(in) :: n_head
    integer(kind=8)             :: tmp, i, j
    integer(kind=8)             :: randpos
    real(kind=8)                :: r, rand_vals(n_head)
    include "./include/random_permutation_head/inc_permutation_head_detail.f90"
end subroutine permutation_head_int64
