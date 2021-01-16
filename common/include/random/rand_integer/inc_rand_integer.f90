subroutine rand_integer_i8(lo, hi, vector, num)
    implicit none
    integer(kind=8), intent(in)    :: lo, hi
    integer(kind=8), intent(inout) :: vector(num)
    integer(kind=8), intent(in)    :: num
    
    real(kind=8), allocatable      :: tmp_array(:)
    integer(kind=8)                :: i, j, factor, min, tmp
    integer(kind=8)                :: unroll, buffer(15)

    include "./include/random/rand_integer/inc_rand_integer_detail.f90"
end subroutine rand_integer_i8