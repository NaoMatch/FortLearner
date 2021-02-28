subroutine rand_uniform_r4(val, min_val, max_val)
    implicit none
    real(kind=4), intent(inout) :: val
    real(kind=4), intent(in)    :: min_val, max_val
    call random_number(val)
    val = (max_val-min_val) * val + min_val
end subroutine rand_uniform_r4

subroutine rand_uniform_r8(val, min_val, max_val)
    implicit none
    real(kind=8), intent(inout) :: val
    real(kind=8), intent(in)    :: min_val, max_val
    call random_number(val)
    val = (max_val-min_val) * val + min_val
end subroutine rand_uniform_r8

subroutine rand_uniform_1d_r8(vector, min_val, max_val, num)
    implicit none
    real(kind=8), intent(inout) :: vector(num)
    real(kind=8), intent(in)    :: min_val, max_val
    integer(kind=8), intent(in) :: num
    call random_number(vector)
    vector = (max_val-min_val) * vector + min_val
end subroutine rand_uniform_1d_r8
