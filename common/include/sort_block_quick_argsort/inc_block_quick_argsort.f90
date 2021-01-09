recursive subroutine block_quick_argsort_r8_r8(vector1, vector2, num)
    real(kind=8), intent(inout) :: vector1(num)
    real(kind=8), intent(inout)   :: vector2(num)
    integer(kind=8), intent(in)    :: num
    integer(kind=8) :: offset_l(15), offset_r(15)
    integer(kind=8) :: start_l, start_r, num_l, num_r, r, l, i, j, num_m
    integer(kind=8) :: idx_l, idx_r, max_idx, next_l, next_r, one
    real(kind=8)    :: pivot, tmp1
    real(kind=8) :: tmp2
    integer(kind=8) :: three, dev
    real(kind=8)    :: pivots(3)
    include "./include/sort_block_quick_argsort/inc_block_quick_argsort_detail.f90"
    if (next_l .gt. 1)     call block_quick_argsort_r8_r8(vector1(1:next_l),vector2(1:next_l),      next_l)
    if (num-next_l .gt. 1) call block_quick_argsort_r8_r8(vector1(next_l+1:num), vector2(next_l+1:num), num-next_l)
end subroutine block_quick_argsort_r8_r8
