recursive subroutine block_quick_sort_r8(vector, num)
    real(kind=8), intent(inout) :: vector(num)
    integer(kind=8), intent(in)    :: num
    integer(kind=8) :: offset_l(31), offset_r(31)
    integer(kind=8) :: start_l, start_r, num_l, num_r, r, l, i, j, num_m
    integer(kind=8) :: idx_l, idx_r, max_idx, next_l, next_r, one
    real(kind=8) :: pivot, tmp
    integer(kind=8)             :: three, dev
    real(kind=8)                :: dev_rand
    real(kind=8)                  :: pivots(3)
    include "./include/sort_block_quick_sort/inc_block_quick_sort_detail.f90"
    if (next_l .gt. 1)     call block_quick_sort_r8(vector(1:next_l),     next_l)
    if (num-next_l .gt. 1) call block_quick_sort_r8(vector(next_l+1:num), num-next_l)
end subroutine block_quick_sort_r8

recursive subroutine block_quick_sort_i4(vector, num)
    integer(kind=4), intent(inout) :: vector(num)
    integer(kind=4), intent(in)    :: num
    integer(kind=4) :: offset_l(31), offset_r(31)
    integer(kind=4) :: start_l, start_r, num_l, num_r, r, l, i, j, num_m
    integer(kind=4) :: idx_l, idx_r, max_idx, next_l, next_r, one
    integer(kind=4) :: pivot, tmp
    integer(kind=4) :: three, dev
    integer(kind=4) :: pivots(3)
    include "./include/sort_block_quick_sort/inc_block_quick_sort_detail.f90"
    if (next_l .gt. 1)     call block_quick_sort_i4(vector(1:next_l),     next_l)
    if (num-next_l .gt. 1) call block_quick_sort_i4(vector(next_l+1:num), num-next_l)
end subroutine block_quick_sort_i4

recursive subroutine block_quick_sort_i8(vector, num)
    integer(kind=8), intent(inout) :: vector(num)
    integer(kind=8), intent(in)    :: num
    integer(kind=8) :: offset_l(31), offset_r(31)
    integer(kind=8) :: start_l, start_r, num_l, num_r, r, l, i, j, num_m
    integer(kind=8) :: idx_l, idx_r, max_idx, next_l, next_r, one
    integer(kind=8) :: pivot, tmp
    integer(kind=8) :: three, dev
    integer(kind=8) :: pivots(3)
    include "./include/sort_block_quick_sort/inc_block_quick_sort_detail.f90"
    if (next_l .gt. 1)     call block_quick_sort_i8(vector(1:next_l),     next_l)
    if (num-next_l .gt. 1) call block_quick_sort_i8(vector(next_l+1:num), num-next_l)
end subroutine block_quick_sort_i8
