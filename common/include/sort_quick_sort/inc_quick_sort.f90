recursive subroutine quick_sort_r8(vector, num)
    implicit none
    real(kind=8), intent(inout) :: vector(num)
    integer(kind=8), intent(in) :: num
    real(kind=8)                :: pivot, tmp
    integer(kind=8)             :: i, j, one, three, dev
    real(kind=8)                :: pivots(3)
    include "./include/sort_quick_sort/inc_quick_sort_detail.f90"        
    if (1 < i-1)    call quick_sort_r8(vector(1:i-1),  i-1)
    if (j+1 < num)  call quick_sort_r8(vector(j+1:num),num-j)
end subroutine quick_sort_r8

recursive subroutine quick_sort_i4(vector, num)
    implicit none
    integer(kind=4), intent(inout) :: vector(num)
    integer(kind=4), intent(in)    :: num
    integer(kind=4)                :: pivot, tmp
    integer(kind=4)                :: i, j, one, three, dev
    integer(kind=4)                :: pivots(3)
    include "./include/sort_quick_sort/inc_quick_sort_detail.f90"        
    if (1 < i-1)    call quick_sort_i4(vector(1:i-1),  i-1)
    if (j+1 < num)  call quick_sort_i4(vector(j+1:num),num-j)
end subroutine quick_sort_i4

recursive subroutine quick_sort_i8(vector, num)
    implicit none
    integer(kind=8), intent(inout) :: vector(num)
    integer(kind=8), intent(in)    :: num
    integer(kind=8)                :: pivot, tmp
    integer(kind=8)                :: i, j, one, three, dev
    integer(kind=8)                :: pivots(3)
    include "./include/sort_quick_sort/inc_quick_sort_detail.f90"        
    if (1 < i-1)    call quick_sort_i8(vector(1:i-1),  i-1)
    if (j+1 < num)  call quick_sort_i8(vector(j+1:num),num-j)
end subroutine quick_sort_i8
