recursive subroutine quick_argsort_r4_r4(vector1, vector2, num)
    implicit none
    real(kind=4), intent(inout)    :: vector1(num)
    real(kind=4), intent(inout)    :: vector2(num)
    integer(kind=4), intent(in)    :: num
    real(kind=4)                   :: pivot, tmp1
    real(kind=4)                   :: tmp2
    integer(kind=4)                :: i, j, one, three, dev
    real(kind=4)                   :: pivots(3)
    include "./include/sort/quick_argsort/inc_quick_argsort_detail.f90"        
    if (1 < i-1)    call quick_argsort_r4_r4(vector1(1:i-1),  vector2(1:i-1), i-1)
    if (j+1 < num)  call quick_argsort_r4_r4(vector1(j+1:num),vector2(j+1:num), num-j)
end subroutine quick_argsort_r4_r4

recursive subroutine quick_argsort_r8_i8(vector1, vector2, num)
    implicit none
    real(kind=8), intent(inout)    :: vector1(num)
    integer(kind=8), intent(inout) :: vector2(num)
    integer(kind=8), intent(in)    :: num
    real(kind=8)                   :: pivot, tmp1
    integer(kind=8)                :: tmp2
    integer(kind=8)                :: i, j, one, three, dev
    real(kind=8)                   :: pivots(3)
    include "./include/sort/quick_argsort/inc_quick_argsort_detail.f90"        
    if (1 < i-1)    call quick_argsort_r8_i8(vector1(1:i-1),  vector2(1:i-1),  i-1)
    if (j+1 < num)  call quick_argsort_r8_i8(vector1(j+1:num),vector2(j+1:num),num-j)
end subroutine quick_argsort_r8_i8

recursive subroutine quick_argsort_r8_r8(vector1, vector2, num)
    implicit none
    real(kind=8), intent(inout)    :: vector1(num)
    real(kind=8), intent(inout)    :: vector2(num)
    integer(kind=8), intent(in)    :: num
    real(kind=8)                   :: pivot, tmp1
    real(kind=8)                   :: tmp2
    integer(kind=8)                :: i, j, one, three, dev
    real(kind=8)                   :: pivots(3)
    include "./include/sort/quick_argsort/inc_quick_argsort_detail.f90"        
    if (1 < i-1)    call quick_argsort_r8_r8(vector1(1:i-1),  vector2(1:i-1), i-1)
    if (j+1 < num)  call quick_argsort_r8_r8(vector1(j+1:num),vector2(j+1:num), num-j)
end subroutine quick_argsort_r8_r8

recursive subroutine quick_argsort_i4_i4(vector1, vector2, num)
    implicit none
    integer(kind=4), intent(inout) :: vector1(num)
    integer(kind=4), intent(inout) :: vector2(num)
    integer(kind=4), intent(in)    :: num
    integer(kind=4)                :: pivot, tmp1
    integer(kind=4)                :: tmp2
    integer(kind=4)                :: i, j, one, three, dev
    integer(kind=4)                :: pivots(3)
    include "./include/sort/quick_argsort/inc_quick_argsort_detail.f90"        
    if (1 < i-1)    call quick_argsort_i4_i4(vector1(1:i-1),  vector2(1:i-1),  i-1)
    if (j+1 < num)  call quick_argsort_i4_i4(vector1(j+1:num),vector2(j+1:num),num-j)
end subroutine quick_argsort_i4_i4

recursive subroutine quick_argsort_i4_r4(vector1, vector2, num)
    implicit none
    integer(kind=4), intent(inout) :: vector1(num)
    real(kind=4), intent(inout)    :: vector2(num)
    integer(kind=4), intent(in)    :: num
    integer(kind=4)                :: pivot, tmp1
    real(kind=4)                   :: tmp2
    integer(kind=4)                :: i, j, one, three, dev
    integer(kind=4)                :: pivots(3)
    include "./include/sort/quick_argsort/inc_quick_argsort_detail.f90"        
    if (1 < i-1)    call quick_argsort_i4_r4(vector1(1:i-1),  vector2(1:i-1), i-1)
    if (j+1 < num)  call quick_argsort_i4_r4(vector1(j+1:num),vector2(j+1:num), num-j)
end subroutine quick_argsort_i4_r4

recursive subroutine quick_argsort_i8_i8(vector1, vector2, num)
    implicit none
    integer(kind=8), intent(inout) :: vector1(num)
    integer(kind=8), intent(inout) :: vector2(num)
    integer(kind=8), intent(in)    :: num
    integer(kind=8)                :: pivot, tmp1
    integer(kind=8)                :: tmp2
    integer(kind=8)                :: i, j, one, three, dev
    integer(kind=8)                :: pivots(3)
    include "./include/sort/quick_argsort/inc_quick_argsort_detail.f90"        
    if (1 < i-1)    call quick_argsort_i8_i8(vector1(1:i-1),  vector2(1:i-1),  i-1)
    if (j+1 < num)  call quick_argsort_i8_i8(vector1(j+1:num),vector2(j+1:num),num-j)
end subroutine quick_argsort_i8_i8

recursive subroutine quick_argsort_i8_r8(vector1, vector2, num)
    implicit none
    integer(kind=8), intent(inout) :: vector1(num)
    real(kind=8), intent(inout)    :: vector2(num)
    integer(kind=8), intent(in)    :: num
    integer(kind=8)                :: pivot, tmp1
    real(kind=8)                   :: tmp2
    integer(kind=8)                :: i, j, one, three, dev
    integer(kind=8)                :: pivots(3)
    include "./include/sort/quick_argsort/inc_quick_argsort_detail.f90"        
    if (1 < i-1)    call quick_argsort_i8_r8(vector1(1:i-1),  vector2(1:i-1), i-1)
    if (j+1 < num)  call quick_argsort_i8_r8(vector1(j+1:num),vector2(j+1:num), num-j)
end subroutine quick_argsort_i8_r8
