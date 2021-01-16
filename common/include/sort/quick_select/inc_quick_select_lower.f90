recursive subroutine quick_select_lower_r8(val_lower, vector, num_all, num_bottom)
    implicit none
    real(kind=8), intent(inout)    :: val_lower
    real(kind=8), intent(inout)    :: vector(num_all)
    integer(kind=8), intent(in)    :: num_all
    integer(kind=8), intent(inout) :: num_bottom
    
    integer(kind=8) :: num_lower, num_upper, i, j
    real(kind=8)    :: pivot, tmp
    include "./include/sort/quick_select/inc_quick_select_lower_detail.f90"
end subroutine quick_select_lower_r8

recursive subroutine quick_select_lower_i4(val_lower, vector, num_all, num_bottom)
    implicit none
    integer(kind=4), intent(inout) :: val_lower
    integer(kind=4), intent(inout) :: vector(num_all)
    integer(kind=4), intent(in)    :: num_all
    integer(kind=4), intent(inout) :: num_bottom
    
    integer(kind=4) :: num_lower, num_upper, i, j
    integer(kind=4) :: pivot, tmp
    include "./include/sort/quick_select/inc_quick_select_lower_detail.f90"
end subroutine quick_select_lower_i4

recursive subroutine quick_select_lower_i8(val_lower, vector, num_all, num_bottom)
    implicit none
    integer(kind=8), intent(inout) :: val_lower
    integer(kind=8), intent(inout) :: vector(num_all)
    integer(kind=8), intent(in)    :: num_all
    integer(kind=8), intent(inout) :: num_bottom
    
    integer(kind=8) :: num_lower, num_upper, i, j
    integer(kind=8) :: pivot, tmp
    include "./include/sort/quick_select/inc_quick_select_lower_detail.f90"
end subroutine quick_select_lower_i8

