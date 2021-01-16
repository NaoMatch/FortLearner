recursive subroutine quick_select_upper_r8(val_upper, vector, num_all, num_top)
    implicit none
    real(kind=8), intent(inout)    :: val_upper
    real(kind=8), intent(inout)    :: vector(num_all)
    integer(kind=8), intent(in)    :: num_all
    integer(kind=8), intent(inout) :: num_top
    integer(kind=8) :: num_bottom
    num_bottom = num_all - num_top
    call quick_select_lower(val_upper, vector, num_all, num_bottom)
end subroutine quick_select_upper_r8

recursive subroutine quick_select_upper_i4(val_upper, vector, num_all, num_top)
    implicit none
    integer(kind=4), intent(inout) :: val_upper
    integer(kind=4), intent(inout) :: vector(num_all)
    integer(kind=4), intent(in)    :: num_all
    integer(kind=4), intent(inout) :: num_top
    integer(kind=4) :: num_bottom
    num_bottom = num_all - num_top
    call quick_select_lower(val_upper, vector, num_all, num_bottom)
end subroutine quick_select_upper_i4

recursive subroutine quick_select_upper_i8(val_upper, vector, num_all, num_top)
    implicit none
    integer(kind=8), intent(inout) :: val_upper
    integer(kind=8), intent(inout) :: vector(num_all)
    integer(kind=8), intent(in)    :: num_all
    integer(kind=8), intent(inout) :: num_top
    integer(kind=8) :: num_bottom
    num_bottom = num_all - num_top
    call quick_select_lower(val_upper, vector, num_all, num_bottom)
end subroutine quick_select_upper_i8
