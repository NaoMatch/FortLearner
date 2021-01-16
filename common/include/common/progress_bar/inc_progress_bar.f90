subroutine progress_bar_i8(loop_index, loop_max_index, step_size)
    implicit none
    integer(kind=8), intent(in) :: loop_index,loop_max_index,step_size
    integer(kind=8), parameter  :: ndigit=100
    integer(kind=8)             :: j, step_size_mod
    step_size_mod = maxval((/step_size, 1_8/))
    include "./include/common/progress_bar/inc_progress_bar_detail.f90"
end subroutine progress_bar_i8
