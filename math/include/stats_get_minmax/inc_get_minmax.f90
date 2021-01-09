subroutine get_minmax_real64(min_val, max_val, vector, num)
    implicit none
    real(kind=8), intent(out)   :: min_val
    real(kind=8), intent(out)   :: max_val
    real(kind=8), intent(in)    :: vector(num)
    integer(kind=8), intent(in) :: num
    real(kind=8)    :: tmp_val, tmp_min_val, tmp_max_val
    real(kind=8)    :: buffer(buffer_get_minmax)
    integer(kind=8) :: n, unroll, j
    include "./include/stats_get_minmax/inc_get_minmax_detail.f90"
end subroutine get_minmax_real64

subroutine get_minmax_int32(min_val, max_val, vector, num)
    implicit none
    integer(kind=4), intent(out)   :: min_val
    integer(kind=4), intent(out)   :: max_val
    integer(kind=4), intent(in)    :: vector(num)
    integer(kind=4), intent(in) :: num
    integer(kind=4)    :: tmp_val, tmp_min_val, tmp_max_val
    integer(kind=4)    :: buffer(buffer_get_minmax)
    integer(kind=4) :: n, unroll, j
    include "./include/stats_get_minmax/inc_get_minmax_detail.f90"
end subroutine get_minmax_int32

subroutine get_minmax_int64(min_val, max_val, vector, num)
    implicit none
    integer(kind=8), intent(out)   :: min_val
    integer(kind=8), intent(out)   :: max_val
    integer(kind=8), intent(in)    :: vector(num)
    integer(kind=8), intent(in) :: num
    integer(kind=8)    :: tmp_val, tmp_min_val, tmp_max_val
    integer(kind=8)    :: buffer(buffer_get_minmax)
    integer(kind=8) :: n, unroll, j
    include "./include/stats_get_minmax/inc_get_minmax_detail.f90"
end subroutine get_minmax_int64
