subroutine get_minmax_r8(min_val, max_val, vector, n_samples)
    implicit none
    real(kind=8), intent(out)   :: min_val
    real(kind=8), intent(out)   :: max_val
    real(kind=8), intent(in)    :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    real(kind=8)    :: tmp_val, tmp_min_val, tmp_max_val
    real(kind=8)    :: buffer(buffer_get_minmax)
    integer(kind=8) :: n, n_samples_unroll, j
    include "./include/stats/get_minmax/inc_get_minmax_detail.f90"
end subroutine get_minmax_r8

subroutine get_minmax_i4(min_val, max_val, vector, n_samples)
    implicit none
    integer(kind=4), intent(out)   :: min_val
    integer(kind=4), intent(out)   :: max_val
    integer(kind=4), intent(in)    :: vector(n_samples)
    integer(kind=4), intent(in) :: n_samples
    integer(kind=4)    :: tmp_val, tmp_min_val, tmp_max_val
    integer(kind=4)    :: buffer(buffer_get_minmax)
    integer(kind=4) :: n, n_samples_unroll, j
    include "./include/stats/get_minmax/inc_get_minmax_detail.f90"
end subroutine get_minmax_i4

subroutine get_minmax_i8(min_val, max_val, vector, n_samples)
    implicit none
    integer(kind=8), intent(out)   :: min_val
    integer(kind=8), intent(out)   :: max_val
    integer(kind=8), intent(in)    :: vector(n_samples)
    integer(kind=8), intent(in) :: n_samples
    integer(kind=8)    :: tmp_val, tmp_min_val, tmp_max_val
    integer(kind=8)    :: buffer(buffer_get_minmax)
    integer(kind=8) :: n, n_samples_unroll, j
    include "./include/stats/get_minmax/inc_get_minmax_detail.f90"
end subroutine get_minmax_i8
