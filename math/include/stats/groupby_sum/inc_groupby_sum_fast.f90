subroutine groupby_sum_fast_r8(uniq_x, stat_x, x, n_samples)
    implicit none
    real(kind=8), allocatable   :: uniq_x(:)
    real(kind=8), allocatable   :: stat_x(:)
    real(kind=8), intent(in)    :: x(n_samples)
    integer(kind=8), intent(in) :: n_samples

    real(kind=8), allocatable :: x_copy(:)
    real(kind=8)              :: sum_x
    integer(kind=8)           :: n, i, i_start, i_stop, n_unique, idx
    integer(kind=8), allocatable :: positions(:)
    include "./include/stats/groupby_sum/inc_groupby_sum_fast_detail.f90"
end subroutine groupby_sum_fast_r8

subroutine groupby_sum_fast_i4(uniq_x, stat_x, x, n_samples)
    implicit none
    integer(kind=4), allocatable :: uniq_x(:)
    integer(kind=4), allocatable :: stat_x(:)
    integer(kind=4), intent(in)  :: x(n_samples)
    integer(kind=4), intent(in)  :: n_samples

    integer(kind=4), allocatable :: x_copy(:)
    integer(kind=4)              :: sum_x
    integer(kind=4)              :: n, i, i_start, i_stop, n_unique, idx
    integer(kind=4), allocatable :: positions(:)
    include "./include/stats/groupby_sum/inc_groupby_sum_fast_detail.f90"
end subroutine groupby_sum_fast_i4

subroutine groupby_sum_fast_i8(uniq_x, stat_x, x, n_samples)
    implicit none
    integer(kind=8), allocatable :: uniq_x(:)
    integer(kind=8), allocatable :: stat_x(:)
    integer(kind=8), intent(in)  :: x(n_samples)
    integer(kind=8), intent(in)  :: n_samples

    integer(kind=8), allocatable :: x_copy(:)
    integer(kind=8)              :: sum_x
    integer(kind=8)              :: n, i, i_start, i_stop, n_unique, idx
    integer(kind=8), allocatable :: positions(:)
    include "./include/stats/groupby_sum/inc_groupby_sum_fast_detail.f90"
end subroutine groupby_sum_fast_i8
