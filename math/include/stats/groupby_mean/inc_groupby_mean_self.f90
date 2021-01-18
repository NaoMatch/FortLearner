subroutine groupby_mean_self_r8(uniq_x, stat_x, x, n_samples)
    implicit none
    real(kind=8), allocatable   :: uniq_x(:)
    real(kind=8), allocatable   :: stat_x(:)
    real(kind=8), intent(in)    :: x(n_samples)
    integer(kind=8), intent(in) :: n_samples

    real(kind=8), allocatable    :: x_copy(:)
    real(kind=8), allocatable    :: sum_x(:)
    integer(kind=8), allocatable :: count_x(:)
    integer(kind=8) :: n, u, n_unique_x, zero_i
    real(kind=8) :: kind_r, zero_r=0.0
    include "./include/stats/groupby_mean/inc_groupby_mean_self_detail.f90"
end subroutine groupby_mean_self_r8

subroutine groupby_mean_self_i4(uniq_x, stat_x, x, n_samples)
    implicit none
    integer(kind=4), allocatable :: uniq_x(:)
    real(kind=4), allocatable    :: stat_x(:)
    integer(kind=4), intent(in)  :: x(n_samples)
    integer(kind=4), intent(in)  :: n_samples

    integer(kind=4), allocatable :: x_copy(:)
    integer(kind=4), allocatable :: sum_x(:)
    integer(kind=4), allocatable :: count_x(:)
    integer(kind=4) :: n, u, n_unique_x, zero_i
    real(kind=4) :: kind_r, zero_r=0.0
    include "./include/stats/groupby_mean/inc_groupby_mean_self_detail.f90"
end subroutine groupby_mean_self_i4

subroutine groupby_mean_self_i8(uniq_x, stat_x, x, n_samples)
    implicit none
    integer(kind=8), allocatable :: uniq_x(:)
    real(kind=8), allocatable    :: stat_x(:)
    integer(kind=8), intent(in)  :: x(n_samples)
    integer(kind=8), intent(in)  :: n_samples

    integer(kind=8), allocatable :: x_copy(:)
    integer(kind=8), allocatable :: sum_x(:)
    integer(kind=8), allocatable :: count_x(:)
    integer(kind=8) :: n, u, n_unique_x, zero_i
    real(kind=8) :: kind_r, zero_r=0.0
    include "./include/stats/groupby_mean/inc_groupby_mean_self_detail.f90"
end subroutine groupby_mean_self_i8
