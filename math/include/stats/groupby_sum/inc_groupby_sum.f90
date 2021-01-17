subroutine groupby_sum_r4_i4(uniq_x, x, stat_y, y, n_samples)
    implicit none
    real(kind=4), allocatable    :: uniq_x(:)
    real(kind=4), intent(in)     :: x(n_samples)
    integer(kind=4), allocatable :: stat_y(:)
    integer(kind=4), intent(in)  :: y(n_samples)
    integer(kind=4), intent(in)  :: n_samples

    real(kind=4), allocatable    :: x_copy(:)
    integer(kind=4), allocatable :: y_copy(:)
    integer(kind=4)              :: sum_y
    integer(kind=4)              :: n, i, i_start, i_stop, idx
    integer(kind=4), allocatable :: positions(:), indices(:)
    include "./include/stats/groupby_sum/inc_groupby_sum_detail.f90"
end subroutine groupby_sum_r4_i4

subroutine groupby_sum_r8_r8(uniq_x, x, stat_y, y, n_samples)
    implicit none
    real(kind=8), allocatable    :: uniq_x(:)
    real(kind=8), intent(in)     :: x(n_samples)
    real(kind=8), allocatable    :: stat_y(:)
    real(kind=8), intent(in)     :: y(n_samples)
    integer(kind=8), intent(in)  :: n_samples

    real(kind=8), allocatable    :: x_copy(:)
    real(kind=8), allocatable    :: y_copy(:)
    real(kind=8)                 :: sum_y
    integer(kind=8)              :: n, i, i_start, i_stop, idx
    integer(kind=8), allocatable :: positions(:), indices(:)
    include "./include/stats/groupby_sum/inc_groupby_sum_detail.f90"
end subroutine groupby_sum_r8_r8

subroutine groupby_sum_r8_i8(uniq_x, x, stat_y, y, n_samples)
    implicit none
    real(kind=8), allocatable    :: uniq_x(:)
    real(kind=8), intent(in)     :: x(n_samples)
    integer(kind=8), allocatable :: stat_y(:)
    integer(kind=8), intent(in)  :: y(n_samples)
    integer(kind=8), intent(in)  :: n_samples

    real(kind=8), allocatable    :: x_copy(:)
    integer(kind=8), allocatable :: y_copy(:)
    integer(kind=8)              :: sum_y
    integer(kind=8)              :: n, i, i_start, i_stop, idx
    integer(kind=8), allocatable :: positions(:), indices(:)
    include "./include/stats/groupby_sum/inc_groupby_sum_detail.f90"
end subroutine groupby_sum_r8_i8

subroutine groupby_sum_i4_r4(uniq_x, x, stat_y, y, n_samples)
    implicit none
    integer(kind=4), allocatable :: uniq_x(:)
    integer(kind=4), intent(in)  :: x(n_samples)
    real(kind=4), allocatable    :: stat_y(:)
    real(kind=4), intent(in)     :: y(n_samples)
    integer(kind=4), intent(in)  :: n_samples

    integer(kind=4), allocatable :: x_copy(:)
    real(kind=4), allocatable    :: y_copy(:)
    real(kind=4)                 :: sum_y
    integer(kind=4)              :: n, i, i_start, i_stop, idx
    integer(kind=4), allocatable :: positions(:), indices(:)
    include "./include/stats/groupby_sum/inc_groupby_sum_detail.f90"
end subroutine groupby_sum_i4_r4

subroutine groupby_sum_i4_i4(uniq_x, x, stat_y, y, n_samples)
    implicit none
    integer(kind=4), allocatable :: uniq_x(:)
    integer(kind=4), intent(in)  :: x(n_samples)
    integer(kind=4), allocatable :: stat_y(:)
    integer(kind=4), intent(in)  :: y(n_samples)
    integer(kind=4), intent(in)  :: n_samples

    integer(kind=4), allocatable :: x_copy(:)
    integer(kind=4), allocatable :: y_copy(:)
    integer(kind=4)              :: sum_y
    integer(kind=4)              :: n, i, i_start, i_stop, idx
    integer(kind=4), allocatable :: positions(:), indices(:)
    include "./include/stats/groupby_sum/inc_groupby_sum_detail.f90"
end subroutine groupby_sum_i4_i4

subroutine groupby_sum_i8_r8(uniq_x, x, stat_y, y, n_samples)
    implicit none
    integer(kind=8), allocatable :: uniq_x(:)
    integer(kind=8), intent(in)  :: x(n_samples)
    real(kind=8), allocatable    :: stat_y(:)
    real(kind=8), intent(in)     :: y(n_samples)
    integer(kind=8), intent(in)  :: n_samples

    integer(kind=8), allocatable :: x_copy(:)
    real(kind=8), allocatable    :: y_copy(:)
    real(kind=8)                 :: sum_y
    integer(kind=8)              :: n, i, i_start, i_stop, idx
    integer(kind=8), allocatable :: positions(:), indices(:)
    include "./include/stats/groupby_sum/inc_groupby_sum_detail.f90"
end subroutine groupby_sum_i8_r8

subroutine groupby_sum_i8_i8(uniq_x, x, stat_y, y, n_samples)
    implicit none
    integer(kind=8), allocatable :: uniq_x(:)
    integer(kind=8), intent(in)  :: x(n_samples)
    integer(kind=8), allocatable :: stat_y(:)
    integer(kind=8), intent(in)  :: y(n_samples)
    integer(kind=8), intent(in)  :: n_samples

    integer(kind=8), allocatable :: x_copy(:)
    integer(kind=8), allocatable :: y_copy(:)
    integer(kind=8)              :: sum_y
    integer(kind=8)              :: n, i, i_start, i_stop, idx
    integer(kind=8), allocatable :: positions(:), indices(:)
    include "./include/stats/groupby_sum/inc_groupby_sum_detail.f90"
end subroutine groupby_sum_i8_i8

