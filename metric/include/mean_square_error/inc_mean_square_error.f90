function mean_square_error_r8(this, y_true, y_pred)
    implicit none
    class(metrics)           :: this
    real(kind=8), intent(in) :: y_true(:)
    real(kind=8), intent(in) :: y_pred(:)
    real(kind=8) :: mean_square_error_r8

    real(kind=8) :: tmp_sq_sum
    integer(kind=8) :: i, y_size_true, y_size_pred

    include "./include/mean_square_error/inc_mean_square_error_detail.f90"
    mean_square_error_r8 = tmp_sq_sum / dble(y_size_true)
end function mean_square_error_r8

function mean_square_error_i4(this, y_true, y_pred)
    implicit none
    class(metrics)           :: this
    real(kind=4), intent(in) :: y_true(:)
    integer(kind=4), intent(in) :: y_pred(:)
    real(kind=4) :: mean_square_error_i4

    real(kind=4) :: tmp_sq_sum
    integer(kind=4) :: i, y_size_true, y_size_pred

    include "./include/mean_square_error/inc_mean_square_error_detail.f90"
    mean_square_error_i4 = tmp_sq_sum / dble(y_size_true)
end function mean_square_error_i4

function mean_square_error_i8(this, y_true, y_pred)
    implicit none
    class(metrics)           :: this
    real(kind=8), intent(in) :: y_true(:)
    integer(kind=8), intent(in) :: y_pred(:)
    real(kind=8) :: mean_square_error_i8

    real(kind=8) :: tmp_sq_sum
    integer(kind=8) :: i, y_size_true, y_size_pred

    include "./include/mean_square_error/inc_mean_square_error_detail.f90"
    mean_square_error_i8 = tmp_sq_sum / dble(y_size_true)
end function mean_square_error_i8

