function root_mean_square_error_r8(this, y_true, y_pred)
    implicit none
    class(metrics)           :: this
    real(kind=8), intent(in) :: y_true(:)
    real(kind=8), intent(in) :: y_pred(:)
    real(kind=8) :: root_mean_square_error_r8
    root_mean_square_error_r8 = sqrt(this%mean_square_error_r8(y_true, y_pred))
end function root_mean_square_error_r8

function root_mean_square_error_i4(this, y_true, y_pred)
    implicit none
    class(metrics)           :: this
    real(kind=4), intent(in) :: y_true(:)
    integer(kind=4), intent(in) :: y_pred(:)
    real(kind=4) :: root_mean_square_error_i4
    root_mean_square_error_i4 = sqrt(this%mean_square_error_i4(y_true, y_pred))
end function root_mean_square_error_i4

function root_mean_square_error_i8(this, y_true, y_pred)
    implicit none
    class(metrics)           :: this
    real(kind=8), intent(in) :: y_true(:)
    integer(kind=8), intent(in) :: y_pred(:)
    real(kind=8) :: root_mean_square_error_i8
    root_mean_square_error_i8 = sqrt(this%mean_square_error_i8(y_true, y_pred))
end function root_mean_square_error_i8

