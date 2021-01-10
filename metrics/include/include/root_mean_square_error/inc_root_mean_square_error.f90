function root_mean_sqaure_error_real64(y_true, y_pred)
    implicit none
    real(kind=8), intent(in) :: y_true(:)
    real(kind=8), intent(in) :: y_pred(:)
    real(kind=8) :: root_mean_sqaure_error_real64
    root_mean_sqaure_error_real64 = sqrt(mean_sqaure_error_real64(y_true, y_pred))
end function root_mean_sqaure_error_real64

function root_mean_sqaure_error_int32(y_true, y_pred)
    implicit none
    real(kind=4), intent(in) :: y_true(:)
    integer(kind=4), intent(in) :: y_pred(:)
    real(kind=4) :: root_mean_sqaure_error_int32
    root_mean_sqaure_error_int32 = sqrt(mean_sqaure_error_int32(y_true, y_pred))
end function root_mean_sqaure_error_int32

function root_mean_sqaure_error_int64(y_true, y_pred)
    implicit none
    real(kind=8), intent(in) :: y_true(:)
    integer(kind=8), intent(in) :: y_pred(:)
    real(kind=8) :: root_mean_sqaure_error_int64
    root_mean_sqaure_error_int64 = sqrt(mean_sqaure_error_int64(y_true, y_pred))
end function root_mean_sqaure_error_int64

