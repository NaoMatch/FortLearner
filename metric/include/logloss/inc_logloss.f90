function logloss_i8(this, y_true, y_pred)
    implicit none
    class(metrics)  :: this
    real(kind=8)    :: logloss_i8
    integer(kind=8) :: y_true(:)
    real(kind=8)    :: y_pred(:)

    integer(kind=8) :: i, y_size_true, y_size_pred
    real(kind=8)    :: label, proba, tmp
    real(kind=8)    :: eposilon, one
    include "./include/logloss/inc_logloss_detail.f90"
    logloss_i8 = tmp / real(y_size_true, kind=kind(y_size_true))
end function logloss_i8
