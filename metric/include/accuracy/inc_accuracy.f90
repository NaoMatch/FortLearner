function accuracy_i8(this, y_pred, y_true)
    implicit none
    class(metrics)              :: this
    real(kind=8)                :: accuracy_i8
    integer(kind=8), intent(in) :: y_pred(:)
    integer(kind=8), intent(in) :: y_true(:)

    integer(kind=8) :: y_size_true, y_size_pred
    integer(kind=8) :: count_correct, i, factor
    include "./include/accuracy/inc_accuracy_detail.f90"
    accuracy_i8 = real(count_correct, kind=8) / real(y_size_true, kind=8)
end function accuracy_i8
