function auc_i8(this, y_true, y_pred)
    implicit none
    class(metrics)   :: this
    integer(kind=8)  :: y_true(:)
    real(kind=8)     :: y_pred(:)
    real(kind=8)     :: auc_i8, tmp

    integer(kind=8) :: y_size_true, y_size_pred

    integer(kind=8)  :: i, idx
    integer(kind=8)  :: min_class_val, max_class_val
    real(kind=8)     :: min_proba_val, max_proba_val
    integer(kind=8), allocatable :: indices(:)
    real(kind=8)  :: count_false, y_true_i
    integer(kind=8)  :: min_label
    include "./include/auc/inc_auc_detail.f90"
    auc_i8 = tmp
end function auc_i8
