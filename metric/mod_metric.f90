module mod_metric
    use mod_sort
    use mod_stats
    implicit none
    
    type metrics
    contains
        procedure, pass :: accuracy_i4
        procedure, pass :: accuracy_i8
        generic   :: accuracy => accuracy_i4, accuracy_i8

        procedure, pass :: auc_i4
        procedure, pass :: auc_i8
        generic   :: auc => auc_i4, auc_i8

        procedure, pass :: mean_square_error_r4
        procedure, pass :: mean_square_error_r8
        procedure, pass :: mean_square_error_i4
        procedure, pass :: mean_square_error_i8
        generic   :: mean_square_error => &
            mean_square_error_r4, mean_square_error_r8, &
            mean_square_error_i4, mean_square_error_i8

        procedure, pass :: root_mean_square_error_r4
        procedure, pass :: root_mean_square_error_r8
        procedure, pass :: root_mean_square_error_i4
        procedure, pass :: root_mean_square_error_i8
        generic   :: root_mean_square_error => &
            root_mean_square_error_r4, root_mean_square_error_r8, &
            root_mean_square_error_i4, root_mean_square_error_i8


        procedure, pass :: check_same_sample_size_i4
        procedure, pass :: check_same_sample_size_i8
        generic :: check_same_sample_size => check_same_sample_size_i4, check_same_sample_size_i8

        procedure, pass :: check_non_empty_vector_i4
        procedure, pass :: check_non_empty_vector_i8
        generic :: check_non_empty_vector => check_non_empty_vector_i4, check_non_empty_vector_i8

        procedure, pass :: check_binary_classification_i4
        procedure, pass :: check_binary_classification_i8
        generic :: check_binary_classification => check_binary_classification_i4, check_binary_classification_i8

        procedure, pass :: check_proba_range_r4
        procedure, pass :: check_proba_range_r8
        generic :: check_proba_range => check_proba_range_r4, check_proba_range_r8
    end type metrics

contains

    !> A function to compute accuracy.
    !! \param y_true ground-truth label
    !! \param y_pred predicted label
    function accuracy_i4(this, y_pred, y_true)
        implicit none
        class(metrics)                :: this
        real(kind=4)                :: accuracy_i4
        integer(kind=4), intent(in) :: y_pred(:)
        integer(kind=4), intent(in) :: y_true(:)

        integer(kind=4) :: y_size_true, y_size_pred
        integer(kind=4) :: count_correct, i, factor
        include "./include/accuracy/inc_accuracy_detail.f90"
        accuracy_i4 = real(count_correct, kind=4) / real(y_size_true, kind=4)
    end function accuracy_i4
    include "./include/accuracy/inc_accuracy.f90"


    !> A function to compute auc
    function auc_i4(this, y_true, y_pred)
        implicit none
        class(metrics)   :: this
        integer(kind=4)  :: y_true(:)
        real(kind=4)     :: y_pred(:)
        real(kind=4)     :: auc_i4, tmp

        integer(kind=4) :: y_size_true, y_size_pred

        integer(kind=4)  :: i, idx
        integer(kind=4)  :: min_class_val, max_class_val
        real(kind=4)     :: min_proba_val, max_proba_val
        integer(kind=4), allocatable :: indices(:)
        real(kind=4)     :: count_false, y_true_i
        integer(kind=4)  :: min_label
        include "./include/auc/inc_auc_detail.f90"
        auc_i4 = tmp
    end function auc_i4
    include "./include/auc/inc_auc.f90"


    !> A function to compute Mean Squared Error.
    !! \return returns Mean Squared Error
    !! \param y_true ground-truth values
    !! \param y_pred predicted values
    function mean_square_error_r4(this, y_true, y_pred)
        implicit none
        class(metrics)           :: this
        real(kind=4), intent(in) :: y_true(:)
        real(kind=4), intent(in) :: y_pred(:)
        real(kind=4) :: mean_square_error_r4

        real(kind=4) :: tmp_sq_sum
        integer(kind=4) :: i, y_size_true, y_size_pred

        include "./include/mean_square_error/inc_mean_square_error_detail.f90"
        mean_square_error_r4 = tmp_sq_sum / dble(y_size_true)
    end function mean_square_error_r4
    include "./include/mean_square_error/inc_mean_square_error.f90"


    !> A function to compute Root Mean Squared Error.
    ! \return returns Root Mean Squared Error
    ! \param y_true ground-truth values
    ! \param y_pred predicted values
    function root_mean_square_error_r4(this, y_true, y_pred)
        implicit none
        class(metrics)           :: this
        real(kind=4), intent(in) :: y_true(:)
        real(kind=4), intent(in) :: y_pred(:)
        real(kind=4) :: root_mean_square_error_r4
        root_mean_square_error_r4 = sqrt(this%mean_square_error_r4(y_true, y_pred))
    end function root_mean_square_error_r4
    include "./include/root_mean_square_error/inc_root_mean_square_error.f90"


    !> A subroutine to check sample size.
    !! \param y_true_size size of ground-truth
    !! \param y_true_size size of predict
    subroutine check_same_sample_size_i4(this, y_true_size, y_pred_size)
        class(metrics) :: this
        integer(kind=4), intent(in) :: y_true_size, y_pred_size

        if ( y_true_size .ne. y_pred_size ) then
            stop "Error: The two input vectors have different sizes."
        end if
    end subroutine check_same_sample_size_i4
    include "./include/check_same_sample_size/inc_check_same_sample_size.f90"


    !> A subroutine to check is not empty
    !! \param y_true_size size of ground-truth
    !! \param y_true_size size of predict
    subroutine check_non_empty_vector_i4(this, y_true_size, y_pred_size)
        class(metrics) :: this
        integer(kind=4), intent(in) :: y_true_size, y_pred_size

        if ( y_true_size .eq. 0 .or. y_pred_size .eq. 0 ) then
            stop "Error: Either of the two input vectors is empty."
        end if
    end subroutine check_non_empty_vector_i4
    include "./include/check_non_empty_vector/inc_check_non_empty_vector.f90"


    !> A subroutine to chekc binary classification
    !! \param y_true ground-truth
    subroutine check_binary_classification_i4(this, y_true)
        implicit none
        class(metrics)  :: this
        integer(kind=4) :: y_true(:)
        integer(kind=4) :: min_class_val, max_class_val
        integer(kind=4) :: class_diff

        min_class_val = minval(y_true)
        max_class_val = maxval(y_true)

        if (min_class_val .eq. max_class_val) then
            stop "Error: All instaces are same class."
        end if

        class_diff = abs(max_class_val-min_class_val)
        if (class_diff .ne. 1) then
            stop "Error: Binary classification Only."
        end if
    end subroutine check_binary_classification_i4
    include "./include/check_binary_classification/inc_check_binary_classification.f90"


    !> A subroutine to check probability range
    !! \param y_pred predicted probability
    subroutine check_proba_range_r4(this, y_pred)
        implicit none
        class(metrics) :: this
        real(kind=4)   :: y_pred(:)

        if (minval(y_pred) .lt. 0) then
            stop "Error: Predict Probability must be greater equal than 0."
        end if

        if (maxval(y_pred) .gt. 1) then
            stop "Error: Predict Probability must be less equal than 1."
        end if
    end subroutine check_proba_range_r4
    include "./include/check_proba_range/inc_check_proba_range.f90"


end module mod_metric
