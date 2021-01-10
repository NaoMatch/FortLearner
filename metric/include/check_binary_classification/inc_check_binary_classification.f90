subroutine check_binary_classification_i8(this, y_true)
    implicit none
    class(metrics)  :: this
    integer(kind=8) :: y_true(:)
    integer(kind=8) :: min_class_val, max_class_val
    integer(kind=8) :: class_diff

    min_class_val = minval(y_true)
    max_class_val = maxval(y_true)

    if (min_class_val .eq. max_class_val) then
        stop "Error: All instaces are same class."
    end if

    class_diff = abs(max_class_val-min_class_val)
    if (class_diff .ne. 1) then
        stop "Error: Binary classification Only."
    end if
end subroutine check_binary_classification_i8
