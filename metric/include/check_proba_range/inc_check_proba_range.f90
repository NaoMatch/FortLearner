subroutine check_proba_range_r8(this, y_pred)
    implicit none
    class(metrics) :: this
    real(kind=8)   :: y_pred(:)

    if (minval(y_pred) .lt. 0) then
        stop "Error: Predict Probability must be greater equal than 0."
    end if

    if (maxval(y_pred) .gt. 1) then
        stop "Error: Predict Probability must be less equal than 1."
    end if
end subroutine check_proba_range_r8
