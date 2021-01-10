subroutine check_non_empty_vector_i8(this, y_true_size, y_pred_size)
    class(metrics) :: this
    integer(kind=8), intent(in) :: y_true_size, y_pred_size

    if ( y_true_size .eq. 0 .or. y_pred_size .eq. 0 ) then
        stop "Error: Either of the two input vectors is empty."
    end if
end subroutine check_non_empty_vector_i8
