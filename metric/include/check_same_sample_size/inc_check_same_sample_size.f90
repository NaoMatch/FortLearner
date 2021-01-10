subroutine check_same_sample_size_i8(this, y_true_size, y_pred_size)
    class(metrics) :: this
    integer(kind=8), intent(in) :: y_true_size, y_pred_size

    if ( y_true_size .ne. y_pred_size ) then
        stop "Error: The two input vectors have different sizes."
    end if
end subroutine check_same_sample_size_i8
