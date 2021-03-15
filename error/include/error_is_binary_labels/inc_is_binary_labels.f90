subroutine is_binary_labels_i8(this, labels, n_samples, function_name)
    class(error)     :: this
    integer(kind=8)  :: labels(n_samples, 1_8)
    integer(kind=8)  :: n_samples
    character(len=*) :: function_name

    integer(kind=8)    :: n
    integer(kind=8)    :: min, max, val
    character(len=512) :: err_msg

    min =  huge(0_8)
    max = -huge(0_8)
    
    do n=1, n_samples, 1
        val = labels(n,1)
        min = minval((/min, val/))
        max = maxval((/max, val/))
    end do
    if (min .eq. 0_8 .and. max .eq. 1_8) return

    err_msg = "ValueError: The intput labels must be 0 and 1 for " // trim(function_name) // "."
    stop trim(err_msg)
end subroutine is_binary_labels_i8
