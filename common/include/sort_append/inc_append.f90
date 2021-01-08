subroutine append_r8(this, tmp_val)
    implicit none
    class(work) :: this
    real(kind=8)      :: tmp_val
    if ( this % idx_i8 .gt. this % current_size_i8 ) call this % size_up_r8()
    this % tmp_r8(this % idx_i8) = tmp_val
    this % idx_i8 = this % idx_i8 + 1
end subroutine append_r8

subroutine append_i4(this, tmp_val)
    implicit none
    class(work) :: this
    integer(kind=4)      :: tmp_val
    if ( this % idx_i4 .gt. this % current_size_i4 ) call this % size_up_i4()
    this % tmp_i4(this % idx_i4) = tmp_val
    this % idx_i4 = this % idx_i4 + 1
end subroutine append_i4

subroutine append_i8(this, tmp_val)
    implicit none
    class(work) :: this
    integer(kind=8)      :: tmp_val
    if ( this % idx_i8 .gt. this % current_size_i8 ) call this % size_up_i8()
    this % tmp_i8(this % idx_i8) = tmp_val
    this % idx_i8 = this % idx_i8 + 1
end subroutine append_i8