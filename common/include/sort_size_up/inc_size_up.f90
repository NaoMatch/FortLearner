subroutine size_up_r8(this)
    implicit none
    class(work)           :: this
    real(kind=8), allocatable      :: tmp_array(:)
    integer(kind=8), allocatable :: tmp_idx(:)

    allocate(tmp_array(this % current_size_i8))
    tmp_array(:) = this % tmp_r8(:)

    deallocate(this % tmp_r8)
    allocate(this % tmp_r8(this % current_size_i8 * 2))
    this % tmp_r8(1:this % current_size_i8) = tmp_array

    deallocate(tmp_array)

    if ( allocated(this%tmp_i8) ) then
        allocate(tmp_idx(this % current_size_i8))
        tmp_idx(:) = this % tmp_i8(:)

        deallocate(this % tmp_i8)
        allocate(this % tmp_i8(this % current_size_i8 * 2))
        this % tmp_i8(1:this % current_size_i8) = tmp_idx

        deallocate(tmp_idx)
    end if

    this % current_size_i8 = size(this % tmp_r8)
end subroutine size_up_r8

subroutine size_up_i4(this)
    implicit none
    class(work)           :: this
    integer(kind=4), allocatable      :: tmp_array(:)
    integer(kind=4), allocatable :: tmp_idx(:)

    allocate(tmp_array(this % current_size_i4))
    tmp_array(:) = this % tmp_i4(:)

    deallocate(this % tmp_i4)
    allocate(this % tmp_i4(this % current_size_i4 * 2))
    this % tmp_i4(1:this % current_size_i4) = tmp_array

    deallocate(tmp_array)

    if ( allocated(this%indices_i4) ) then
        allocate(tmp_idx(this % current_size_i4))
        tmp_idx(:) = this % indices_i4(:)

        deallocate(this % indices_i4)
        allocate(this % indices_i4(this % current_size_i4 * 2))
        this % indices_i4(1:this % current_size_i4) = tmp_idx

        deallocate(tmp_idx)
    end if

    this % current_size_i4 = size(this % tmp_i4)
end subroutine size_up_i4

subroutine size_up_i8(this)
    implicit none
    class(work)           :: this
    integer(kind=8), allocatable      :: tmp_array(:)
    integer(kind=8), allocatable :: tmp_idx(:)

    allocate(tmp_array(this % current_size_i8))
    tmp_array(:) = this % tmp_i8(:)

    deallocate(this % tmp_i8)
    allocate(this % tmp_i8(this % current_size_i8 * 2))
    this % tmp_i8(1:this % current_size_i8) = tmp_array

    deallocate(tmp_array)

    if ( allocated(this%indices_i8) ) then
        allocate(tmp_idx(this % current_size_i8))
        tmp_idx(:) = this % indices_i8(:)

        deallocate(this % indices_i8)
        allocate(this % indices_i8(this % current_size_i8 * 2))
        this % indices_i8(1:this % current_size_i8) = tmp_idx

        deallocate(tmp_idx)
    end if

    this % current_size_i8 = size(this % tmp_i8)
end subroutine size_up_i8
