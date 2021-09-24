subroutine ifdealloc_vec_r8(vector)
    real(kind=8), allocatable, intent(inout) :: vector(:)
    if (allocated(vector)) deallocate(vector)
end subroutine ifdealloc_vec_r8

subroutine ifdealloc_vec_i4(vector)
    integer(kind=4), allocatable, intent(inout) :: vector(:)
    if (allocated(vector)) deallocate(vector)
end subroutine ifdealloc_vec_i4

subroutine ifdealloc_vec_i8(vector)
    integer(kind=8), allocatable, intent(inout) :: vector(:)
    if (allocated(vector)) deallocate(vector)
end subroutine ifdealloc_vec_i8
