subroutine ifdealloc_mat_r8(matrix)
    real(kind=8), allocatable, intent(inout) :: matrix(:,:)
    if (allocated(matrix)) deallocate(matrix)
end subroutine ifdealloc_mat_r8

subroutine ifdealloc_mat_i4(matrix)
    integer(kind=4), allocatable, intent(inout) :: matrix(:,:)
    if (allocated(matrix)) deallocate(matrix)
end subroutine ifdealloc_mat_i4

subroutine ifdealloc_mat_i8(matrix)
    integer(kind=8), allocatable, intent(inout) :: matrix(:,:)
    if (allocated(matrix)) deallocate(matrix)
end subroutine ifdealloc_mat_i8
