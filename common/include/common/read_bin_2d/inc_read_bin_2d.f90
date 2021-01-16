subroutine read_bin_2d_i4(file_name, matrix)
    implicit none
    character(len=256), intent(in)              :: file_name
    integer(kind=4), allocatable, intent(inout) :: matrix(:,:)
    integer(kind=4)                             :: n_samples, n_columns
    integer(kind=4)                             :: bit_size, bit_size_file
    character(len=1)                            :: dtype, dtype_set
    integer(kind=4)                             :: nd, nd_file
    nd = 2
    dtype_set = "i"
    bit_size = 32
    include "./include/common/read_bin_2d/inc_read_bin_2d_detail.f90"
end subroutine read_bin_2d_i4

subroutine read_bin_2d_r8(file_name, matrix)
    implicit none
    character(len=256), intent(in)           :: file_name
    real(kind=8), allocatable, intent(inout) :: matrix(:,:)
    integer(kind=8)                          :: n_samples, n_columns
    integer(kind=4)                          :: bit_size, bit_size_file
    character(len=1)                         :: dtype, dtype_set
    integer(kind=4)                          :: nd, nd_file
    nd = 2
    dtype_set = "r"
    bit_size = 64
    include "./include/common/read_bin_2d/inc_read_bin_2d_detail.f90"
end subroutine read_bin_2d_r8

subroutine read_bin_2d_i8(file_name, matrix)
    implicit none
    character(len=256), intent(in)              :: file_name
    integer(kind=8), allocatable, intent(inout) :: matrix(:,:)
    integer(kind=8)                             :: n_samples, n_columns
    integer(kind=4)                             :: bit_size, bit_size_file
    character(len=1)                            :: dtype, dtype_set
    integer(kind=4)                             :: nd, nd_file
    nd = 2
    dtype_set = "i"
    bit_size = 64
    include "./include/common/read_bin_2d/inc_read_bin_2d_detail.f90"
end subroutine read_bin_2d_i8
