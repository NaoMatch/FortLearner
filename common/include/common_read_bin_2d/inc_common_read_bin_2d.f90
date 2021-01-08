subroutine read_bin_2d_int32(file_name, matrix)
    implicit none
    integer(kind=4), allocatable, intent(inout) :: matrix(:,:)
    integer(kind=4)                             :: n_rows, n_cols
    integer(kind=4)                             :: bit_size, bit_size_file
    character(len=1)                            :: dtype, dtype_set
    character(len=256), intent(in)              :: file_name
    integer(kind=4)                             :: nd, nd_file
    nd = 2
    dtype_set = "i"
    bit_size = 32
    include "./include/common_read_bin_2d/inc_common_read_bin_2d_detail.f90"
end subroutine read_bin_2d_int32

subroutine read_bin_2d_real64(file_name, matrix)
    implicit none
    real(kind=8), allocatable, intent(inout) :: matrix(:,:)
    integer(kind=8)                          :: n_rows, n_cols
    integer(kind=4)                          :: bit_size, bit_size_file
    character(len=1)                         :: dtype, dtype_set
    character(len=256), intent(in)           :: file_name
    integer(kind=4)                          :: nd, nd_file
    nd = 2
    dtype_set = "r"
    bit_size = 64
    include "./include/common_read_bin_2d/inc_common_read_bin_2d_detail.f90"
end subroutine read_bin_2d_real64

subroutine read_bin_2d_int64(file_name, matrix)
    implicit none
    integer(kind=8), allocatable, intent(inout) :: matrix(:,:)
    integer(kind=8)                             :: n_rows, n_cols
    integer(kind=4)                             :: bit_size, bit_size_file
    character(len=1)                            :: dtype, dtype_set
    character(len=256), intent(in)              :: file_name
    integer(kind=4)                             :: nd, nd_file
    nd = 2
    dtype_set = "i"
    bit_size = 64
    include "./include/common_read_bin_2d/inc_common_read_bin_2d_detail.f90"
end subroutine read_bin_2d_int64
