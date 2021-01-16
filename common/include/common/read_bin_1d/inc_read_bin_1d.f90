subroutine read_bin_1d_r8(file_name, vector)
    implicit none
    character(len=256), intent(in) :: file_name
    real(kind=8), allocatable      :: vector(:)
    integer(kind=8)                :: n_samples
    integer(kind=4)                :: bit_size, bit_size_file
    character(len=1)               :: dtype, dtype_set
    integer(kind=4)                :: nd, nd_file
    nd = 1
    dtype_set = "r"
    bit_size = 64
    include "./include/common/read_bin_1d/inc_read_bin_1d_detail.f90"
end subroutine read_bin_1d_r8

subroutine read_bin_1d_i4(file_name, vector)
    implicit none
    character(len=256), intent(in) :: file_name
    integer(kind=4), allocatable   :: vector(:)
    integer(kind=4)                :: n_samples
    integer(kind=4)                :: bit_size, bit_size_file
    character(len=1)               :: dtype, dtype_set
    integer(kind=4)                :: nd, nd_file
    nd = 1
    dtype_set = "i"
    bit_size = 32
    include "./include/common/read_bin_1d/inc_read_bin_1d_detail.f90"
end subroutine read_bin_1d_i4

subroutine read_bin_1d_i8(file_name, vector)
    implicit none
    character(len=256), intent(in) :: file_name
    integer(kind=8), allocatable   :: vector(:)
    integer(kind=8)                :: n_samples
    integer(kind=4)                :: bit_size, bit_size_file
    character(len=1)               :: dtype, dtype_set
    integer(kind=4)                :: nd, nd_file
    nd = 1
    dtype_set = "i"
    bit_size = 64
    include "./include/common/read_bin_1d/inc_read_bin_1d_detail.f90"
end subroutine read_bin_1d_i8
