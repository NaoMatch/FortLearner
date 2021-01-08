subroutine read2bin_1d_64bit(input_file_name, output_file_name, n_rows, skip_header, input_dtype, output_dtype)
    implicit none
    character(len=256), intent(in)  :: input_file_name, output_file_name
    integer(kind=8), intent(in)     :: n_rows
    logical(kind=4), intent(in)     :: skip_header
    character(len=1), intent(in)    :: input_dtype, output_dtype
    integer(kind=8)                 :: i, i_tot
    real(kind=8), allocatable       :: vector_r(:)
    integer(kind=8), allocatable    :: vector_i(:)
    integer(kind=8)                 :: tmp_i
    real(kind=8)                    :: tmp_r
    integer(kind=4)                 :: bit_size
    integer(kind=4)                 :: nd
    nd = 1
    bit_size = 64
    include "./include/common_read2bin_1d/inc_read2bin_1d_detail.f90"
end subroutine read2bin_1d_64bit
