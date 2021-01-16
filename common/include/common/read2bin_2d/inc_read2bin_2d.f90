subroutine read2bin_2d_64bit(input_file_name, output_file_name, &
    n_samples, n_columns, skip_header, input_dtype, output_dtype)
    implicit none
    character(len=256), intent(in)  :: input_file_name, output_file_name
    integer(kind=8), intent(in)     :: n_samples, n_columns
    logical(kind=4), intent(in)     :: skip_header
    character(len=1), intent(in)    :: input_dtype, output_dtype
    integer(kind=8)                 :: i, i_tot
    real(kind=8), allocatable       :: matrix_r(:,:)
    integer(kind=8), allocatable    :: matrix_i(:,:)
    integer(kind=8), allocatable    :: tmp_i(:)
    real(kind=8), allocatable       :: tmp_r(:)
    integer(kind=4)                 :: bit_size
    integer(kind=4)                 :: nd
    nd = 2 ! #Dimension
    bit_size = 64 ! Bit Size
    include "./include/common/read2bin_2d/inc_read2bin_2d_detail.f90"
end subroutine read2bin_2d_64bit
