! gfortran -fbounds-check ../common/mod_const.f90 ../common/mod_common.f90 main_read2bin.f90 -o main_read2bin.out
program main_read2bin
    ! Read File(csv, tsv...) and dump it, and Read dumped file.
    ! 'kind' is determined by the input parameters (n_rows and n_cols)
    use mod_const, only: t_, f_
    use mod_common, only: read2bin_2d, read_bin_2d
    implicit none
    
    character(len=1)   :: input_dtype, output_dtype
    character(len=256) :: input_file_name, output_file_name
    integer(kind=4)    :: n_samples_i4, n_columns_i4
    integer(kind=8)    :: n_samples_i8, n_columns_i8
    logical(kind=4)    :: skip_header
    integer(kind=4), allocatable :: mat_i4(:,:)
    integer(kind=4) :: i

    skip_header  = t_ ! This subroutine is not capable of handling headers.
    n_samples_i4 = 4
    n_samples_i8 = 4

    n_columns_i4 = 2
    n_columns_i8 = 2

    print*, "=============================================================="
    print*, "Read & Dump"
    print*, "input data type is integer"
    print*, "output data type is real"
    print*, "and 'kind=4' (n_samples_i4 and n_columns_i4 are integer(kind=4) )"
    input_file_name  = "./input/sample_input.txt"
    output_file_name = "./input/sample_input.bin"
    input_dtype = "i"
    output_dtype = "i"
    call read2bin_2d(                      &
        input_file_name, output_file_name, &
        n_samples_i4, n_columns_i4,        & 
        skip_header,                       & 
        input_dtype, output_dtype)


    print*, "=============================================================="
    print*, "Read Dump File"
    call read_bin_2d(output_file_name, mat_i4)

    do i=1, n_samples_i4, 1
        print*, mat_i4(i,:)
    end do







end program main_read2bin
