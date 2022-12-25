program main_csv2bin
    use mod_common
    implicit none
    intrinsic :: command_argument_count, get_command_argument

    integer(kind=4) :: status, length, i, n_args, idx_arg
    character(:), allocatable :: d_sep, n_rows_char, n_cols_char, file_csv, file_bin, dtype_in, dtype_out
    integer(kind=8) :: n_rows, n_cols


    n_args = command_argument_count()
    print*, "#Args: ", n_args

    ! Train/Valid/Test
    idx_arg = 1
    call get_command_argument(idx_arg, length = length, status = status)
    allocate (character(length) :: d_sep); call get_command_argument(idx_arg, d_sep, status = status)
    print*, d_sep
    
    ! Number of samples
    idx_arg = 2
    call get_command_argument(idx_arg, length = length, status = status)
    allocate (character(length) :: n_rows_char); call get_command_argument(idx_arg, n_rows_char, status = status)
    read(n_rows_char,*) n_rows
    print*, n_rows
    
    ! Number of columns
    idx_arg = 3
    call get_command_argument(idx_arg, length = length, status = status)
    allocate (character(length) :: n_cols_char); call get_command_argument(idx_arg, n_cols_char, status = status)
    read(n_cols_char,*) n_cols
    print*, n_cols
    
    ! Input CSV File Name
    idx_arg = 4
    call get_command_argument(idx_arg, length = length, status = status)
    allocate (character(length) :: file_csv); call get_command_argument(idx_arg, file_csv, status = status)
    print*, file_csv, " :: ", length, " :: ", len_trim(file_csv)
    
    ! Input Binary File Name
    idx_arg = 5
    call get_command_argument(idx_arg, length = length, status = status)
    allocate (character(length) :: file_bin); call get_command_argument(idx_arg, file_bin, status = status)
    print*, file_bin
    
    ! Input Data Type
    idx_arg = 6
    call get_command_argument(idx_arg, length = length, status = status)
    allocate (character(length) :: dtype_in); call get_command_argument(idx_arg, dtype_in, status = status)
    print*, dtype_in

    ! Input Binary File Name
    idx_arg = 7
    call get_command_argument(idx_arg, length = length, status = status)
    allocate (character(length) :: dtype_out); call get_command_argument(idx_arg, dtype_out, status = status)
    print*, dtype_out
    
    call read2bin_2d(file_csv, file_bin, n_rows, n_cols, t_, dtype_in, dtype_out)

    deallocate(d_sep, file_csv, file_bin, n_rows_char, n_cols_char, dtype_in, dtype_out)
end program main_csv2bin