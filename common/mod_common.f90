!> A module for common functions.
module mod_common
    use mod_const
    implicit none

    !> An interface to binary search from left
    interface binary_search_left
        module procedure binary_search_left_r4
        module procedure binary_search_left_r8
        module procedure binary_search_left_i4
        module procedure binary_search_left_i8
    end interface binary_search_left

    !> An interface to binary search from right
    interface binary_search_right
        module procedure binary_search_right_r4
        module procedure binary_search_right_r8
        module procedure binary_search_right_i4
        module procedure binary_search_right_i8
    end interface binary_search_right

    !> An interface to collect unique values from already sorted array
    interface collect_unique_values
        module procedure collect_unique_values_r4
        module procedure collect_unique_values_r8
        module procedure collect_unique_values_i4
        module procedure collect_unique_values_i8
    end interface collect_unique_values

    !> An interface to count unique values from already sorted array
    interface count_unique
        module procedure count_unique_r4
        module procedure count_unique_r8
        module procedure count_unique_i4
        module procedure count_unique_i8
    end interface count_unique

    !> An interface to find rightmost value index from 1-dim array less equal input value
    !> https://docs.python.org/ja/3/library/bisect.html
    interface find_le
        module procedure find_le_r8
    end interface find_le

    !> An interface to find rightmost value index from 1-dim array less than input value
    !> https://docs.python.org/ja/3/library/bisect.html
    interface find_lt
        module procedure find_lt_r8
    end interface find_lt

    !> An interface to find leftmost value index from 1-dim array greater equal input value
    !> https://docs.python.org/ja/3/library/bisect.html
    interface find_ge
        module procedure find_ge_r8
    end interface find_ge

    !> An interface to find leftmost value index from 1-dim array greater than input value
    !> https://docs.python.org/ja/3/library/bisect.html
    interface find_gt
        module procedure find_gt_r8
    end interface find_gt

    !> An interface to find the location and absolute values of the absolute maximum from the non-diagonal elements of a symmetric matrix.
    interface get_abs_maxloc
        module procedure get_abs_maxloc_symmat_r4
        module procedure get_abs_maxloc_symmat_r8
        module procedure get_abs_maxloc_symmat_i4
        module procedure get_abs_maxloc_symmat_i8
    end interface get_abs_maxloc

    !> An interface to get difference of an input array.
    interface get_indices_diff
        module procedure get_indices_diff_i8
    end interface get_indices_diff

    !> An interface to get digit of input integer
    interface get_int_digit
        module procedure get_int_digit_i4
        module procedure get_int_digit_i8       
    end interface get_int_digit

    !> An interface to initialize allocated square matrix to identity matrix
    interface identity
        module procedure identity_r4
        module procedure identity_r8 ! place in "./include/common_identity/inc_identity.f90"
        module procedure identity_i4  ! place in "./include/common_identity/inc_identity.f90"
        module procedure identity_i8  ! place in "./include/common_identity/inc_identity.f90"
    end interface identity

    !> An interface to deallocate already allocated 1-dim, 2-dim array
    interface ifdealloc
        module procedure ifdealloc_vec_r4
        module procedure ifdealloc_vec_r8
        module procedure ifdealloc_vec_i4
        module procedure ifdealloc_vec_i8
        module procedure ifdealloc_mat_r4
        module procedure ifdealloc_mat_r8
        module procedure ifdealloc_mat_i4
        module procedure ifdealloc_mat_i8
    end interface ifdealloc

    !> An interface to deallocate already allocated 1-dim, 2-dim array
    interface ifnullify
        module procedure ifnullify_i8
        module procedure ifnullify_r8
    end interface ifnullify

    !> An interface to check if the input array is already sorted.
    interface is_sorted
        module procedure is_sorted_r4
        module procedure is_sorted_r8
        module procedure is_sorted_i4
        module procedure is_sorted_i8
    end interface is_sorted

    !> An interface to return the position of the value less equal the input value from the ascending sorted 1-dim array.
    interface linear_search
        module procedure linear_search_r4
        module procedure linear_search_r8
        module procedure linear_search_i4
        module procedure linear_search_i8
    end interface linear_search

    !> An interface to get most left non-zero bit, except sign.
    interface most_left_bit_position 
        module procedure most_left_bit_position_i4
        module procedure most_left_bit_position_i8
    end interface most_left_bit_position

    !> An interface to convert number to character
    interface num2char
        module procedure num2char_r4
        module procedure num2char_r8
        module procedure num2char_i4
        module procedure num2char_i8
    end interface num2char

    !> An interface to print progress bar
    interface progress_bar 
        module procedure progress_bar_i4
        module procedure progress_bar_i8 ! place in "./include/common_progress_bar/inc_progress_bar.f90"
    end interface progress_bar

    !> An interface to read binary file dumped by 'read2bin_1d'
    interface read_bin_1d
        module procedure read_bin_1d_r4
        module procedure read_bin_1d_r8
        module procedure read_bin_1d_i4
        module procedure read_bin_1d_i8
    end interface read_bin_1d

    !> An interface to read binary file dumped by 'read2bin_2d'
    interface read_bin_2d
        module procedure read_bin_2d_r4
        module procedure read_bin_2d_r8
        module procedure read_bin_2d_i4
        module procedure read_bin_2d_i8
    end interface read_bin_2d

    !> An interface to read a file with only one column and dump it as binary file.
    !> The dumped file can only be read 'read_bin_1d'.
    interface read2bin_1d
        module procedure read2bin_1d_32bit
        module procedure read2bin_1d_64bit ! place in "./include/common_read2bin_1d/inc_read2bin_1d.f90"
    end interface read2bin_1d

    !> An interface to read a file with multiple columns and dump it as binary file.
    !> The dumped file can only be read 'read_bin_2d'.
    interface read2bin_2d
        module procedure read2bin_2d_32bit
        module procedure read2bin_2d_64bit ! place in "./include/common_read2bin_2d/inc_read2bin_2d.f90"
    end interface read2bin_2d

    !> An interface to calculate a matrix from two vectors.
    interface vv2mat
        module procedure vv2mat_r8
    end interface vv2mat

    !> An interface to calculate a scalar value from one square matrix and one vector.
    interface vmv
        module procedure vmv_r8
    end interface vmv

contains

    subroutine ifnullify_i8(ptr)
        implicit none
        integer(kind=8), pointer :: ptr
        if (ASSOCIATED(ptr)) nullify(ptr)
    end subroutine ifnullify_i8

    subroutine ifnullify_r8(ptr)
        implicit none
        real(kind=8), pointer :: ptr
        if (ASSOCIATED(ptr)) nullify(ptr)
    end subroutine ifnullify_r8

    !> A function to binary search from left.
    !> 'vector' must be sorted by ascending order.
    !! \return returns the index of the closest value less than 'value'.
    !! \param vector ascending sorted array to be searched
    !! \param n_samples size of input vector
    !! \param value value
    function binary_search_left_r4(vector, n_samples, value)
        implicit none
        integer(kind=4)             :: binary_search_left_r4
        real(kind=4), intent(in)    :: vector(n_samples)
        integer(kind=4), intent(in) :: n_samples
        real(kind=4), intent(in)    :: value

        integer(kind=4) :: lo, hi, mid
        include "./include/common/binary_search/inc_binary_search_left_detail.f90"
        binary_search_left_r4 = lo
    end function binary_search_left_r4
    include "./include/common/binary_search/inc_binary_search_left.f90"


    !> A function to binary search from right.
    !> 'vector' must be sorted by ascending order.
    !! \return returns the index of the closest value greater than 'value'.
    !! \param vector ascending sorted array to be searched
    !! \param n_samples size of input vector
    !! \param value value
    function binary_search_right_r4(vector, n_samples, value)
        implicit none
        integer(kind=4) :: binary_search_right_r4
        real(kind=4), intent(in)    :: vector(n_samples)
        integer(kind=4), intent(in) :: n_samples
        real(kind=4), intent(in)    :: value

        integer(kind=4) :: lo, hi, mid
        include "./include/common/binary_search/inc_binary_search_right_detail.f90"
        binary_search_right_r4 = lo
    end function binary_search_right_r4
    include "./include/common/binary_search/inc_binary_search_right.f90"


    !> A subroutine to collect unique values from already sorted array
    !! \return returns unique values
    !! \param uniq_vals unique values collected from 'vector'
    !! \param vector  ascending sorted array to be searched
    !! \param n_samples number of samples 
    subroutine collect_unique_values_r4(uniq_vals, vector, n_samples)
        implicit none
        real(kind=4), allocatable   :: uniq_vals(:)
        real(kind=4), intent(inout) :: vector(n_samples)
        integer(kind=4), intent(in) :: n_samples
        integer(kind=4) :: n_unique, n, idx
        include "./include/common/collect_unique_values/inc_collect_unique_values_detail.f90"
    end subroutine collect_unique_values_r4
    include "./include/common/collect_unique_values/inc_collect_unique_values.f90"


    !> A function to count the unique values of the sorted one-dim array
    !! \return returns number of unique values for one-dim array
    !! \param vector ascending sorted array to be searched
    !! \param n_samples number of samples 
    function count_unique_r4(vector, n_samples)
        implicit none
        real(kind=4), intent(in)    :: vector(n_samples)
        integer(kind=4), intent(in) :: n_samples
        integer(kind=4)             :: count_unique_r4

        integer(kind=4) :: i, factor, tmp_count
        include "./include/common/count_unique/inc_count_unique_detail.f90"
        count_unique_r4 = tmp_count
    end function count_unique_r4
    include "./include/common/count_unique/inc_count_unique.f90"


    !> An interface to find rightmost value index from 1-dim array less equal input value
    !! \return returns rightmost value index from 1-dim array less equal input value
    !! \param vector ascending sorted array to be searched
    !! \param n_samples size of input vector
    !! \param value value
    function find_le_r8(vector, n_samples, value)
        implicit none
        integer(kind=8)             :: find_le_r8
        real(kind=8), intent(in)    :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples
        real(kind=8), intent(in)    :: value
        integer(kind=8)             :: idx
        find_le_r8 = binary_search_right(vector, n_samples, value)-1_8
    end function find_le_r8


    !> An interface to find rightmost value index from 1-dim array less than input value
    !! \return returns rightmost value index from 1-dim array less than input value
    !! \param vector ascending sorted array to be searched
    !! \param n_samples size of input vector
    !! \param value value
    function find_lt_r8(vector, n_samples, value)
        implicit none
        integer(kind=8) :: find_lt_r8
        real(kind=8), intent(in)    :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples
        real(kind=8), intent(in)    :: value
        integer(kind=8)             :: idx
        find_lt_r8 = binary_search_left(vector, n_samples, value)-1_8
    end function find_lt_r8


    !> An interface to find leftmost value index from 1-dim array greater equal input value
    !! \return returns leftmost value index from 1-dim array greater equal input value
    !! \param vector ascending sorted array to be searched
    !! \param n_samples size of input vector
    !! \param value value
    function find_ge_r8(vector, n_samples, value)
        implicit none
        integer(kind=8)             :: find_ge_r8
        real(kind=8), intent(in)    :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples
        real(kind=8), intent(in)    :: value
        integer(kind=8)             :: idx
        find_ge_r8 = binary_search_left(vector, n_samples, value)
    end function find_ge_r8


    !> An interface to find leftmost value index from 1-dim array greater than input value
    !! \return returns leftmost value index from 1-dim array greater than input value
    !! \param vector ascending sorted array to be searched
    !! \param n_samples size of input vector
    !! \param value value
    function find_gt_r8(vector, n_samples, value)
        implicit none
        integer(kind=8)             :: find_gt_r8
        real(kind=8), intent(in)    :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples
        real(kind=8), intent(in)    :: value
        integer(kind=8)             :: idx
        find_gt_r8 = binary_search_right(vector, n_samples, value)
    end function find_gt_r8


    !> A subroutine to find the location and absolute values of the absolute maximum from the non-diagonal elements of a symmetric matrix.
    !! \return returns the absolute maximum value and its location
    !! \param loc off-diagonal location of the absolute maximum value
    !! \param val absolute maximum value itself
    !! \param matrix input 2-dim symmetric array
    !! \param n_dim rank of symmetric matrix
    subroutine get_abs_maxloc_symmat_r4(loc, val, matrix, n_dim)
        implicit none
        integer(kind=4), intent(inout) :: loc(2)
        real(kind=4), intent(inout)    :: val
        real(kind=4), intent(in)       :: matrix(n_dim, n_dim)
        integer(kind=4), intent(in)    :: n_dim
        integer(kind=4)                :: i, j
        real(kind=4)                   :: tmp_absmax, tmp_absval
        include "./include/common/get_abs_maxloc/inc_get_abs_maxloc_symmat_detail.f90"
    end subroutine get_abs_maxloc_symmat_r4
    include "./include/common/get_abs_maxloc/inc_get_abs_maxloc_symmat.f90"


    !> A subroutine to get difference of an input array.
    !! \param indices_diff difference array
    !! \param indices input index array 
    !! \param n_indices size of 'indices'
    subroutine get_indices_diff_i8(indices_diff, indices, n_indices)
        integer(kind=8), intent(inout) :: indices_diff(n_indices)
        integer(kind=8), intent(in)    :: indices(n_indices)
        integer(kind=8), intent(in)    :: n_indices

        integer(kind=8) :: n_idx
        integer(kind=8) :: idx, start_idx
        integer(kind=8) :: idx_unroll_size
        integer(kind=8) :: n_idx_unroll, n_idx_remain

        n_idx = n_indices-1
        idx_unroll_size=1
        n_idx_remain = mod(n_indices, idx_unroll_size)
        n_idx_unroll = n_indices - n_idx_remain

        start_idx=1
        indices_diff(1) = indices(1)-start_idx

        do idx=2, n_idx_unroll, 1
            indices_diff(idx) = indices(idx)-indices(idx-1)
        end do
    end subroutine get_indices_diff_i8


    !> A function to get digit of input integer.
    !! \return returns digit of input integer
    !! \param num input integer
    pure function get_int_digit_i4(num) result(num_digit)
        implicit none
        integer(kind=4),intent(in) :: num
        integer(kind=4) :: num_digit
        include "./include/common/get_int_digit/inc_get_int_digit_detail.f90"
    end function get_int_digit_i4
    include "./include/common/get_int_digit/inc_get_int_digit.f90"


    !> A subroutine to initialize allocated square matrix to identity matrix.
    !! \return returns identity matrix
    !! \param matrix already allocated square matrix
    !! \param n_dim the rank of the input matrix 
    subroutine identity_r4(matrix, n_dim)
        implicit none
        real(kind=4), intent(inout) :: matrix(n_dim,n_dim)
        integer(kind=4), intent(in) :: n_dim
        integer(kind=4) :: i
        real(kind=4)    :: val_1, val_0
        val_1 = 1.0; val_0 = 0.0
        include "./include/common/identity/inc_identity_detail.f90"
    end subroutine identity_r4
    include "./include/common/identity/inc_identity.f90"


    !> A subroutine to deallocate already allocated 1-dim array. If not allocated, nothing to do.
    !! \param vector input vector to be deallocated
    subroutine ifdealloc_vec_r4(vector)
        real(kind=4), allocatable, intent(inout) :: vector(:)
        if (allocated(vector)) deallocate(vector)
    end subroutine ifdealloc_vec_r4
    include "./include/common/ifdealloc/inc_ifdealloc_vec.f90"


    !> A subroutine to deallocate already allocated 2-dim array. If not allocated, nothing to do.
    !! \param vector input matirx to be deallocated
    subroutine ifdealloc_mat_r4(matrix)
        real(kind=4), allocatable, intent(inout) :: matrix(:,:)
        if (allocated(matrix)) deallocate(matrix)
    end subroutine ifdealloc_mat_r4
    include "./include/common/ifdealloc/inc_ifdealloc_mat.f90"


    !> A function to check if the input array is already sorted.
    !! \return returns wheter one-dim array sorted or not
    !! \param vector input one-dim vector to be checked sorted or not
    !! \param n_samples the size of input vector
    !! \param ascending OPTIONAL, whether the vectors are in ascending order or not
    function is_sorted_r4(vector, n_samples, ascending)
        implicit none
        real(kind=4), intent(in)    :: vector(n_samples)
        integer(kind=4), intent(in) :: n_samples
        logical(kind=4), optional   :: ascending
        logical(kind=4)             :: is_sorted_r4

        logical(kind=4) :: ascending_opt, tmp_result
        integer(kind=4) :: i

        include "./include/common/is_sorted/inc_is_sorted_detail.f90"
        is_sorted_r4 = tmp_result
    end function is_sorted_r4
    include "./include/common/is_sorted/inc_is_sorted.f90"


    !> A function to return the position of the value less equal the input value from the ascending sorted 1-dim array.
    !! \return returns index of the value less equal the input value
    !! \param vector ascending sorted array to be searched
    !! \param n_samples number of samples
    !! \param value value
    function linear_search_r4(vector, n_samples, value)
        implicit none
        integer(kind=4)             :: linear_search_r4
        real(kind=4), intent(in)    :: vector(n_samples)
        integer(kind=4), intent(in) :: n_samples
        real(kind=4), intent(in)    :: value
        integer(kind=4) :: i, tmp
        include "./include/common/linear_search/inc_linear_search_detail.f90"
        linear_search_r4 = tmp
    end function linear_search_r4
    include "./include/common/linear_search/inc_linear_search.f90"


    !> A function to get most left non-zero bit, except sign.
    !! \return returns the left most bit position
    !! \param val input integer value
    function most_left_bit_position_i4(val)
        implicit none
        integer(kind=4), intent(in) :: val
        integer(kind=4) :: most_left_bit_position_i4
        integer(kind=4) :: i, i_left, tmp
        i_left = 30
        include "./include/common/most_left_bit_position/inc_most_left_bit_position_detail.f90"
        most_left_bit_position_i4 = tmp
    end function most_left_bit_position_i4
    include "./include/common/most_left_bit_position/inc_most_left_bit_position.f90"


    !> A function to convert number to character.
    !! \param num input number
    function num2char_r4(num)
        character(:), allocatable   :: num2char_r4
        real(kind=4), intent(in) :: num
        integer(kind=4) :: num_digit

        allocate(character(10)::num2char_r4)
        write (num2char_r4, '(E10.3e2)') num
    end function num2char_r4
    include "./include/common/num2char/inc_num2char.f90"


    !> A subroutine to print progress bar. 
    !> **ORIGINAL**:  https://nkmrtkhd.blogspot.com/2009/07/fortran.html 
    !> Subroutines of other data types (progress_bar_int64) are stored in './common/include/progress_bar/'. 
    !! \return returns none
    !! \param loop_index current loop index
    !! \param loop_max_index maximum loop index
    !! \param step_size step size to update the progress bar
    subroutine progress_bar_i4(loop_index, loop_max_index, step_size)
        implicit none
        integer(kind=4), intent(in) :: loop_index,loop_max_index,step_size
        integer(kind=4), parameter  :: ndigit=100
        integer(kind=4)             :: j, step_size_mod
        step_size_mod = maxval((/step_size, 1/))
        include "./include/common/progress_bar/inc_progress_bar_detail.f90"
    end subroutine progress_bar_i4
    include "./include/common/progress_bar/inc_progress_bar.f90"


    !> A subroutine to read binary file dumped by 'read2bin_1d'
    !! \return returns data as vector
    !! \param file_name file name to be read
    !! \param vector read data
    subroutine read_bin_1d_r4(file_name, vector)
        implicit none
        character(len=256), intent(in) :: file_name
        real(kind=4), allocatable      :: vector(:)
        integer(kind=4)                :: n_samples
        integer(kind=4)                :: bit_size, bit_size_file
        character(len=1)               :: dtype, dtype_set
        integer(kind=4)                :: nd, nd_file
        nd = 1
        dtype_set = "r"
        bit_size = 32
        include "./include/common/read_bin_1d/inc_read_bin_1d_detail.f90"
    end subroutine read_bin_1d_r4
    include "./include/common/read_bin_1d/inc_read_bin_1d.f90"


    !> A subroutine to read binary file dumped by 'read2bin_2d'
    !! \return returns data as matrix
    !! \param file_name file name to be read
    !! \param matrix read data
    subroutine read_bin_2d_r4(file_name, matrix)
        implicit none
        real(kind=4), allocatable, intent(inout) :: matrix(:,:)
        integer(kind=4)                          :: n_samples, n_columns
        integer(kind=4)                          :: bit_size, bit_size_file
        character(len=1)                         :: dtype, dtype_set
        character(len=256), intent(in)           :: file_name
        integer(kind=4)                          :: nd, nd_file
        nd = 2
        dtype_set = "r"
        bit_size = 32
        include "./include/common/read_bin_2d/inc_read_bin_2d_detail.f90"
    end subroutine read_bin_2d_r4
    include "./include/common/read_bin_2d/inc_read_bin_2d.f90"


    !> A subroutine to read a file with only one column and dump it as binary file. \n
    !> **NOTICE**: Can't handle multiple data types at the same time.  \n
    !> Use 'read_bin' subroutine to read the dumped binary file.  \n
    !> The type of the output data (32bit or 64bit) is determined by the type of the argument.   \n
    !> Subroutines of other data types (read2bin_2d_64bit) are stored in './common/include/read2bin/'. \n
    !! \return returns none
    !! \param input_file_name input file name
    !! \param output_file_name output file name
    !! \param n_samples number of lines in the input file(header not included). 'n_samples' should be less than or equal to the number of lines in the input file.
    !! \param skip_header whether to skip the header. If .true., 
    !! \param input_dtype input data type, 'r' or 'i' ('real' or 'integer')
    !! \param output_dtype output data type, 'r' or 'i' ('real' or 'integer')
    subroutine read2bin_1d_32bit(input_file_name, output_file_name, n_samples, skip_header, input_dtype, output_dtype)
        implicit none
        character(len=256), intent(in)  :: input_file_name, output_file_name
        integer(kind=4), intent(in)     :: n_samples
        logical(kind=4), intent(in)     :: skip_header
        character(len=1), intent(in)    :: input_dtype, output_dtype
        integer(kind=4)                 :: i, i_tot
        real(kind=4), allocatable       :: vector_r(:)
        integer(kind=4), allocatable    :: vector_i(:)
        integer(kind=4)                 :: tmp_i
        real(kind=4)                    :: tmp_r
        integer(kind=4)                 :: bit_size
        integer(kind=4)                 :: nd
        character(len=1)                :: dtype_r, dtype_i
        nd = 1
        bit_size = 32
        include "./include/common/read2bin_1d/inc_read2bin_1d_detail.f90"
    end subroutine read2bin_1d_32bit
    include "./include/common/read2bin_1d/inc_read2bin_1d.f90"


    !> A subroutine to read a file with multiple columns and dump it as binary file. \n
    !> **NOTICE**: Can't handle multiple data types at the same time.  \n
    !> Use 'read_bin' subroutine to read the dumped binary file.  \n
    !> The type of the output data (32bit or 64bit) is determined by the type of the argument.   \n
    !> Subroutines of other data types (read2bin_2d_64bit) are stored in './common/include/read2bin/'. \n
    !! \return returns none
    !! \param input_file_name input file name
    !! \param output_file_name output file name
    !! \param n_samples number of lines in the input file(header not included). 'n_samples' should be less than or equal to the number of lines in the input file.
    !! \param n_columns number of columns in the input file. 'n_columns' must be equal to the number of lines in the input file.
    !! \param skip_header whether to skip the header. If .true., 
    !! \param input_dtype input data type, 'r' or 'i' ('real' or 'integer')
    !! \param output_dtype output data type, 'r' or 'i' ('real' or 'integer')
    subroutine read2bin_2d_32bit(input_file_name, output_file_name, &
        n_samples, n_columns, skip_header, input_dtype, output_dtype)
        implicit none
        character(len=256), intent(in)  :: input_file_name, output_file_name
        integer(kind=4), intent(in)     :: n_samples, n_columns
        logical(kind=4), intent(in)     :: skip_header
        character(len=1), intent(in)    :: input_dtype, output_dtype
        integer(kind=4)                 :: i, i_tot
        real(kind=4), allocatable       :: matrix_r(:,:)
        integer(kind=4), allocatable    :: matrix_i(:,:)
        integer(kind=4), allocatable    :: tmp_i(:)
        real(kind=4), allocatable       :: tmp_r(:)
        integer(kind=4)                 :: bit_size
        integer(kind=4)                 :: nd
        nd = 2 ! #Dimension
        bit_size = 32 ! Bit Size
        include "./include/common/read2bin_2d/inc_read2bin_2d_detail.f90"
    end subroutine read2bin_2d_32bit
    include "./include/common/read2bin_2d/inc_read2bin_2d.f90"


    !> An subroutine to calculate a matrix from two vectors.
    !> @todo move to 'linalg'
    !! \return returns calculated matrix
    !! \param vector1 input vector
    !! \param vector2 input vector
    !! \param matrix calculated matrix
    !! \param n_dim1 size of 'vector1'
    !! \param n_dim2 size of 'vector2'
    subroutine vv2mat_r8(vector1, vector2, matrix, n_dim1, n_dim2)
        implicit none
        real(kind=8), intent(in)    :: vector1(n_dim1)
        real(kind=8), intent(in)    :: vector2(n_dim2)
        real(kind=8), intent(inout) :: matrix(n_dim1, n_dim2)
        integer(kind=8), intent(in) :: n_dim1, n_dim2

        integer(kind=8) :: i, j
        real(kind=8) :: tmp

        do j=1, n_dim2, 1
            tmp = vector2(j)
            do i=1, n_dim1, 1
                matrix(i,j) = tmp * vector1(i)
            end do
        end do
    end subroutine vv2mat_r8


    !> An subroutine to calculate a scalar value from one square matrix and one vector.
    !> @todo move to 'linalg'
    !! \return returns calculated scalar value
    !! \param vector input vector
    !! \param matrix input matrix
    !! \param n_dim size of 'vector'
    function vmv_r8(vector, matrix, n_dim)
        implicit none
        real(kind=8)                :: vmv_r8
        real(kind=8), intent(in)    :: vector(n_dim)
        real(kind=8), intent(in)    :: matrix(n_dim, n_dim)
        integer(kind=8), intent(in) :: n_dim

        real(kind=8), ALLOCATABLE :: tmp_vector(:)
        real(kind=8)              :: tmp_sum
        integer(kind=8) :: i, j

        allocate(tmp_vector(n_dim))

        tmp_vector = 0d0
        do j=1, n_dim, 1
            tmp_sum = 0d0
            do i=1, n_dim, 1
                tmp_sum = tmp_sum + matrix(i,j) * vector(i)
            end do
            tmp_vector(j) = tmp_sum
        end do

        vmv_r8 = 0d0
        do j=1, n_dim, 1
            vmv_r8 = vmv_r8 + tmp_vector(j) * vector(j)
        end do
    end function vmv_r8

end module mod_common