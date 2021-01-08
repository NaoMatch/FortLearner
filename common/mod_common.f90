!> A module for common functions.
module mod_common
    use mod_const
    implicit none

    !> Interface to call identity_real32, identity_real64, identity_int32, identity_int64
    interface identity
        module procedure identity_real32
        module procedure identity_real64 ! place in "./include/common_identity/inc_common_identity.f90"
        module procedure identity_int32  ! place in "./include/common_identity/inc_common_identity.f90"
        module procedure identity_int64  ! place in "./include/common_identity/inc_common_identity.f90"
    end interface identity

    !> Interface to call read_bin_1d_real32, read_bin_1d_real64, read_bin_1d_int32, read_bin_1d_int64
    interface read_bin_1d
        module procedure read_bin_1d_real32
        module procedure read_bin_1d_real64
        module procedure read_bin_1d_int32
        module procedure read_bin_1d_int64
    end interface read_bin_1d

    !> Interface to call read_bin_2d_real32, read_bin_2d_real64, read_bin_2d_int32, read_bin_2d_int64
    interface read_bin_2d
        module procedure read_bin_2d_real32
        module procedure read_bin_2d_real64
        module procedure read_bin_2d_int32
        module procedure read_bin_2d_int64
    end interface read_bin_2d

    !> Interface to call read2bin_2d_32bit, read2bin_2d_64bit
    interface read2bin_2d
        module procedure read2bin_2d_32bit
        module procedure read2bin_2d_64bit ! place in "./include/common_read2bin_2d/inc_read2bin_2d.f90"
    end interface read2bin_2d

    !> Interface to call read2bin_1d_32bit, read2bin_1d_64bit
    interface read2bin_1d
        module procedure read2bin_1d_32bit
        module procedure read2bin_1d_64bit ! place in "./include/common_read2bin_1d/inc_read2bin_1d.f90"
    end interface read2bin_1d

    !> Interface to call progress_bar_int32, progress_bar_int64
    interface progress_bar 
        module procedure progress_bar_int32
        module procedure progress_bar_int64 ! place in "./include/common_progress_bar/inc_progress_bar.f90"
    end interface progress_bar

    !> Interface to call is_sorted_real32, is_sorted_real64, is_sorted_int32, is_sorted_int64
    interface is_sorted
        module procedure is_sorted_real32
        module procedure is_sorted_real64
        module procedure is_sorted_int32
        module procedure is_sorted_int64
    end interface is_sorted

    !> Interface to call count_unique_real32, count_unique_real64, count_unique_int32, count_unique_int64
    interface count_unique
        module procedure count_unique_real32
        module procedure count_unique_real64
        module procedure count_unique_int32
        module procedure count_unique_int64
    end interface count_unique

    !> Interface to call most_left_bit_position_int32, most_left_bit_position_int64
    interface most_left_bit_position 
        module procedure most_left_bit_position_int32
        module procedure most_left_bit_position_int64
    end interface most_left_bit_position

    !> Interface to call get_abs_maxloc_real32, get_abs_maxloc_real64, get_abs_maxloc_int32, get_abs_maxloc_int64
    interface get_abs_maxloc
        module procedure get_abs_maxloc_sqmat_real32
        module procedure get_abs_maxloc_sqmat_real64
        module procedure get_abs_maxloc_sqmat_int32
        module procedure get_abs_maxloc_sqmat_int64
    end interface get_abs_maxloc

    ! interface collect_data_points
    !     module procedure collect_data_points_r8
    ! end interface collect_data_points

    interface collect_unique_values
        module procedure collect_unique_values_r8
        module procedure collect_unique_values_i8
    end interface collect_unique_values

    interface binary_search_left
        module procedure binary_search_left_r8
    end interface binary_search_left

    interface binary_search_right
        module procedure binary_search_right_r8
    end interface binary_search_right

    interface linear_search
        module procedure linear_search_r8
    end interface linear_search

contains

    !> A subroutine to collect unique values from sorted vector
    !! \return returns unique values
    !! \param uniq_v unique values collected from 'vector'
    !! \param vector sorted vector
    !! \param n_samples number of samples 
    subroutine collect_unique_values_r8(uniq_v, vector, n_samples)
        implicit none
        real(kind=8), allocatable   :: uniq_v(:)
        real(kind=8), intent(inout) :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples

        integer(kind=8) :: n_unique, n, idx

        if (allocated(uniq_v)) deallocate(uniq_v)

        idx = 2
        n_unique = count_unique(vector, n_samples)
        allocate(uniq_v(n_unique))
        uniq_v(1) = vector(1)
        do n=2, n_samples
            if ( vector(n-1) .ne. vector(n) ) then
                uniq_v(idx) = vector(n)
                idx = idx + 1
            end if
        end do
    end subroutine collect_unique_values_r8
    include "./include/common_collect_unique_values/inc_collect_unique_values.f90"


    !> A function to linear search.
    !> 'vector' must be sorted by ascending order.
    !! \return returns index
    !! \param vector values to be searched
    !! \param n_samples number of samples
    !! \param value value
    function linear_search_r8(vector, n_samples, value)
        implicit none
        integer(kind=8) :: linear_search_r8
        real(kind=8), intent(in)    :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples
        real(kind=8), intent(in)    :: value

        integer(kind=8) :: i

        do i=1, n_samples
            if ( value .le. vector(i) ) then
                linear_search_r8 = i
                return
            end if
        end do
    end function linear_search_r8


    !> A function to binary search from left.
    !> 'vector' must be sorted by ascending order.
    !! \return returns index
    !! \param vector values to be searched
    !! \param n_samples number of samples
    !! \param value value
    function binary_search_left_r8(vector, n_samples, value)
        implicit none
        integer(kind=8) :: binary_search_left_r8
        real(kind=8), intent(in)    :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples
        real(kind=8), intent(in)    :: value

        integer(kind=8) :: lo, hi, mid
        lo = 1
        hi = n_samples

        do while (lo .lt. hi)
            mid = (lo+hi)/2
            if ( vector(mid) .lt. value ) then
                lo = mid + 1
            else
                hi = mid
            end if
        end do
        binary_search_left_r8 = lo
    end function binary_search_left_r8


    !> A function to binary search from right.
    !> 'vector' must be sorted by ascending order.
    !! \return returns index
    !! \param vector values to be searched
    !! \param n_samples number of samples
    !! \param value value
    function binary_search_right_r8(array, n_samples, value)
        implicit none
        integer(kind=8) :: binary_search_right_r8
        real(kind=8), intent(in)    :: array(n_samples)
        integer(kind=8), intent(in) :: n_samples
        real(kind=8), intent(in)    :: value

        integer(kind=8) :: lo, hi, mid
        lo = 1
        hi = n_samples

        do while (lo .lt. hi)
            mid = (lo+hi)/2
            if ( array(mid) .gt. value ) then
                hi = mid
            else
                lo = mid + 1
            end if
        end do
        binary_search_right_r8 = lo
    end function binary_search_right_r8


    !> A function to get the leftmost bit position.
    !! \return returns the left most bit position
    !! \param val input integer value
    function most_left_bit_position_int32(val)
        implicit none
        integer(kind=4), intent(in) :: val
        integer(kind=4) :: most_left_bit_position_int32
        integer(kind=4) :: i, i_left, tmp
        i_left = 30
        include "./include/common_most_left_bit_position/inc_most_left_bit_position_detail.f90"
        most_left_bit_position_int32 = tmp
    end function most_left_bit_position_int32
    include "./include/common_most_left_bit_position/inc_most_left_bit_position.f90"


    !> A function to count the unique values of the sorted 1-dim array
    !! \return returns number of unique values for 1-dim array
    !! \param vector a sorted input 1-dim array
    !! \param num the size of input vector
    function count_unique_real32(vector, num)
        implicit none
        real(kind=4), intent(in)    :: vector(num)
        integer(kind=4), intent(in) :: num
        integer(kind=4)             :: count_unique_real32

        integer(kind=4) :: i, factor
        integer(kind=4) :: tmp_count
        if (num .eq. 1) then
            count_unique_real32 = 0
            return
        end if
        tmp_count = 1
        include "./include/common_count_unique/inc_count_unique_detail.f90"
        count_unique_real32 = tmp_count
    end function count_unique_real32
    include "./include/common_count_unique/inc_count_unique.f90"


    !> A function to check if the 1-dim array is sorted or not
    !! \return returns wheter 1-dim array sorted or not
    !! \param vector input 1-dim vector to be checked sorted or not
    !! \param num the size of input vector
    !! \param ascending OPTIONAL, whether the vectors are in ascending order or not
    function is_sorted_real32(vector, num, ascending)
        implicit none
        real(kind=4), intent(in)    :: vector(num)
        integer(kind=4), intent(in) :: num
        logical(kind=4), optional   :: ascending
        logical(kind=4)             :: is_sorted_real32

        logical(kind=4) :: ascending_opt, tmp_result
        integer(kind=4) :: i

        include "./include/common_is_sorted/is_sorted_detail.f90"
        is_sorted_real32 = tmp_result
    end function is_sorted_real32
    include "./include/common_is_sorted/is_sorted.f90"


    !> A subroutine to initialize an n-dimensional square matrix to a unit matrix.
    !> Subroutines of other data types (identity_real64, identity_int32, identity_int64) are stored in './common/include/identity/'. \n
    !> Note that 'integer' and 'real' kind are unified.
    !! \return returns identity matrix
    !! \param matrix already allocated n-dimensional square matrix
    !! \param n_dim the order of the input matrix 
    subroutine identity_real32(matrix, n_dim)
        implicit none
        real(kind=4), intent(inout) :: matrix(n_dim,n_dim)
        integer(kind=4), intent(in) :: n_dim
        integer(kind=4) :: i
        real(kind=4)    :: val_1, val_0
        val_1 = 1.0; val_0 = 0.0
        include "./include/common_identity/inc_common_identity_detail.f90"
    end subroutine identity_real32
    include "./include/common_identity/inc_common_identity.f90"


    !> A subroutine to read binary file dumped by 'read2bin_1d'.
    !! \return returns data as vector
    !! \param file_name file name to be read
    !! \param vector read data
    subroutine read_bin_1d_real32(file_name, vector)
        implicit none
        character(len=256), intent(in) :: file_name
        real(kind=4), allocatable      :: vector(:)
        integer(kind=4)                :: n_rows
        integer(kind=4)                :: bit_size, bit_size_file
        character(len=1)               :: dtype, dtype_set
        integer(kind=4)                :: nd, nd_file
        nd = 1
        dtype_set = "r"
        bit_size = 32
        include "./include/common_read_bin_1d/inc_read_bin_1d_detail.f90"
    end subroutine read_bin_1d_real32
    include "./include/common_read_bin_1d/inc_read_bin_1d.f90"


    !> A subroutine to read binary file dumped by 'read2bin_2d'.
    !! \return returns data as matrix
    !! \param file_name file name to be read
    !! \param matrix read data
    subroutine read_bin_2d_real32(file_name, matrix)
        implicit none
        real(kind=4), allocatable, intent(inout) :: matrix(:,:)
        integer(kind=4)                          :: n_rows, n_cols
        integer(kind=4)                          :: bit_size, bit_size_file
        character(len=1)                         :: dtype, dtype_set
        character(len=256), intent(in)           :: file_name
        integer(kind=4)                          :: nd, nd_file
        nd = 2
        dtype_set = "r"
        bit_size = 32
        include "./include/common_read_bin_2d/inc_common_read_bin_2d_detail.f90"
    end subroutine read_bin_2d_real32
    include "./include/common_read_bin_2d/inc_common_read_bin_2d.f90"


    !> A subroutine to read a CSV file and dump it as a binary file. \n
    !> **NOTICE**: Can't handle multiple data types at the same time.  \n
    !> Use 'read_bin' subroutine to read the dumped binary file.  \n
    !> The type of the output data (32bit or 64bit) is determined by the type of the argument.   \n
    !> Subroutines of other data types (read2bin_2d_64bit) are stored in './common/include/read2bin/'. \n
    !! \return returns none
    !! \param input_file_name input file name
    !! \param output_file_name output file name
    !! \param n_rows number of lines in the input file(header not included). 'n_rows' should be less than or equal to the number of lines in the input file.
    !! \param n_cols number of columns in the input file. 'n_cols' must be equal to the number of lines in the input file.
    !! \param skip_header whether to skip the header. If .true., 
    !! \param input_dtype input data type, 'r' or 'i' ('real' or 'integer')
    !! \param output_dtype output data type, 'r' or 'i' ('real' or 'integer')
    subroutine read2bin_2d_32bit(input_file_name, output_file_name, &
        n_rows, n_cols, skip_header, input_dtype, output_dtype)
        implicit none
        character(len=256), intent(in)  :: input_file_name, output_file_name
        integer(kind=4), intent(in)     :: n_rows, n_cols
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
        include "./include/common_read2bin_2d/inc_read2bin_2d_detail.f90"
    end subroutine read2bin_2d_32bit
    include "./include/common_read2bin_2d/inc_read2bin_2d.f90"


    !> A subroutine to read a CSV file and dump it as a binary file. \n
    !> **NOTICE**: Can't handle multiple data types at the same time.  \n
    !> Use 'read_bin' subroutine to read the dumped binary file.  \n
    !> The type of the output data (32bit or 64bit) is determined by the type of the argument.   \n
    !> Subroutines of other data types (read2bin_2d_64bit) are stored in './common/include/read2bin/'. \n
    !! \return returns none
    !! \param input_file_name input file name
    !! \param output_file_name output file name
    !! \param n_rows number of lines in the input file(header not included). 'n_rows' should be less than or equal to the number of lines in the input file.
    !! \param skip_header whether to skip the header. If .true., 
    !! \param input_dtype input data type, 'r' or 'i' ('real' or 'integer')
    !! \param output_dtype output data type, 'r' or 'i' ('real' or 'integer')
    subroutine read2bin_1d_32bit(input_file_name, output_file_name, n_rows, skip_header, input_dtype, output_dtype)
        implicit none
        character(len=256), intent(in)  :: input_file_name, output_file_name
        integer(kind=4), intent(in)     :: n_rows
        logical(kind=4), intent(in)     :: skip_header
        character(len=1), intent(in)    :: input_dtype, output_dtype
        integer(kind=4)                 :: i, i_tot
        real(kind=4), allocatable       :: vector_r(:)
        integer(kind=4), allocatable    :: vector_i(:)
        integer(kind=4)                 :: tmp_i
        real(kind=4)                    :: tmp_r
        integer(kind=4)                 :: bit_size
        integer(kind=4)                 :: nd
        nd = 1
        bit_size = 32
        include "./include/common_read2bin_1d/inc_read2bin_1d_detail.f90"
    end subroutine read2bin_1d_32bit
    include "./include/common_read2bin_1d/inc_read2bin_1d.f90"


    !> A subroutine to display a progress bar. \n
    !> **ORIGINAL**:  https://nkmrtkhd.blogspot.com/2009/07/fortran.html \n
    !> Subroutines of other data types (progress_bar_int64) are stored in './common/include/progress_bar/'. \n
    !! \return returns none
    !! \param loop_index current loop index
    !! \param loop_max_index maximum loop index
    !! \param step_size step size to update the progress bar
    subroutine progress_bar_int32(loop_index, loop_max_index, step_size)
        implicit none
        integer(kind=4), intent(in) :: loop_index,loop_max_index,step_size
        integer(kind=4), parameter  :: ndigit=100
        integer(kind=4)             :: j, step_size_mod
        step_size_mod = maxval((/step_size, 1/))
        include "./include/common_progress_bar/inc_progress_bar_detail.f90"
    end subroutine progress_bar_int32
    include "./include/common_progress_bar/inc_progress_bar.f90"


    !> A subroutine to get the off-diagonal absolute maximum value and its location from a symmetric matrix at the same time. \n
    !! \return returns the absolute maximum value and its location
    !! \param loc absolute maximum value off-diagonal location
    !! \param val absolute maximum value itself
    !! \param matrix input 2-dim symmetric array
    !! \param n_dim rank of symmetric matrix
    subroutine get_abs_maxloc_sqmat_real32(loc, val, matrix, n_dim)
        implicit none
        integer(kind=4), intent(inout) :: loc(2)
        real(kind=4), intent(inout)    :: val
        real(kind=4), intent(in)       :: matrix(n_dim, n_dim)
        integer(kind=4), intent(in)    :: n_dim
        integer(kind=4)                :: i, j
        real(kind=4)                   :: tmp_absmax, tmp_absval
        include "./include/common_get_abs_maxloc/inc_get_abs_maxloc_sqmat_detail.f90"
    end subroutine get_abs_maxloc_sqmat_real32
    include "./include/common_get_abs_maxloc/inc_get_abs_maxloc_sqmat.f90"


end module mod_common