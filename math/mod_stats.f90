!> A module to calculate simple statistics.
module mod_stats
    use mod_const
    use mod_sort
    implicit none

    !> Interface to call sum_up_left_real32, sum_up_left_with_indices_real32
    interface sum_up_left
        module procedure sum_up_left_real32
        module procedure sum_up_left_with_indices_real32
    end interface sum_up_left

    !> Interface to call get_minmax_real32, get_minmax_real64, get_minmax_int32, get_minmax_int64
    interface get_minmax
        module procedure get_minmax_real32
        module procedure get_minmax_real64
        module procedure get_minmax_int32
        module procedure get_minmax_int64
    end interface get_minmax

    !> Interface to call mean_value_of_vector_real32, mean_value_of_vector_real64, mean_value_of_vector_int32, mean_value_of_vector_int64
    interface mean  
        module procedure mean_value_of_vector_real32
        module procedure mean_value_of_vector_real64     ! place in "./include/stats_mean_value_of_vector/inc_mean_value_of_vector.f90"
        module procedure mean_value_of_vector_int32      ! place in "./include/stats_mean_value_of_vector/inc_mean_value_of_vector.f90"
        module procedure mean_value_of_vector_int64      ! place in "./include/stats_mean_value_of_vector/inc_mean_value_of_vector.f90"

        module procedure mean_values_of_matrix_real32
        module procedure mean_values_of_matrix_real64     ! place in "./include/stats_mean_values_of_matrix/inc_mean_values_of_matrix.f90"
        module procedure mean_values_of_matrix_int32      ! place in "./include/stats_mean_values_of_matrix/inc_mean_values_of_matrix.f90"
        module procedure mean_values_of_matrix_int64      ! place in "./include/stats_mean_values_of_matrix/inc_mean_values_of_matrix.f90"
    end interface mean

    !> Interface to call variance_value_of_vector_real32, variance_value_of_vector_real64, variance_value_of_vector_int32, variance_value_of_vector_int64
    interface variance
        module procedure variance_value_of_vector_real32
        module procedure variance_value_of_vector_real64     ! place in "./include/stats_variance_value_of_vector/inc_variance_value_of_vector.f90"
        module procedure variance_value_of_vector_int32      ! place in "./include/stats_variance_value_of_vector/inc_variance_value_of_vector.f90"
        module procedure variance_value_of_vector_int64      ! place in "./include/stats_variance_value_of_vector/inc_variance_value_of_vector.f90"

        module procedure variance_values_of_matrix_real32
        module procedure variance_values_of_matrix_real64     ! place in "./include/stats_variance_values_of_matrix/inc_variance_values_of_matrix.f90"
        module procedure variance_values_of_matrix_int32      ! place in "./include/stats_variance_values_of_matrix/inc_variance_values_of_matrix.f90"
        module procedure variance_values_of_matrix_int64      ! place in "./include/stats_variance_values_of_matrix/inc_variance_values_of_matrix.f90"
    end interface variance

    !> Interface to call covariance_value_of_vectors_real32, covariance_value_of_vectors_real64, covariance_value_of_vectors_int32, covariance_value_of_vectors_int64
    interface covariance
        module procedure covariance_value_of_vectors_real32
        module procedure covariance_value_of_vectors_real64     ! place in "./include/stats_covariance_value_of_vectors/inc_covariance_value_of_vectors.f90"
        module procedure covariance_value_of_vectors_int32      ! place in "./include/stats_covariance_value_of_vectors/inc_covariance_value_of_vectors.f90"
        module procedure covariance_value_of_vectors_int64      ! place in "./include/stats_covariance_value_of_vectors/inc_covariance_value_of_vectors.f90"
    end interface covariance

    interface covariance_matrix
        module procedure covariance_matrix_of_matrix_real32
        module procedure covariance_matrix_of_matrix_real64     ! place in "./include/stats_covariance_matrix_of_matrix/inc_covariance_matrix_of_matrix.f90"
        module procedure covariance_matrix_of_matrix_int32      ! place in "./include/stats_covariance_matrix_of_matrix/inc_covariance_matrix_of_matrix.f90"
        module procedure covariance_matrix_of_matrix_int64      ! place in "./include/stats_covariance_matrix_of_matrix/inc_covariance_matrix_of_matrix.f90"        
    end interface covariance_matrix

    interface groupby_sum
        module procedure groupby_sum_r8
        module procedure groupby_sum_fast_r8
    end interface groupby_sum

    interface groupby_sq_sum
        module procedure groupby_sq_sum_r8
    end interface groupby_sq_sum

    interface groupby_count
        module procedure groupby_count_r8
        module procedure groupby_count_i8
        module procedure groupby_count_fast_r8
    end interface groupby_count

contains

    !> A subroutine to the sum of values less than or equal the threshold from a 1-dim array. \n
    !! \return returns the sum of values less than or equal the threshold.
    !! \param sum_left value that is less than or equal to the threshold value in a 1D vector added up
    !! \param vector input 1-dim vector
    !! \param num size of input 1-dim vector
    !! \param threshold threshold value
    subroutine sum_up_left_real32(sum_left, vector, num, threshold)
        implicit none
        real(kind=4), intent(out)   :: sum_left
        real(kind=4), intent(in)    :: vector(num)
        integer(kind=4), intent(in) :: num
        real(kind=4), intent(in)    :: threshold
        ! include "./include/common_get_minmax/inc_get_minmax_detail.f90"
    end subroutine sum_up_left_real32


    !> A subroutine to the sum of values less than or equal the threshold from a 1-dim array with position indices. \n
    !! \return returns the sum of values less than or equal the threshold.
    !! \param vector input 1-dim vector
    !! \param num size of input 1-dim vector
    !! \param indices input 1-dim vector indices
    !! \param n_idx size of input 1-dim vector indices, num >= n_idx
    !! \param threshold threshold value
    subroutine sum_up_left_with_indices_real32(sum_left, vector, num, indices, n_idx, threshold)
        implicit none
        real(kind=4), intent(out)   :: sum_left
        real(kind=4), intent(in)    :: vector(num)
        integer(kind=4), intent(in) :: num
        integer(kind=4), intent(in) :: indices(n_idx)
        integer(kind=4), intent(in) :: n_idx
        real(kind=4), intent(in)    :: threshold
        ! include "./include/common_get_minmax/inc_get_minmax_detail.f90"
    end subroutine sum_up_left_with_indices_real32


    !> A function to calculate mean value of vector.
    !! \return mean_value_of_vector_real32 mean value of vector
    !! \param vector input 1-dim vector
    !! \param num size to input vector
    function mean_value_of_vector_real32(vector, num)
        implicit none
        real(kind=4), intent(in)      :: vector(num)
        integer(kind=4), intent(in) :: num
        real(kind=4)                :: mean_value_of_vector_real32

        real(kind=4)    :: tmp_sum
        real(kind=4)    :: buffer(31)
        integer(kind=4) :: i, i_unroll, j
        
        tmp_sum = 0.0
        i_unroll = num - mod(num, 31)
        do i=1, i_unroll, 31
            do j=0, 31-1, 1
                buffer(j+1) = vector(i+j)
            end do

            do j=0, 31-1, 1
                tmp_sum = tmp_sum + buffer(j+1)
            end do
        end do

        do i=i_unroll+1, num, 1
            tmp_sum = tmp_sum + vector(i)
        end do
        mean_value_of_vector_real32 = tmp_sum / float(num)
    end function mean_value_of_vector_real32
    include "./include/stats_mean_value_of_vector/inc_mean_value_of_vector.f90"


    !> A function to calculate mean value of matrix, exactly equal to sum(matrix, dim=1)/N.
    !! \return mean_values_of_matrix_real32 mean values of matrix
    !! \param matrix input 2-dim matrix
    !! \param n_rows number of rows of matrix
    !! \param n_cols number of columns of matrix
    function mean_values_of_matrix_real32(matrix, n_rows, n_cols)
        implicit none
        real(kind=4), intent(in)    :: matrix(n_rows, n_cols)
        integer(kind=4), intent(in) :: n_rows, n_cols
        real(kind=4)                :: mean_values_of_matrix_real32(n_cols)

        integer(kind=4) :: i, j, k
        real(kind=4)    :: tmp_sum
        integer(kind=4) :: n_cols_unroll
        real(kind=4)    :: tmp_sums(7)
        real(kind=4)    :: tmp_means(n_cols), tmp_inv

        include "./include/stats_mean_values_of_matrix/inc_mean_values_of_matrix_detail.f90"
        mean_values_of_matrix_real32 = tmp_means
    end function mean_values_of_matrix_real32
    include "./include/stats_mean_values_of_matrix/inc_mean_values_of_matrix.f90"


    !> A subroutine to get the minimum and maximum values from a 1-dim array at the same time. \n
    !! \return returns minimum and maximum values at the same time
    !! \param min_val minimum value of the input vector
    !! \param max_val maximum value of the input vector
    !! \param vector input 1-dim vector
    !! \param num size of input 1-dim vector
    subroutine get_minmax_real32(min_val, max_val, vector, num)
        implicit none
        real(kind=4), intent(out)   :: min_val
        real(kind=4), intent(out)   :: max_val
        real(kind=4), intent(in)    :: vector(num)
        integer(kind=4), intent(in) :: num
        real(kind=4)    :: tmp_val, tmp_min_val, tmp_max_val
        real(kind=4)    :: buffer(buffer_get_minmax)
        integer(kind=4) :: n, unroll, j
        include "./include/stats_get_minmax/inc_get_minmax_detail.f90"
    end subroutine get_minmax_real32
    include "./include/stats_get_minmax/inc_get_minmax.f90"


    !> A subroutine to get the minimum and maximum values of a specified index of a 1-dim array at the same time. \n
    !! \return returns minimum and maximum values at the same time
    !! \param min_val minimum value of the input vector
    !! \param max_val maximum value of the input vector
    !! \param vector input 1-dim vector
    !! \param num size of input 1-dim vector
    !! \param indices input 1-dim vector indices
    !! \param n_idx size of input 1-dim vector indices, num >= n_idx
    subroutine get_minmax_with_indices_real32(min_val, max_val, vector, num, indices, n_idx)
        implicit none
        real(kind=4), intent(out)   :: min_val
        real(kind=4), intent(out)   :: max_val
        real(kind=4), intent(in)    :: vector(num)
        integer(kind=4), intent(in) :: num
        integer(kind=4), intent(in) :: indices(n_idx)
        integer(kind=4), intent(in) :: n_idx
        ! include "./include/common_get_minmax_with_index/inc_get_minmax_with_index_detail.f90"
    end subroutine get_minmax_with_indices_real32


    !> A function to calculate variance value of vector.
    !! \param vector input 1-dim vector
    !! \param num size to input vector
    !! \return variance_real32 variance
    function variance_value_of_vector_real32(vector, num, mean_of_vector)
        implicit none
        real(kind=4), intent(in)      :: vector(num)
        integer(kind=4), intent(in) :: num
        real(kind=4)                :: variance_value_of_vector_real32
        real(kind=4), optional      :: mean_of_vector
        real(kind=4)                :: mean_of_vector_opt, tmp_sq_sum, tmp
        real(kind=4)                :: buffer(31)
        integer(kind=4)             :: i, j, unroll, len_buffer

        len_buffer = 31

        if ( present(mean_of_vector) ) then
            mean_of_vector_opt = mean_of_vector
        else
            mean_of_vector_opt = mean(vector, num)
        end if
        tmp_sq_sum = 0
        unroll = num - mod(num, len_buffer)

        do i=1, unroll, len_buffer
            do j=0, len_buffer-1, 1
                buffer(j+1) = vector(i+j) - mean_of_vector_opt
            end do
            
            do j=0, len_buffer-1, 1
                tmp_sq_sum = tmp_sq_sum + buffer(j+1) ** 2
            end do
        end do

        do i=unroll+1, num
            tmp_sq_sum = tmp_sq_sum + (vector(i)-mean_of_vector_opt) ** 2
        end do
        variance_value_of_vector_real32 = tmp_sq_sum / float(num-1)
    end function variance_value_of_vector_real32
    include "./include/stats_variance_value_of_vector/inc_variance_value_of_vector.f90"


    !> A function to calculate variance values of matrix.
    !! \return variance_real32 variance values of matrix
    !! \param matrix input 2-dim matrix
    !! \param n_rows number of rows of matrix
    !! \param n_cols number of columns of matrix
    !! \param means_of_matrix ***optional*** precomputed mean values of matrix
    function variance_values_of_matrix_real32(matrix, n_rows, n_cols, means_of_matrix)
        implicit none
        real(kind=4), intent(in)    :: matrix(n_rows, n_cols)
        integer(kind=4), intent(in) :: n_rows, n_cols
        real(kind=4)                :: variance_values_of_matrix_real32(n_cols)
        real(kind=4), optional      :: means_of_matrix(n_cols)

        integer(kind=4) :: i, j, k
        real(kind=4)    :: tmp_sq_sum, means_of_matrix_opt(n_cols)
        integer(kind=4) :: n_cols_unroll
        real(kind=4)    :: tmp_sq_sums(7)
        real(kind=4)    :: tmp_variances(n_cols), tmp_inv

        tmp_inv = 1.0 / float(n_rows)

        if (present(means_of_matrix)) then
            means_of_matrix_opt = means_of_matrix
        else
            means_of_matrix_opt = mean(matrix, n_rows, n_cols)
        end if

        n_cols_unroll = n_cols - mod(n_cols, 7)
        do j=1, n_cols_unroll, 7
            tmp_sq_sums = 0
            do i=1, n_rows
                do k=0, 7-1, 1
                    tmp_sq_sums(k+1) = tmp_sq_sums(k+1) + (matrix(i,j+k) - means_of_matrix_opt(j+k))**2.0
                end do
            end do

            do k=0, 7-1, 1
                tmp_variances(j+k) = tmp_sq_sums(k+1) * tmp_inv
            end do
        end do
        do j=n_cols_unroll+1, n_cols
            tmp_sq_sum = 0
            do i=1, n_rows
                tmp_sq_sum = tmp_sq_sum + (matrix(i,j) - means_of_matrix_opt(j))**2.0
            end do
            tmp_variances(j) = tmp_sq_sum * tmp_inv
        end do
        variance_values_of_matrix_real32 = tmp_variances
    end function variance_values_of_matrix_real32
    include "./include/stats_variance_values_of_matrix/inc_variance_values_of_matrix.f90"


    !> A function to calculate covariance of two imput vectors.
    !! \return covariance_real32 covariance
    !! \param vector1 input 1-dim vector, sizes of input vectors must be same.
    !! \param vector2 input 1-dim vector, sizes of input vectors must be same.
    !! \param num size to input vector
    !! \param mean1 ***optional*** precomputed mean value of vector1
    !! \param mean2 ***optional*** precomputed mean value of vector2
    function covariance_value_of_vectors_real32(vector1, vector2, num, mean1, mean2)
        implicit none
        real(kind=4), intent(in)      :: vector1(num), vector2(num)
        integer(kind=4), intent(in) :: num
        real(kind=4)                :: covariance_value_of_vectors_real32
        real(kind=4), optional      :: mean1, mean2
        real(kind=4)                :: mean1_opt, mean2_opt, tmp_sum
        integer(kind=4)             :: i

        tmp_sum = 0
        if ( present(mean1) ) then
            mean1_opt = mean1
        else
            mean1 = mean(vector1, num)
        end if
        if ( present(mean2) ) then
            mean2_opt = mean2
        else
            mean2 = mean(vector2, num)
        end if

        do i=1, num, 1
            tmp_sum = tmp_sum + (vector1(i)-mean1_opt) * (vector2(i)-mean2_opt)
        end do
        covariance_value_of_vectors_real32 = tmp_sum / float(num-1)
    end function covariance_value_of_vectors_real32
    include "./include/stats_covariance_value_of_vectors/inc_covariance_value_of_vectors.f90"


    !> A subroutine to calculate covariance matrix of input matrix.
    !! \return cov_mat covariance matrix of input matrix
    !! \param cov_mat computed covariance matrix.
    !! \param matrix input 2-dim matrix
    !! \param n_rows number of rows of matrix
    !! \param n_cols number of columns of matrix
    !! \param mean1 ***optional*** precomputed mean value of vector1
    !! \param mean2 ***optional*** precomputed mean value of vector2
    subroutine covariance_matrix_of_matrix_real32(cov_mat, matrix, n_rows, n_cols, means_of_matrix)
        implicit none
        real(kind=4), intent(inout) :: cov_mat(n_cols,n_cols)
        real(kind=4), intent(in)    :: matrix(n_rows, n_cols)
        integer(kind=4), intent(in) :: n_rows, n_cols
        real(kind=4), optional      :: means_of_matrix(n_cols)

        integer(kind=4) :: i, j, k, l, n_cols_unroll
        real(kind=4) :: means_of_matrix_opt(n_cols)
        real(kind=4) :: var_vals(n_cols)
        real(kind=4) :: tmp_v1xv2(7)

        if ( present(means_of_matrix) ) then
            means_of_matrix_opt = means_of_matrix
        else
            means_of_matrix_opt = mean(matrix, n_rows, n_cols)
        end if
        var_vals  = variance(matrix, n_rows, n_cols, means_of_matrix=means_of_matrix_opt)
        do i=1, n_cols, 1
            cov_mat(i,i) = var_vals(i)
        end do

        n_cols_unroll = n_cols - mod(n_cols, 7)
        do j=1, n_cols_unroll, 7
            do i=j+1, n_cols, 1
                tmp_v1xv2 = 0
                do l=1, n_rows, 1
                    do k=0, 7-1
                        tmp_v1xv2(k+1) = tmp_v1xv2(k+1) + matrix(l,i)*matrix(l,j+k)
                    end do
                end do
                do k=0, 7-1, 1
                    cov_mat(i,j+k) = tmp_v1xv2(k+1)/float(n_rows-1) - means_of_matrix_opt(i)*means_of_matrix_opt(j+k)
                end do
            end do
        end do

        do j=n_cols_unroll+1, n_cols, 1
            do i=j+1, n_cols, 1
                cov_mat(i,j) = covariance(matrix(:,i), matrix(:,j), n_rows, means_of_matrix_opt(i), means_of_matrix_opt(j))
            end do
        end do

        do j=1, n_cols, 1
            do i=j+1, n_cols
            cov_mat(j,i) = cov_mat(i,j)
            end do
        end do
    end subroutine covariance_matrix_of_matrix_real32
    include "./include/stats_covariance_matrix_of_matrix/inc_covariance_matrix_of_matrix.f90"


    !> A subroutine to calculate the total value of y for each unique value of x.
    !! \return returns statistical values of y
    !! \param uniq_x unique values of x (sorted)
    !! \param x input values
    !! \param stat_y statistical values of y for each unique values
    !! \param y input values to be aggregated
    !! \param n_samples number of samples of x and y
    subroutine groupby_sum_r8(uniq_x, x, stat_y, y, n_samples)
        implicit none
        real(kind=8), allocatable   :: uniq_x(:)
        real(kind=8), intent(in)    :: x(n_samples)
        real(kind=8), allocatable   :: stat_y(:)
        real(kind=8), intent(in)    :: y(n_samples)
        integer(kind=8), intent(in) :: n_samples

        real(kind=8), allocatable :: x_copy(:), y_copy(:)
        real(kind=8)              :: sum_y
        integer(kind=8)           :: n, i, i_start, i_stop
        integer(kind=8), allocatable :: positions(:)

        if ( allocated(uniq_x) ) deallocate(uniq_x)
        if ( allocated(stat_y) ) deallocate(stat_y)

        allocate(x_copy(n_samples), y_copy(n_samples))
        do n=1, n_samples, 1
            x_copy(n) = x(n)
            y_copy(n) = y(n)
        end do

        call quick_argsort(x_copy, y_copy, n_samples)

        allocate(uniq_x(0))
        allocate(positions(0))
        uniq_x = [uniq_x, x_copy(1)]
        do n=2, n_samples, 1
            if ( x_copy(n-1) .ne. x_copy(n) ) then
                uniq_x = [uniq_x, x_copy(n)]
                positions = [positions, n-1]
            end if
        end do
        positions = [positions, n_samples]

        allocate(stat_y(0))
        
        i_start = 1
        do n=1, size(positions), 1
            sum_y = 0d0
            i_stop = positions(n)
            do i=i_start,  i_stop, 1
                sum_y = sum_y + y_copy(i)
            end do
            i_start = i_stop + 1
            stat_y = [stat_y, sum_y]
        end do
    end subroutine groupby_sum_r8

    subroutine groupby_sum_fast_r8(uniq_x, stat_x, x, n_samples)
        implicit none
        real(kind=8), allocatable   :: uniq_x(:)
        real(kind=8), allocatable   :: stat_x(:)
        real(kind=8), intent(in)    :: x(n_samples)
        integer(kind=8), intent(in) :: n_samples

        real(kind=8), allocatable :: x_copy(:)
        real(kind=8)              :: sum_x
        integer(kind=8)           :: n, i, i_start, i_stop, n_unique, idx
        integer(kind=8), allocatable :: positions(:)

        if ( allocated(uniq_x) ) deallocate(uniq_x)
        if ( allocated(stat_x) ) deallocate(stat_x)

        allocate(x_copy(n_samples))
        do n=1, n_samples, 1
            x_copy(n) = x(n)
        end do
        call quick_sort(x_copy, n_samples)
        n_unique = count_unique(x_copy, n_samples)

        allocate(uniq_x(n_unique))
        allocate(positions(n_unique))
        uniq_x(1) = x_copy(1)
        idx = 2
        do n=2, n_samples, 1
            if ( x_copy(n-1) .ne. x_copy(n) ) then
                uniq_x(idx) = x_copy(n)
                positions(idx-1) = n-1
                idx = idx + 1
            end if
        end do
        positions(n_unique) = n_samples

        allocate(stat_x(n_unique))        
        i_start = 1
        do n=1, size(positions), 1
            sum_x = 0d0
            i_stop = positions(n)
            do i=i_start,  i_stop, 1
                sum_x = sum_x + x_copy(i)
            end do
            stat_x(n) = sum_x
            i_start = i_stop + 1
        end do
    end subroutine groupby_sum_fast_r8


    !> A subroutine to calculate the squared value of y for each unique value of x.
    !! \return returns statistical values of y
    !! \param uniq_x unique values of x (sorted)
    !! \param x input values
    !! \param stat_y statistical values of y for each unique values
    !! \param y input values to be aggregated
    !! \param n_samples number of samples of x and y
    subroutine groupby_sq_sum_r8(uniq_x, x, stat_y, y, n_samples)
        implicit none
        real(kind=8), allocatable   :: uniq_x(:)
        real(kind=8), intent(in)    :: x(n_samples)
        real(kind=8), allocatable   :: stat_y(:)
        real(kind=8), intent(in)    :: y(n_samples)
        integer(kind=8), intent(in) :: n_samples

        real(kind=8), allocatable :: x_copy(:), y_copy(:)
        real(kind=8)              :: sum_y
        integer(kind=8) :: n, i, i_start, i_stop
        integer(kind=8), allocatable :: positions(:)

        if ( allocated(uniq_x) ) deallocate(uniq_x)
        if ( allocated(stat_y) ) deallocate(stat_y)

        allocate(x_copy(n_samples), y_copy(n_samples))
        do n=1, n_samples, 1
            x_copy(n) = x(n)
            y_copy(n) = y(n)
        end do

        call quick_argsort(x_copy, y_copy, n_samples)

        allocate(uniq_x(0))
        allocate(positions(0))
        uniq_x = [uniq_x, x_copy(1)]
        do n=2, n_samples, 1
            if ( x_copy(n-1) .ne. x_copy(n) ) then
                uniq_x = [uniq_x, x_copy(n)]
                positions = [positions, n-1]
            end if
        end do
        positions = [positions, n_samples]

        allocate(stat_y(0))
        
        i_start = 1
        do n=1, size(positions), 1
            sum_y = 0d0
            i_stop = positions(n)
            do i=i_start,  i_stop, 1
                sum_y = sum_y + y_copy(i) ** 2d0
            end do
            i_start = i_stop + 1
            stat_y = [stat_y, sum_y]
        end do

        ! do n=1, size(uniq_x), 1
        !     print*, uniq_x(n), positions(n), stat_y(n)
        ! end do
        ! print*, sum(y), sum(y_copy), sum(stat_y)
    end subroutine groupby_sq_sum_r8


    !> A subroutine to calculate the number of y for each unique value of x.
    !! \return returns statistical values of y
    !! \param uniq_x unique values of x (sorted)
    !! \param x input values
    !! \param stat_y statistical values of y for each unique values
    !! \param y input values to be aggregated
    !! \param n_samples number of samples of x and y
    subroutine groupby_count_r8(uniq_x, x, stat_y, y, n_samples)
        implicit none
        real(kind=8), allocatable   :: uniq_x(:)
        real(kind=8), intent(in)    :: x(n_samples)
        integer(kind=8), allocatable   :: stat_y(:)
        real(kind=8), intent(in)    :: y(n_samples)
        integer(kind=8), intent(in) :: n_samples

        real(kind=8), allocatable :: x_copy(:), y_copy(:)
        real(kind=8)              :: sum_y
        integer(kind=8) :: n, i, i_start, i_stop
        integer(kind=8), allocatable :: positions(:)

        if ( allocated(uniq_x) ) deallocate(uniq_x)
        if ( allocated(stat_y) ) deallocate(stat_y)

        allocate(x_copy(n_samples), y_copy(n_samples))
        do n=1, n_samples, 1
            x_copy(n) = x(n)
            y_copy(n) = y(n)
        end do

        call quick_argsort(x_copy, y_copy, n_samples)

        allocate(uniq_x(0))
        allocate(positions(0))
        uniq_x = [uniq_x, x_copy(1)]
        do n=2, n_samples, 1
            if ( x_copy(n-1) .ne. x_copy(n) ) then
                uniq_x = [uniq_x, x_copy(n)]
                positions = [positions, n-1]
            end if
        end do
        positions = [positions, n_samples]

        allocate(stat_y(0))
        
        i_start = 1
        do n=1, size(positions), 1
            sum_y = 0d0
            stat_y = [stat_y, positions(n)-i_start+1]
            i_start = positions(n)+1
        end do
    end subroutine groupby_count_r8

    subroutine groupby_count_i8(uniq_x, x, stat_y, y, n_samples)
        implicit none
        integer(kind=8), allocatable   :: uniq_x(:)
        integer(kind=8), intent(in)    :: x(n_samples)
        integer(kind=8), allocatable   :: stat_y(:)
        integer(kind=8), intent(in)    :: y(n_samples)
        integer(kind=8), intent(in) :: n_samples

        integer(kind=8), allocatable :: x_copy(:), y_copy(:)
        integer(kind=8)              :: sum_y
        integer(kind=8) :: n, i, i_start, i_stop
        integer(kind=8), allocatable :: positions(:)

        if ( allocated(uniq_x) ) deallocate(uniq_x)
        if ( allocated(stat_y) ) deallocate(stat_y)

        allocate(x_copy(n_samples), y_copy(n_samples))
        do n=1, n_samples, 1
            x_copy(n) = x(n)
            y_copy(n) = y(n)
        end do

        call quick_argsort(x_copy, y_copy, n_samples)

        allocate(uniq_x(0))
        allocate(positions(0))
        uniq_x = [uniq_x, x_copy(1)]
        do n=2, n_samples, 1
            if ( x_copy(n-1) .ne. x_copy(n) ) then
                uniq_x = [uniq_x, x_copy(n)]
                positions = [positions, n-1]
            end if
        end do
        positions = [positions, n_samples]

        allocate(stat_y(0))
        
        i_start = 1
        do n=1, size(positions), 1
            sum_y = 0d0
            stat_y = [stat_y, positions(n)-i_start+1]
            i_start = positions(n)+1
        end do
    end subroutine groupby_count_i8

    subroutine groupby_count_fast_r8(uniq_x, stat_x, x, n_samples)
        implicit none
        real(kind=8), allocatable    :: uniq_x(:)
        integer(kind=8), allocatable :: stat_x(:)
        real(kind=8), intent(in)     :: x(n_samples)
        integer(kind=8), intent(in)  :: n_samples

        real(kind=8), allocatable :: x_copy(:)
        integer(kind=8) :: n, i, i_start, i_stop, n_unique, idx
        integer(kind=8), allocatable :: positions(:)

        if ( allocated(uniq_x) ) deallocate(uniq_x)
        if ( allocated(stat_x) ) deallocate(stat_x)

        allocate(x_copy(n_samples))
        do n=1, n_samples, 1
            x_copy(n) = x(n)
        end do
        call quick_sort(x_copy, n_samples)
        n_unique = count_unique(x_copy, n_samples)

        allocate(uniq_x(n_unique))
        allocate(positions(n_unique))
        uniq_x(1) = x_copy(1)
        idx = 2
        do n=2, n_samples, 1
            if ( x_copy(n-1) .ne. x_copy(n) ) then
                uniq_x(idx) = x_copy(n)
                positions(idx-1) = n-1
                idx = idx + 1
            end if
        end do
        positions(n_unique) = n_samples

        allocate(stat_x(n_unique))        
        i_start = 1
        do n=1, size(positions), 1
            stat_x(n) = positions(n)-i_start+1
            i_start = positions(n)+1
        end do
    end subroutine groupby_count_fast_r8

end module mod_stats