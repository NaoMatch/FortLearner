!> A module to calculate simple statistics.
module mod_stats
    use mod_const
    use mod_sort
    implicit none

    !> Interface to call sum_up_left_r4, sum_up_left_with_indices_real32
    interface sum_up_left
        module procedure sum_up_left_r4
        module procedure sum_up_left_r8
        module procedure sum_up_left_i4
        module procedure sum_up_left_i4_r4_threshold
        module procedure sum_up_left_i8
        module procedure sum_up_left_i8_r8_threshold
    end interface sum_up_left

    interface sum_of_matrix
        module procedure sum_of_matrix_r4
        module procedure sum_of_matrix_r8
        module procedure sum_of_matrix_i4
        module procedure sum_of_matrix_i8
    end interface sum_of_matrix

    !> Interface to call get_minmax_r4, get_minmax_real64, get_minmax_int32, get_minmax_int64
    interface get_minmax
        module procedure get_minmax_r4
        module procedure get_minmax_r8
        module procedure get_minmax_i4
        module procedure get_minmax_i8
    end interface get_minmax

    !> Interface to call mean_value_of_vector_r4, mean_value_of_vector_r8, mean_value_of_vector_i4, mean_value_of_vector_i8
    interface mean  
        module procedure mean_value_of_vector_r4
        module procedure mean_value_of_vector_r8     ! place in "./include/stats/mean_value_of_vector/inc_mean_value_of_vector.f90"
        module procedure mean_value_of_vector_i4      ! place in "./include/stats/mean_value_of_vector/inc_mean_value_of_vector.f90"
        module procedure mean_value_of_vector_i8      ! place in "./include/stats/mean_value_of_vector/inc_mean_value_of_vector.f90"

        module procedure mean_values_of_matrix_r4
        module procedure mean_values_of_matrix_r8     ! place in "./include/stats/mean_values_of_matrix/inc_mean_values_of_matrix.f90"
        module procedure mean_values_of_matrix_i4      ! place in "./include/stats/mean_values_of_matrix/inc_mean_values_of_matrix.f90"
        module procedure mean_values_of_matrix_i8      ! place in "./include/stats/mean_values_of_matrix/inc_mean_values_of_matrix.f90"
    end interface mean

    !> Interface to call variance_value_of_vector_r4, variance_value_of_vector_r8, variance_value_of_vector_i4, variance_value_of_vector_i8
    interface variance
        module procedure variance_value_of_vector_r4
        module procedure variance_value_of_vector_r8     ! place in "./include/stats/variance_value_of_vector/inc_variance_value_of_vector.f90"
        module procedure variance_value_of_vector_i4      ! place in "./include/stats/variance_value_of_vector/inc_variance_value_of_vector.f90"
        module procedure variance_value_of_vector_i8      ! place in "./include/stats/variance_value_of_vector/inc_variance_value_of_vector.f90"

        module procedure variance_values_of_matrix_r4
        module procedure variance_values_of_matrix_r8     ! place in "./include/stats/variance_values_of_matrix/inc_variance_values_of_matrix.f90"
        module procedure variance_values_of_matrix_i4      ! place in "./include/stats/variance_values_of_matrix/inc_variance_values_of_matrix.f90"
        module procedure variance_values_of_matrix_i8      ! place in "./include/stats/variance_values_of_matrix/inc_variance_values_of_matrix.f90"
    end interface variance

    !> Interface to call covariance_value_of_vectors_r4, covariance_value_of_vectors_r8, covariance_value_of_vectors_i4, covariance_value_of_vectors_i8
    interface covariance
        module procedure covariance_value_of_vectors_r4
        module procedure covariance_value_of_vectors_r8     ! place in "./include/stats/covariance_value_of_vectors/inc_covariance_value_of_vectors.f90"
        module procedure covariance_value_of_vectors_i4      ! place in "./include/stats/covariance_value_of_vectors/inc_covariance_value_of_vectors.f90"
        module procedure covariance_value_of_vectors_i8      ! place in "./include/stats/covariance_value_of_vectors/inc_covariance_value_of_vectors.f90"
    end interface covariance

    interface covariance_matrix
        module procedure covariance_matrix_of_matrix_r4
        module procedure covariance_matrix_of_matrix_r8     ! place in "./include/stats/covariance_matrix_of_matrix/inc_covariance_matrix_of_matrix.f90"
        module procedure covariance_matrix_of_matrix_i4      ! place in "./include/stats/covariance_matrix_of_matrix/inc_covariance_matrix_of_matrix.f90"
        module procedure covariance_matrix_of_matrix_i8      ! place in "./include/stats/covariance_matrix_of_matrix/inc_covariance_matrix_of_matrix.f90"        
    end interface covariance_matrix

    interface groupby_sum
        module procedure groupby_sum_r4_r4
        module procedure groupby_sum_r4_i4
        module procedure groupby_sum_r8_r8
        module procedure groupby_sum_r8_i8
        module procedure groupby_sum_i4_r4
        module procedure groupby_sum_i4_i4
        module procedure groupby_sum_i8_r8
        module procedure groupby_sum_i8_i8
        
        module procedure groupby_sum_self_r4
        module procedure groupby_sum_self_r8
        module procedure groupby_sum_self_i4
        module procedure groupby_sum_self_i8
    end interface groupby_sum

    interface groupby_sq_sum
        module procedure groupby_sq_sum_r4_i4
        module procedure groupby_sq_sum_r4_r4
        module procedure groupby_sq_sum_r8_i8
        module procedure groupby_sq_sum_r8_r8
        module procedure groupby_sq_sum_i4_i4
        module procedure groupby_sq_sum_i4_r4
        module procedure groupby_sq_sum_i8_i8
        module procedure groupby_sq_sum_i8_r8

        module procedure groupby_sq_sum_self_r4
        module procedure groupby_sq_sum_self_r8
        module procedure groupby_sq_sum_self_i4
        module procedure groupby_sq_sum_self_i8
    end interface groupby_sq_sum

    interface groupby_mean
        module procedure groupby_mean_r4_i4
        module procedure groupby_mean_r4_r4
        module procedure groupby_mean_r8_i8
        module procedure groupby_mean_r8_r8
        module procedure groupby_mean_i4_i4
        module procedure groupby_mean_i4_r4
        module procedure groupby_mean_i8_i8
        module procedure groupby_mean_i8_r8

        module procedure groupby_mean_self_r4
        module procedure groupby_mean_self_r8
        module procedure groupby_mean_self_i4
        module procedure groupby_mean_self_i8
    end interface groupby_mean

    interface groupby_count
        module procedure groupby_count_r4
        module procedure groupby_count_r8
        module procedure groupby_count_i4
        module procedure groupby_count_i8
    end interface groupby_count

contains

    !> A subroutine to the sum of values less than or equal the threshold from a 1-dim array. \n
    !! \return returns the sum of values less than or equal the threshold.
    !! \param sum_left value that is less than or equal to the threshold value in a 1D vector added up
    !! \param vector input 1-dim vector
    !! \param num size of input 1-dim vector
    !! \param threshold threshold value
    function sum_up_left_r4(vector, threshold, n_samples)
        implicit none
        real(kind=4)                :: sum_up_left_r4
        real(kind=4), intent(in)    :: vector(n_samples)
        real(kind=4), intent(in)    :: threshold
        integer(kind=4), intent(in) :: n_samples
        integer(kind=4)             :: n, factor
        real(kind=4)                :: val, tmp_sum, zero=0
        include "./include/stats/sum_up_left/inc_sum_up_left_detail.f90"
        sum_up_left_r4 = tmp_sum
    end function sum_up_left_r4
    include "./include/stats/sum_up_left/inc_sum_up_left.f90"


    subroutine sum_of_matrix_r4(sum_mat, matrix, n_samples, n_columns, dim)
        implicit none
        real(kind=4), allocatable, intent(inout) :: sum_mat(:)
        real(kind=4), intent(in)                 :: matrix(n_samples, n_columns)
        integer(kind=4), intent(in)              :: n_samples, n_columns
        integer(kind=4), intent(in)              :: dim

        integer(kind=4) :: i, j, k, one=1, two=2
        real(kind=4) :: tmp_sum, zero=0.0
        include "./include/stats/sum_of_matrix/inc_sum_of_matrix_detail.f90"
    end subroutine sum_of_matrix_r4
    include "./include/stats/sum_of_matrix/inc_sum_of_matrix.f90"    


    !> A function to calculate mean value of vector.
    !! \return mean_value_of_vector_r4 mean value of vector
    !! \param vector input 1-dim vector
    !! \param n_samples size to input vector
    function mean_value_of_vector_r4(vector, n_samples)
        implicit none
        real(kind=4), intent(in)      :: vector(n_samples)
        integer(kind=4), intent(in) :: n_samples
        real(kind=4)                :: mean_value_of_vector_r4

        real(kind=4)    :: tmp_sum
        real(kind=4)    :: buffer(31)
        integer(kind=4) :: i, i_unroll, j, buffer_size=31
        include "./include/stats/mean_value_of_vector/inc_mean_value_of_vector_detail.f90"        
        mean_value_of_vector_r4 = tmp_sum / float(n_samples)
    end function mean_value_of_vector_r4
    include "./include/stats/mean_value_of_vector/inc_mean_value_of_vector.f90"


    !> A function to calculate mean value of matrix, exactly equal to sum(matrix, dim=1)/N.
    !! \return mean_values_of_matrix_r4 mean values of matrix
    !! \param matrix input 2-dim matrix
    !! \param n_samples number of rows of matrix
    !! \param n_columns number of columns of matrix
    function mean_values_of_matrix_r4(matrix, n_samples, n_columns)
        implicit none
        real(kind=4), intent(in)    :: matrix(n_samples, n_columns)
        integer(kind=4), intent(in) :: n_samples, n_columns
        real(kind=4)                :: mean_values_of_matrix_r4(n_columns)

        integer(kind=4) :: i, j, k
        real(kind=4)    :: tmp_sum
        integer(kind=4) :: n_columns_unroll
        real(kind=4)    :: tmp_sums(7)
        real(kind=4)    :: tmp_means(n_columns), tmp_inv
        include "./include/stats/mean_values_of_matrix/inc_mean_values_of_matrix_detail.f90"
        mean_values_of_matrix_r4 = tmp_means
    end function mean_values_of_matrix_r4
    include "./include/stats/mean_values_of_matrix/inc_mean_values_of_matrix.f90"


    !> A subroutine to get the minimum and maximum values from a 1-dim array at the same time. \n
    !! \return returns minimum and maximum values at the same time
    !! \param min_val minimum value of the input vector
    !! \param max_val maximum value of the input vector
    !! \param vector input 1-dim vector
    !! \param n_samples size of input 1-dim vector
    subroutine get_minmax_r4(min_val, max_val, vector, n_samples)
        implicit none
        real(kind=4), intent(out)   :: min_val
        real(kind=4), intent(out)   :: max_val
        real(kind=4), intent(in)    :: vector(n_samples)
        integer(kind=4), intent(in) :: n_samples
        real(kind=4)    :: tmp_val, tmp_min_val, tmp_max_val
        real(kind=4)    :: buffer(buffer_get_minmax)
        integer(kind=4) :: n, n_samples_unroll, j
        include "./include/stats/get_minmax/inc_get_minmax_detail.f90"
    end subroutine get_minmax_r4
    include "./include/stats/get_minmax/inc_get_minmax.f90"


    !> A function to calculate variance value of vector.
    !! \param vector input 1-dim vector
    !! \param n_samples size to input vector
    !! \return variance_real32 variance
    function variance_value_of_vector_r4(vector, n_samples, mean_of_vector)
        implicit none
        real(kind=4), intent(in)    :: vector(n_samples)
        integer(kind=4), intent(in) :: n_samples
        real(kind=4)                :: variance_value_of_vector_r4
        real(kind=4), optional      :: mean_of_vector
        real(kind=4)                :: mean_of_vector_opt, tmp_sq_sum, tmp
        real(kind=4)                :: buffer(31), val
        integer(kind=4)             :: i, j, n_samples_unroll, len_buffer = 31

        include "./include/stats/variance_value_of_vector/inc_variance_value_of_vector_detail.f90"
        variance_value_of_vector_r4 = tmp_sq_sum / real(n_samples, kind=kind(n_samples))
    end function variance_value_of_vector_r4
    include "./include/stats/variance_value_of_vector/inc_variance_value_of_vector.f90"


    !> A function to calculate variance values of matrix.
    !! \return variance_real32 variance values of matrix
    !! \param matrix input 2-dim matrix
    !! \param n_rows number of rows of matrix
    !! \param n_cols number of columns of matrix
    !! \param means_of_matrix ***optional*** precomputed mean values of matrix
    function variance_values_of_matrix_r4(matrix, n_rows, n_cols, means_of_matrix)
        implicit none
        real(kind=4), intent(in)    :: matrix(n_rows, n_cols)
        integer(kind=4), intent(in) :: n_rows, n_cols
        real(kind=4)                :: variance_values_of_matrix_r4(n_cols)
        real(kind=4), optional      :: means_of_matrix(n_cols)

        integer(kind=4) :: i, j, k
        real(kind=4)    :: tmp_sq_sum, means_of_matrix_opt(n_cols)
        integer(kind=4) :: n_cols_unroll, buffer_len=7
        real(kind=4)    :: tmp_sq_sums(7), val
        real(kind=4)    :: tmp_variances(n_cols), tmp_inv
        include "./include/stats/variance_values_of_matrix/inc_variance_values_of_matrix_detail.f90"
        variance_values_of_matrix_r4 = tmp_variances
    end function variance_values_of_matrix_r4
    include "./include/stats/variance_values_of_matrix/inc_variance_values_of_matrix.f90"


    !> A function to calculate covariance of two imput vectors.
    !! \return covariance_real32 covariance
    !! \param vector1 input 1-dim vector, sizes of input vectors must be same.
    !! \param vector2 input 1-dim vector, sizes of input vectors must be same.
    !! \param num size to input vector
    !! \param mean1 ***optional*** precomputed mean value of vector1
    !! \param mean2 ***optional*** precomputed mean value of vector2
    function covariance_value_of_vectors_r4(vector1, vector2, num, mean1, mean2)
        implicit none
        real(kind=4), intent(in)      :: vector1(num), vector2(num)
        integer(kind=4), intent(in) :: num
        real(kind=4)                :: covariance_value_of_vectors_r4
        real(kind=4), optional      :: mean1, mean2
        real(kind=4)                :: mean1_opt, mean2_opt, tmp_sum
        integer(kind=4)             :: i, j
        include "./include/stats/covariance_value_of_vectors/inc_covariance_value_of_vectors_detail.f90"
        covariance_value_of_vectors_r4 = tmp_sum / real(num, kind=kind(tmp_sum))
    end function covariance_value_of_vectors_r4
    include "./include/stats/covariance_value_of_vectors/inc_covariance_value_of_vectors.f90"


    !> A subroutine to calculate covariance matrix of input matrix.
    !! \return cov_mat covariance matrix of input matrix
    !! \param cov_mat computed covariance matrix.
    !! \param matrix input 2-dim matrix
    !! \param n_rows number of rows of matrix
    !! \param n_cols number of columns of matrix
    !! \param mean1 ***optional*** precomputed mean value of vector1
    !! \param mean2 ***optional*** precomputed mean value of vector2
    subroutine covariance_matrix_of_matrix_r4(cov_mat, matrix, n_rows, n_cols, means_of_matrix)
        implicit none
        real(kind=4), intent(inout) :: cov_mat(n_cols,n_cols)
        real(kind=4), intent(in)    :: matrix(n_rows, n_cols)
        integer(kind=4), intent(in) :: n_rows, n_cols
        real(kind=4), optional      :: means_of_matrix(n_cols)

        integer(kind=4) :: i, j, k, l, n_cols_unroll
        real(kind=4) :: means_of_matrix_opt(n_cols)
        real(kind=4) :: var_vals(n_cols)
        real(kind=4) :: tmp_v1xv2(7), tmp
        integer(kind=4) :: buffer_len=7
        include "./include/stats/covariance_matrix_of_matrix/inc_covariance_matrix_of_matrix_detail.f90"
    end subroutine covariance_matrix_of_matrix_r4
    include "./include/stats/covariance_matrix_of_matrix/inc_covariance_matrix_of_matrix.f90"


    !> A subroutine to calculate the total value of y for each unique value of x.
    !! \return returns statistical values of y
    !! \param uniq_x unique values of x (sorted)
    !! \param x input values
    !! \param stat_y statistical values of y for each unique values of x
    !! \param y input values to be aggregated
    !! \param n_samples number of samples of x and y
    subroutine groupby_sum_r4_r4(uniq_x, x, stat_y, y, n_samples)
        implicit none
        real(kind=4), allocatable   :: uniq_x(:)
        real(kind=4), intent(in)    :: x(n_samples)
        real(kind=4), allocatable   :: stat_y(:)
        real(kind=4), intent(in)    :: y(n_samples)
        integer(kind=4), intent(in) :: n_samples

        real(kind=4), allocatable :: x_copy(:)
        real(kind=4), allocatable :: y_copy(:)
        real(kind=4)              :: sum_y
        integer(kind=4)           :: n, i, i_start, i_stop, idx
        integer(kind=4), allocatable :: positions(:), indices(:)
        include "./include/stats/groupby_sum/inc_groupby_sum_detail.f90"
    end subroutine groupby_sum_r4_r4
    include "./include/stats/groupby_sum/inc_groupby_sum.f90"


    !> A subroutine to calculate the total value of x for each unique value of x.
    !! \return returns statistical values of y
    !! \param uniq_x unique values of x (sorted)
    !! \param x input values
    !! \param stat_x statistical values of x for each unique values of x
    !! \param n_samples number of samples of x
    subroutine groupby_sum_self_r4(uniq_x, stat_x, x, n_samples)
        implicit none
        real(kind=4), allocatable   :: uniq_x(:)
        real(kind=4), allocatable   :: stat_x(:)
        real(kind=4), intent(in)    :: x(n_samples)
        integer(kind=4), intent(in) :: n_samples

        real(kind=4), allocatable :: x_copy(:)
        real(kind=4)              :: sum_x
        integer(kind=4)           :: n, i, i_start, i_stop, n_unique, idx
        integer(kind=4), allocatable :: positions(:)
        include "./include/stats/groupby_sum/inc_groupby_sum_self_detail.f90"
    end subroutine groupby_sum_self_r4
    include "./include/stats/groupby_sum/inc_groupby_sum_self.f90"


    !> A subroutine to calculate the squared value of y for each unique value of x.
    !! \return returns statistical values of y
    !! \param uniq_x unique values of x (sorted)
    !! \param x input values
    !! \param stat_y statistical values of y for each unique values of x
    !! \param y input values to be aggregated
    !! \param n_samples number of samples of x and y
    subroutine groupby_sq_sum_r4_i4(uniq_x, x, stat_y, y, n_samples)
        implicit none
        real(kind=4), allocatable    :: uniq_x(:)
        real(kind=4), intent(in)     :: x(n_samples)
        integer(kind=4), allocatable :: stat_y(:)
        integer(kind=4), intent(in)  :: y(n_samples)
        integer(kind=4), intent(in)  :: n_samples

        real(kind=4), allocatable    :: x_copy(:)
        integer(kind=4), allocatable :: y_copy(:)
        integer(kind=4)              :: sum_y
        integer(kind=4)              :: n, i, i_start, i_stop, idx
        integer(kind=4), allocatable :: positions(:)
        include "./include/stats/groupby_sq_sum/inc_groupby_sq_sum_detail.f90"
    end subroutine groupby_sq_sum_r4_i4
    include "./include/stats/groupby_sq_sum/inc_groupby_sq_sum.f90"


    !> A subroutine to calculate the squared value of x for each unique values.
    !! \return returns statistical values of x
    !! \param uniq_x unique values of x (sorted)
    !! \param stat_x statistical values of x for each unique values
    !! \param x input values
    !! \param n_samples number of samples of x and y
    subroutine groupby_sq_sum_self_r4(uniq_x, stat_x, x, n_samples)
        implicit none
        real(kind=4), allocatable    :: uniq_x(:)
        real(kind=4), allocatable    :: stat_x(:)
        real(kind=4), intent(in)     :: x(n_samples)
        integer(kind=4), intent(in)  :: n_samples

        real(kind=4), allocatable    :: x_copy(:)
        real(kind=4)                 :: sum_x
        integer(kind=4)              :: n, i, i_start, i_stop, idx
        integer(kind=4), allocatable :: positions(:)
        include "./include/stats/groupby_sq_sum/inc_groupby_sq_sum_self_detail.f90"
    end subroutine groupby_sq_sum_self_r4
    include "./include/stats/groupby_sq_sum/inc_groupby_sq_sum_self.f90"


    !> A subroutine to calculate the number of y for each unique value of x.
    !! \return returns statistical values of y
    !! \param uniq_x unique values of x (sorted)
    !! \param x input values
    !! \param stat_y statistical values of y for each unique values
    !! \param y input values to be aggregated
    !! \param n_samples number of samples of x and y
    subroutine groupby_count_r4(uniq_x, stat_x, x, n_samples)
        implicit none
        real(kind=4), allocatable    :: uniq_x(:)
        integer(kind=4), allocatable :: stat_x(:)
        real(kind=4), intent(in)     :: x(n_samples)
        integer(kind=4), intent(in)  :: n_samples

        real(kind=4), allocatable :: x_copy(:)
        integer(kind=4) :: n, i, i_start, i_stop, n_unique, idx
        integer(kind=4), allocatable :: positions(:)
        include "./include/stats/groupby_count/inc_groupby_count_detail.f90"
    end subroutine groupby_count_r4
    include "./include/stats/groupby_count/inc_groupby_count.f90"


    !> A subroutine to calculate mean of y for each unique value of x.
    !! \return returns statistical values of y
    !! \param uniq_x unique values of x (sorted)
    !! \param x input values
    !! \param stat_y statistical values of y for each unique values
    !! \param y input values to be aggregated
    !! \param n_samples number of samples of x and y
    subroutine groupby_mean_r4_i4(uniq_x, x, stat_y, y, n_samples)
        implicit none
        real(kind=4), allocatable   :: uniq_x(:)
        real(kind=4), intent(in)    :: x(n_samples)
        real(kind=4), allocatable   :: stat_y(:)
        integer(kind=4), intent(in) :: y(n_samples)
        integer(kind=4), intent(in) :: n_samples

        real(kind=4), allocatable    :: x_copy(:)
        integer(kind=4), allocatable :: y_copy(:)
        integer(kind=4), allocatable :: sum_y(:)
        integer(kind=4), allocatable :: count_x(:)
        integer(kind=4) :: n, u, n_unique_x, zero_i
        real(kind=4) :: kind_r, zero_r=0.0
        include "./include/stats/groupby_mean/inc_groupby_mean_detail.f90"
    end subroutine groupby_mean_r4_i4
    include "./include/stats/groupby_mean/inc_groupby_mean.f90"


    !> A subroutine to calculate mean of x for each unique value.
    !! \return returns statistical values of x
    !! \param uniq_x unique values of x (sorted)
    !! \param stat_x statistical values of x for each unique values
    !! \param x input values
    !! \param n_samples number of samples of x and y
    subroutine groupby_mean_self_r4(uniq_x, stat_x, x, n_samples)
        implicit none
        real(kind=4), allocatable   :: uniq_x(:)
        real(kind=4), allocatable   :: stat_x(:)
        real(kind=4), intent(in)    :: x(n_samples)
        integer(kind=4), intent(in) :: n_samples

        real(kind=4), allocatable    :: x_copy(:)
        real(kind=4), allocatable    :: sum_x(:)
        integer(kind=4), allocatable :: count_x(:)
        integer(kind=4) :: n, u, n_unique_x, zero_i
        real(kind=4) :: kind_r, zero_r=0.0
        include "./include/stats/groupby_mean/inc_groupby_mean_self_detail.f90"
    end subroutine groupby_mean_self_r4
    include "./include/stats/groupby_mean/inc_groupby_mean_self.f90"

end module mod_stats