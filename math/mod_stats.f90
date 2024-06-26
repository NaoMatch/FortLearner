!> A module to calculate simple statistics.
module mod_stats
    !$ use omp_lib
    use iso_c_binding
    use mod_const
    use mod_sort
    use mod_common
    implicit none

    !> An interface to call sum_up function with inline assembler and Fortran
    include "./include/stats/sum_up/inc_sum_up_interface_to_C.f90"
    interface sum_up
        module procedure sum_up_r8
        module procedure sum_up_i8
    end interface sum_up

    include "./include/stats/sum_of_matrix/inc_sum_of_matrix_interface_to_C.f90"
    interface sum_up_matrix
        module procedure sum_up_matrix_r8
        module procedure sum_up_matrix_i8
    end interface sum_up_matrix

    interface sum_up_matrix_parallel
        module procedure sum_up_matrix_parallel_r8
        module procedure sum_up_matrix_parallel_i8
    end interface sum_up_matrix_parallel

    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_interface_to_C.f90"
    interface count_and_sum_up_gt
        module procedure count_and_sum_up_gt_r8
        module procedure count_and_sum_up_gt_i8
    end interface count_and_sum_up_gt

    include "./include/stats/sum_up_gt/inc_matrix_count_and_sum_up_gt_interface_to_C.f90"
    interface get_matrix_count_and_sum_up_gt
        module procedure get_matrix_count_and_sum_up_r8
    end interface get_matrix_count_and_sum_up_gt

    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_vector2vector_interface_to_C.f90"
    interface get_count_and_sum_up_gt_vector2vector
        module procedure get_count_and_sum_up_gt_vector2vector_r8
    end interface get_count_and_sum_up_gt_vector2vector

    include "./include/stats/sum_up_gt/inc_matrix_count_and_sum_up_gt_parallel_interface_to_C.f90"
    interface get_matrix_count_and_sum_up_parallel
        module procedure get_matrix_count_and_sum_up_parallel_r8
    end interface get_matrix_count_and_sum_up_parallel


    !> Interface to call sum_up_left_r4, sum_up_left_with_indices_real32
    interface sum_of_matrix
        module procedure sum_of_matrix_r4
        module procedure sum_of_matrix_r8
        module procedure sum_of_matrix_i4
        module procedure sum_of_matrix_i8
    end interface sum_of_matrix


    include "./include/stats/get_minmax/inc_get_minmax_interface_to_C.f90"
    interface get_minmax
        module procedure get_minmax_hybrid_r8
        module procedure get_minmax_hybrid_i8
    end interface get_minmax

    include "./include/stats/get_minmax/inc_get_matrix_minmax_interface_to_C.f90"
    interface get_matrix_minmax
        module procedure get_matrix_minmax_r8
    end interface get_matrix_minmax

    include "./include/stats/get_minmax/inc_get_matrix_minmax_parallel_interface_to_C.f90"
    interface get_matrix_minmax_parallel
        module procedure get_matrix_minmax_parallel_r8
    end interface get_matrix_minmax_parallel

    Interface
        subroutine new_get_matrix_minmax(min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols, n_jobs) & 
            Bind(C,Name='new_get_matrix_minmax')
            Import
            integer(c_int64_t), value      :: n_indices
            integer(c_int64_t), value      :: n_rows
            integer(c_int64_t), value      :: n_cols
            integer(c_int64_t), value      :: n_jobs
            real(c_double), intent(inout)  :: min_vals(n_cols)
            real(c_double), intent(inout)  :: max_vals(n_cols)
            real(c_double), intent(in)     :: mat_t(n_cols, n_rows)
            integer(c_int64_t), intent(in) :: indices(n_indices)
        end subroutine new_get_matrix_minmax
    end interface

    Interface
        subroutine new_get_matrix_count_and_sum_up_gt(&
                sum_vals_r, cnt_vals_r, thr_vals, &
                mat_t, y, indices, &
                n_indices, n_rows, n_cols, n_jobs) & 
            Bind(C,Name='new_get_matrix_count_and_sum_up_gt')
            Import
            integer(c_int64_t), value          :: n_indices
            integer(c_int64_t), value          :: n_rows
            integer(c_int64_t), value          :: n_cols
            integer(c_int64_t), value          :: n_jobs
            real(c_double), intent(inout)      :: sum_vals_r(n_cols)
            integer(c_int64_t), intent(inout)  :: cnt_vals_r(n_cols)
            real(c_double), intent(inout)      :: thr_vals(n_cols)
            real(c_double), intent(in)         :: mat_t(n_cols, n_rows)
            real(c_double), intent(in)         :: y(n_rows)
            integer(c_int64_t), intent(in)     :: indices(n_indices)
        end subroutine new_get_matrix_count_and_sum_up_gt    

        subroutine new_get_matrix_count_and_sum_up_gt_single(&
                sum_vals_r, cnt_vals_r, thr_vals, &
                mat_t, y, indices, &
                n_indices, n_rows, n_cols) & 
            Bind(C,Name='new_get_matrix_count_and_sum_up_gt_single')
            Import
            integer(c_int64_t), value          :: n_indices
            integer(c_int64_t), value          :: n_rows
            integer(c_int64_t), value          :: n_cols
            real(c_double), intent(inout)      :: sum_vals_r(n_cols)
            integer(c_int64_t), intent(inout)  :: cnt_vals_r(n_cols)
            real(c_double), intent(inout)      :: thr_vals(n_cols)
            real(c_double), intent(in)         :: mat_t(n_cols, n_rows)
            real(c_double), intent(in)         :: y(n_rows)
            integer(c_int64_t), intent(in)     :: indices(n_indices)
        end subroutine new_get_matrix_count_and_sum_up_gt_single    
    end interface

    !> Median Interface
    interface median
            module procedure median_value_of_vector_r8
    end interface median

    !> Weighted Median Interface
    interface weighted_median
            module procedure weighted_median_value_of_vector_r8_r8
    end interface weighted_median

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

    !> Interface to call variance_value_of_vector_r4, variance_value_of_vector_r8, variance_value_of_vector_i4, variance_value_of_vector_i8
    include "./include/stats/variance_value_of_vector/inc_variance_interface_to_C.f90"
    interface variance_fast
        module procedure variance_fast_r8     ! place in "./include/stats/variance_value_of_vector/inc_variance_value_of_vector.f90"
    end interface variance_fast

    !> Interface to call covariance_value_of_vectors_r4, covariance_value_of_vectors_r8, covariance_value_of_vectors_i4, covariance_value_of_vectors_i8
    interface covariance
        module procedure covariance_value_of_vectors_r4
        module procedure covariance_value_of_vectors_r8     ! place in "./include/stats/covariance_value_of_vectors/inc_covariance_value_of_vectors.f90"
        module procedure covariance_value_of_vectors_i4      ! place in "./include/stats/covariance_value_of_vectors/inc_covariance_value_of_vectors.f90"
        module procedure covariance_value_of_vectors_i8      ! place in "./include/stats/covariance_value_of_vectors/inc_covariance_value_of_vectors.f90"
    end interface covariance

    include "./include/stats/covariance_value_of_vectors/inc_covariance_interface_to_C.f90"
    interface covariance_fast
        module procedure covariance_fast_r8
    end interface covariance_fast

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

    include "./include/stats/get_minmax_vector2vector/inc_get_minmax_vector2vector_interface_to_C.f90"
    interface get_minmax_vector2vector
        module procedure get_minmax_vector2vector_r8
    end interface get_minmax_vector2vector

    interface kurtosis
        module procedure kurtosis_vector
        module procedure kurtosis_matrix
    end interface kurtosis

contains

    function kurtosis_vector(vector, n_samples) result(kurtosis)
        implicit none
        real(kind=8), intent(in) :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples
        real(kind=8) :: kurtosis

        real(kind=8) :: sum_0, sum_1, sum_2, sum_3, sum_4
        real(kind=8) :: tmp, tmp_
        real(kind=8) :: mu, var
        real(kind=8) :: sum_v, sum_sq_v
        integer(kind=8) :: n

        sum_v = 0d0
        sum_sq_v = 0d0
        do n=1, n_samples, 1
            tmp = vector(n)
            sum_v = sum_v + tmp
            sum_sq_v = sum_sq_v + tmp**2d0
        end do
        
        mu = sum_v / dble(n_samples)
        var = sum_sq_v / dble(n_samples) - mu**2d0
        tmp_ = 0d0
        do n=1, n_samples, 1
            tmp = vector(n)
            tmp_ = tmp_ + (tmp-mu)**4d0 / var**2d0
        end do
        n = n_samples
        if (var==0d0) then
            kurtosis = 0d0
        else
            kurtosis = sum((vector - mu)**4) &
                / (n * var**2d0) - 3.0
        end if
    end function kurtosis_vector

    function kurtosis_matrix(matrix, n_samples, n_columns) result(kurtosis)
        implicit none
        real(kind=8), intent(in) :: matrix(n_samples, n_columns)
        integer(kind=8), intent(in) :: n_samples, n_columns
        real(kind=8), allocatable :: kurtosis(:)

        integer(kind=8) :: f

        allocate(kurtosis(n_columns))
        do f=1, n_columns, 1
            kurtosis(f) = kurtosis_vector(matrix(:,f), n_samples)
        end do
    end function kurtosis_matrix






    include "./include/stats/get_minmax_vector2vector/inc_get_minmax_vector2vector.f90"
    subroutine get_minmax_vector2vector_r8(vals, min_vals, max_vals, n)
        implicit none
        real(kind=8), intent(in)    :: vals(n)
        real(kind=8), intent(inout) :: min_vals(n), max_vals(n)
        integer(kind=8), intent(in) :: n
#if _default
        call get_minmax_vector2vector_02_F_r8(vals, min_vals, max_vals, n)
#elif _x86_64
        call get_minmax_vector2vector_08z_A_r8(vals, min_vals, max_vals, n)
#else 
#error "CPU Architecture is not supported. Use '-D_default'."
#endif
    end subroutine get_minmax_vector2vector_r8



    ! -----------------------------------------------------------------------------
    ! -----------------------------------------------------------------------------
    ! -----------------------------------------------------------------------------
    ! sum_up functions

    include "./include/stats/sum_up/inc_sum_up_vector_variants.f90"
    !> A function to sum up vector, real 64bit.
    !! Change the function to be executed depending on the size of the vector.
    !! \param sum_up_r8 Sum of vector elements
    !! \param x vector
    !! \param n size of vector
    function sum_up_r8(x,n)
        implicit none
        real(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in) :: n
        real(kind=8)                :: sum_up_r8
#if _default
        if (n .le. 32_8) then
            sum_up_r8 = sum(x)
        else
            sum_up_r8 = sum_up_32_f_r8(x,n)
        end if
#elif _x86_64
        if (n .le. 32_8) then
            sum_up_r8 = sum_up_16_C_r8(x,n)
        elseif(n .le. 10000000) then
            sum_up_r8 = sum_up_32_ASM_r8(x,n)
        else
            sum_up_r8 = sum_up_04_ASM_r8(x,n)
        end if
#else 
#error "CPU Architecture is not supported. Use '-D_default'."
#endif
    end function sum_up_r8


    !> A function to sum up vector, integer 64bit.
    !! Change the function to be executed depending on the size of the vector.
    !! \param sum_up_i8 Sum of vector elements
    !! \param x vector
    !! \param n size of vector
    function sum_up_i8(x,n)
        implicit none
        integer(kind=8), intent(in) :: x(n)
        integer(kind=8), intent(in) :: n
        integer(kind=8)             :: sum_up_i8
#if _default
        if (n .le. 32_8) then
            sum_up_i8 = sum(x)
        else
            sum_up_i8 = sum_up_32_f_i8(x,n)
        end if
#elif _x86_64
        if (n .le. 32_8) then
            sum_up_i8 = sum_up_16_C_i8(x,n)
        elseif(n .le. 10000000) then
            sum_up_i8 = sum_up_32_ASM_i8(x,n)
        else
            sum_up_i8 = sum_up_04_ASM_i8(x,n)
        end if
#else 
#error "CPU Architecture is not supported. Use '-D_default'."
#endif
    end function sum_up_i8


    include "./include/stats/sum_up/inc_sum_up_matrix_variants.f90"
    !> A function to sum up matrix column by column, real 64bit.
    !! Change the function to be executed depending on the size(row, col) of the matrix.
    !! \param r column sum
    !! \param x matrix n by c
    !! \param n number of rows
    !! \param c number of columns
    subroutine sum_up_matrix_r8(r, x, n, c)
        implicit none
        integer(kind=8) :: i
        real(kind=8) :: tmp_sum
#if _default
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        if (n .le. 250000_8) then
            do i=1, c, 1
                tmp_sum = sum_up(x(:,i),n)
                r(i) = tmp_sum
            end do
        else
            call sum_up_matrix_02_04_r8(r, x, n, c)
        end if
#elif _x86_64
        real(kind=8), intent(inout), target :: r(c)
        real(kind=8), intent(in), target    :: x(n,c)
        integer(kind=8), intent(in)         :: n,c

        type(c_ptr) :: x_ptr
        type(c_ptr) :: r_ptr

        x_ptr = c_loc(x)
        r_ptr = c_loc(r)

        if (n .le. 256_8) then
            call sum_up_matrix_08_04_ASM_r8(r_ptr, x_ptr, n, c)
        elseif(n .le. 50000) then
            do i=1, c, 1
                tmp_sum = sum_up(x(:,i),n)
                r(i) = tmp_sum
            end do
        else
            call sum_up_matrix_04_08_C_r8(r_ptr, x_ptr, n, c)
        end if
#else 
#error "CPU Architecture is not supported. Use '-D_default'."
#endif
    end subroutine sum_up_matrix_r8

    !> A function to sum up matrix column by column, integer 64bit.
    !! Change the function to be executed depending on the size(row, col) of the matrix.
    !! \param r column sum
    !! \param x matrix n by c
    !! \param n number of rows
    !! \param c number of columns
    subroutine sum_up_matrix_i8(r, x, n, c)
        implicit none
        integer(kind=8) :: i
        integer(kind=8) :: tmp_sum
#if _default
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in)    :: n,c

        if (n .le. 250000_8) then
            do i=1, c, 1
                tmp_sum = sum_up(x(:,i),n)
                r(i) = tmp_sum
            end do
        else
            call sum_up_matrix_02_04_i8(r, x, n, c)
        end if
#elif _x86_64
        integer(kind=8), intent(inout), target :: r(c)
        integer(kind=8), intent(in), target    :: x(n,c)
        integer(kind=8), intent(in)            :: n,c

        type(c_ptr) :: x_ptr
        type(c_ptr) :: r_ptr

        x_ptr = c_loc(x)
        r_ptr = c_loc(r)

        if (n .le. 256_8) then
            call sum_up_matrix_08_04_ASM_i8(r_ptr, x_ptr, n, c)
        elseif(n .le. 50000) then
            do i=1, c, 1
                tmp_sum = sum_up(x(:,i),n)
                r(i) = tmp_sum
            end do
        else
            call sum_up_matrix_04_08_C_i8(r_ptr, x_ptr, n, c)
        end if
#else 
#error "CPU Architecture is not supported. Use '-D_default'."
#endif
    end subroutine sum_up_matrix_i8


    !> A subroutine to calculate sum up matrix column by column with 'sum_up'(for vector sum up function) openmp, real 64bit.
    !! \param r column sum
    !! \param x matrix n by c
    !! \param n number of rows
    !! \param c number of columns
    subroutine sum_up_matrix_parallel_r8(r, x, n, c)
        implicit none
        real(kind=8), intent(inout) :: r(c)
        real(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: i
        real(kind=8) :: tmp_sum

        !$omp parallel num_threads(8)
        !$omp do
        do i=1, c, 1
            tmp_sum = sum_up(x(:,i),n)
            r(i) = tmp_sum
        end do
        !$omp end do
        !$omp end parallel
    end subroutine sum_up_matrix_parallel_r8


    !> A subroutine to calculate sum up matrix column by column with 'sum_up'(for vector sum up function) openmp, integer 64bit.
    !! \param r column sum
    !! \param x matrix n by c
    !! \param n number of rows
    !! \param c number of columns
    subroutine sum_up_matrix_parallel_i8(r, x, n, c)
        implicit none
        integer(kind=8), intent(inout) :: r(c)
        integer(kind=8), intent(in)    :: x(n,c)
        integer(kind=8), intent(in) :: n,c

        integer(kind=8) :: i
        integer(kind=8) :: tmp_sum

        !$omp parallel num_threads(8)
        !$omp do
        do i=1, c, 1
            tmp_sum = sum_up(x(:,i),n)
            r(i) = tmp_sum
        end do
        !$omp end do
        !$omp end parallel
    end subroutine sum_up_matrix_parallel_i8


    include "./include/stats/sum_up_gt/inc_count_and_sum_up_gt_vector2vector.f90"
    subroutine get_count_and_sum_up_gt_vector2vector_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n)
        implicit none
        real(kind=8), intent(in)       :: x_vals(n)
        real(kind=8), intent(in)       :: thr_vals(n)
        real(kind=8), intent(inout)    :: sum_vals(n)
        integer(kind=8), intent(inout) :: cnt_vals(n)
        real(kind=8), intent(in)       :: y_val
        integer(kind=8), intent(in)    :: n
#if _default
        call count_and_sum_up_gt_vector2vector_16_F_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n)
#elif _x86_64
        call count_and_sum_up_gt_vector2vector_16z_A_r8(x_vals, thr_vals, sum_vals, cnt_vals, y_val, n)
#else 
#error "CPU Architecture is not supported. Use '-D_default'."
#endif
    end subroutine get_count_and_sum_up_gt_vector2vector_r8

        include "./include/stats/sum_up_gt/inc_get_matrix_count_and_sum_up_gt_parallel_F.f90"



    ! -----------------------------------------------------------------------------
    ! -----------------------------------------------------------------------------
    ! -----------------------------------------------------------------------------
    subroutine count_and_sum_up_gt_r8(sum_gt, cnt_gt, x, y, v, n)
        implicit none
        real(kind=8), intent(inout)    :: sum_gt
        integer(kind=8), intent(inout) :: cnt_gt
        real(kind=8), intent(in)       :: x(n), y(n)
        real(kind=8), intent(in)       :: v
        integer(kind=8), intent(in)    :: n
#if _default
        if (n .le. 16000_8) then
            call count_and_sum_up_gt_loop_16_if_F_r8(sum_gt, cnt_gt, x, y, v, n)
        else
            call count_and_sum_up_gt_loop_branchless_16_F_r8(sum_gt, cnt_gt, x, y, v, n)
        end if
#elif _x86_64
        if (n .le. 64_8) then
            call count_and_sum_up_gt_loop_16_if_F_r8(sum_gt, cnt_gt, x, y, v, n)
        elseif(n .le. 3000_8) then
            call count_and_sum_up_gt_loop_08_A_r8(sum_gt, cnt_gt, x, y, v, n)
        elseif(n .le. 1000000_8) then
            call count_and_sum_up_gt_loop_32_A_r8(sum_gt, cnt_gt, x, y, v, n)
        else
            call count_and_sum_up_gt_loop_04_A_r8(sum_gt, cnt_gt, x, y, v, n)
        end if
#else 
#error "CPU Architecture is not supported. Use '-D_default'."
#endif
    end subroutine count_and_sum_up_gt_r8


    subroutine count_and_sum_up_gt_i8(sum_gt, cnt_gt, x, y, v, n)
        implicit none
        integer(kind=8), intent(inout) :: sum_gt
        integer(kind=8), intent(inout) :: cnt_gt
        integer(kind=8), intent(in)    :: x(n), y(n)
        integer(kind=8), intent(in)    :: v
        integer(kind=8), intent(in)    :: n
#if _default
        if (n .le. 16000_8) then
            call count_and_sum_up_gt_loop_16_if_F_i8(sum_gt, cnt_gt, x, y, v, n)
        else
            call count_and_sum_up_gt_loop_branchless_16_F_i8(sum_gt, cnt_gt, x, y, v, n)
        end if
#elif _x86_64
        if (n .le. 3000_8) then
            call count_and_sum_up_gt_loop_16_if_F_i8(sum_gt, cnt_gt, x, y, v, n)
        elseif(n .le. 1000000_8) then
            call count_and_sum_up_gt_loop_32_A_i8(sum_gt, cnt_gt, x, y, v, n)
        else
            call count_and_sum_up_gt_loop_04_A_i8(sum_gt, cnt_gt, x, y, v, n)
        end if
#else 
#error "CPU Architecture is not supported. Use '-D_default'."
#endif
    end subroutine count_and_sum_up_gt_i8


    include "./include/stats/sum_up_gt/inc_matrix_count_and_sum_up_gt.f90"
    subroutine get_matrix_count_and_sum_up_r8(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, n_indices, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout), target    :: sum_vals(n_cols)
        integer(kind=8), intent(inout), target :: cnt_vals(n_cols)
        real(kind=8), intent(inout), target    :: thr_vals(n_cols)
        real(kind=8), intent(in)   , target    :: mat_t(n_cols, n_rows), y(n_rows)
        integer(kind=8), intent(in), target    :: indices(n_indices)
        integer(kind=8), intent(in)            :: n_indices, n_rows, n_cols
#if _default
        call get_matrix_count_and_sum_up_gt_with_index_08(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
            n_indices, n_rows, n_cols)
#elif _x86_64
        type(c_ptr) :: sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, mat_t_ptr, y_ptr, indices_diff_ptr
        integer(kind=8), allocatable, target :: indices_diff(:)

        sum_vals = 0d0
        cnt_vals = 0_8

        allocate(indices_diff(n_indices))
        call get_indices_diff(indices_diff, indices, n_indices)

        sum_vals_ptr = c_loc(sum_vals)
        cnt_vals_ptr = c_loc(cnt_vals)
        thr_vals_ptr = c_loc(thr_vals)
        mat_t_ptr    = c_loc(mat_t)
        y_ptr        = c_loc(y)
        indices_diff_ptr = c_loc(indices_diff)

        if (n_cols .ge. 4_8 .and. n_rows .ge. 4_8) then
            call get_matrix_count_and_sum_up_gt_with_index_A_04_04(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, mat_t_ptr, y_ptr, & 
                    indices_diff_ptr, n_indices, n_rows, n_cols)
        else
            call get_matrix_count_and_sum_up_gt_with_index_08(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
                    n_indices, n_rows, n_cols)
        end if
#else 
#error "CPU Architecture is not supported. Use '-D_default'."
#endif
    end subroutine get_matrix_count_and_sum_up_r8

    subroutine get_matrix_count_and_sum_up_parallel_r8(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
        n_indices, n_rows, n_cols, n_thds)
        implicit none
        real(kind=8), intent(inout), target    :: sum_vals(n_cols)
        integer(kind=8), intent(inout), target :: cnt_vals(n_cols)
        real(kind=8), intent(inout), target    :: thr_vals(n_cols)
        real(kind=8), intent(in)   , target    :: mat_t(n_cols, n_rows), y(n_rows)
        integer(kind=8), intent(in), target    :: indices(n_indices)
        integer(kind=8), intent(in)            :: n_indices, n_rows, n_cols, n_thds
#if _default
        call get_matrix_count_and_sum_up_gt_16_F_parallel_r8(sum_vals, cnt_vals, thr_vals, mat_t, y, indices, & 
            n_indices, n_rows, n_cols, n_thds)
#elif _x86_64
        type(c_ptr) :: sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, mat_t_ptr, y_ptr, indices_ptr
        integer(kind=8), allocatable, target :: indices_diff(:)

        sum_vals = 0d0
        cnt_vals = 0_8

        sum_vals_ptr = c_loc(sum_vals)
        cnt_vals_ptr = c_loc(cnt_vals)
        thr_vals_ptr = c_loc(thr_vals)
        mat_t_ptr    = c_loc(mat_t)
        y_ptr        = c_loc(y)
        indices_ptr = c_loc(indices)

        call get_matrix_count_and_sum_up_gt_with_index_parallel_32z_A(sum_vals_ptr, cnt_vals_ptr, thr_vals_ptr, & 
                mat_t_ptr, y_ptr, & 
                indices_ptr, n_indices, n_rows, n_cols, n_thds)
#else 
#error "CPU Architecture is not supported. Use '-D_default'."
#endif
    end subroutine get_matrix_count_and_sum_up_parallel_r8

    ! -----------------------------------------------------------------------------
    ! -----------------------------------------------------------------------------
    ! -----------------------------------------------------------------------------
    ! minmax functions
    ! include "./include/stats/get_minmax/inc_get_minmax.f90"
    subroutine get_minmax_hybrid_r8(min, max, x, n)
        implicit none
        real(kind=8), intent(inout) :: min, max
        real(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in) :: n
#if _default
        if (n .le. 64_8) then
            min = minval(x)
            max = maxval(x)
        elseif(n .le. 5000000_8) then
            call get_minmax_02_F_r8(min, max, x, n)
        end if
#elif _x86_64
        if (n .le. 256_8) then
            call get_minmax_unroll_08z_a_r8(min, max, x, n)
        elseif(n .le. 15000_8) then
            call get_minmax_unroll_16z_a_r8(min, max, x, n)
        else
            call get_minmax_unroll_16_a_r8(min, max, x, n)
        end if
#else 
#error "CPU Architecture is not supported. Use '-D_default'."
#endif
    end subroutine get_minmax_hybrid_r8

    subroutine get_minmax_hybrid_i8(min, max, x, n)
        implicit none
        integer(kind=8), intent(inout) :: min, max
        integer(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in)    :: n
#if _default
        if (n .le. 64_8) then
            call get_minmax_04_F_i8(min, max, x, n)
        elseif(n .le. 5000000_8) then
            call get_minmax_08_F_i8(min, max, x, n)
        else
            call get_minmax_08_F_i8(min, max, x, n)
        end if
#elif _x86_64
        if (n .le. 256_8) then
            call get_minmax_unroll_08z_a_i8(min, max, x, n)
        elseif(n .le. 15000_8) then
            call get_minmax_unroll_64_a_i8(min, max, x, n)
        elseif(n .gt. 70000_8) then
            call get_minmax_unroll_16_a_i8(min, max, x, n)
        end if
#else 
#error "CPU Architecture is not supported. Use '-D_default'."
#endif
    end subroutine get_minmax_hybrid_i8

    include "./include/stats/get_minmax/inc_get_matrix_minmax_F.f90"
    subroutine get_matrix_minmax_r8(min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols)
        implicit none
        real(kind=8), intent(inout), target :: min_vals(n_cols), max_vals(n_cols)
        real(kind=8), intent(in)   , target :: mat_t(n_cols, n_rows)
        integer(kind=8), intent(in), target :: indices(n_indices)
        integer(kind=8), intent(in)         :: n_indices, n_rows, n_cols
#if _default
        call get_matrix_minmax_col_major_loop_with_index(min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols)
#elif _x86_64
        type(c_ptr) :: min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_diff_ptr
        integer(kind=8), allocatable, target :: indices_diff(:)

        min_vals = huge(0d0)
        max_vals = -huge(0d0)

        allocate(indices_diff(n_indices))
        call get_indices_diff(indices_diff, indices, n_indices)

        min_vals_ptr = c_loc(min_vals)
        max_vals_ptr = c_loc(max_vals)
        mat_t_ptr    = c_loc(mat_t)
        indices_diff_ptr = c_loc(indices_diff)

        if (n_cols .lt. 4_8) then
            call get_matrix_minmax_col_major_loop_with_index(min_vals, max_vals, mat_t, indices, &
                    n_indices, n_rows, n_cols)
        elseif(n_cols .lt. 8_8) then
            call get_matrix_minmax_loop_04_A(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_diff_ptr, &
                    n_indices, n_rows, n_cols)
        elseif(n_cols .lt. 16_8) then
            call get_matrix_minmax_loop_08z_A(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_diff_ptr, &
                    n_indices, n_rows, n_cols)
        else
            call get_matrix_minmax_loop_16z_A(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_diff_ptr, &
                    n_indices, n_rows, n_cols)
        end if
#else 
#error "CPU Architecture is not supported. Use '-D_default'."
#endif
    end subroutine get_matrix_minmax_r8


    include "./include/stats/get_minmax/inc_get_matrix_minmax_parallel_F.f90"
    subroutine get_matrix_minmax_parallel_r8(min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols, n_thds)
        implicit none
        real(kind=8), intent(inout), target :: min_vals(n_cols), max_vals(n_cols)
        real(kind=8), intent(in)   , target :: mat_t(n_cols, n_rows)
        integer(kind=8), intent(in), target :: indices(n_indices)
        integer(kind=8), intent(in)         :: n_indices, n_rows, n_cols, n_thds
#if _default
        call get_matrix_minmax_with_index_parallel_04_F_r8(min_vals, max_vals, mat_t, indices, n_indices, n_rows, n_cols, n_thds)
#elif _x86_64
        type(c_ptr) :: min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_ptr

        min_vals = huge(0d0)
        max_vals = -huge(0d0)

        min_vals_ptr = c_loc(min_vals)
        max_vals_ptr = c_loc(max_vals)
        mat_t_ptr    = c_loc(mat_t)
        indices_ptr  = c_loc(indices)

        ! call get_matrix_minmax_with_index_parallel_32z_A_r8(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_ptr, &
        !             n_indices, n_rows, n_cols, n_thds)
        call get_matrix_minmax_with_index_parallel_01_C_r8(min_vals_ptr, max_vals_ptr, mat_t_ptr, indices_ptr, &
                    n_indices, n_rows, n_cols, n_thds)
#else 
#error "CPU Architecture is not supported. Use '-D_default'."
#endif
    end subroutine get_matrix_minmax_parallel_r8


    include "./include/stats/variance_value_of_vector/inc_variance_value_of_vector_detail_new.f90"
    function variance_fast_r8(x, n)
        implicit none
        real(kind=8)                :: variance_fast_r8
        real(kind=8), intent(in)    :: x(n)
        integer(kind=8), intent(in) :: n
#if _default
        variance_fast_r8 = variance_16_F_r8(x, n)
#elif _x86_64
        variance_fast_r8 = variance_08_A_r8(x, n)
#else 
#error "CPU Architecture is not supported. Use '-D_default'."
#endif
    end function variance_fast_r8

    include "./include/stats/covariance_value_of_vectors/inc_covariance_value_of_vectors_fast.f90"
    function covariance_fast_r8(x, y, n)
        implicit none
        real(kind=8)                :: covariance_fast_r8
        real(kind=8), intent(in)    :: x(n), y(n)
        integer(kind=8), intent(in) :: n
#if _default
        covariance_fast_r8 = covariance_loop_16_F(x, y, n)
#elif _x86_64
        covariance_fast_r8 = covariance_loop_16z_A(x, y, n)
#else 
#error "CPU Architecture is not supported. Use '-D_default'."
#endif
    end function covariance_fast_r8






    ! -----------------------------------------------------------------------------
    ! -----------------------------------------------------------------------------
    ! -----------------------------------------------------------------------------
    ! sum_up_left functions
    !> A function to sum up vector, real 64bit.
    !! Change the function to be executed depending on the size of the vector.
    !! \param sum_up_hybrid_r8 Sum of vector elements
    !! \param x vector
    !! \param n size of vector
    function sum_up_left_hybrid_i8(x, y, n, v)
        implicit none
        integer(kind=8), intent(in) :: x(n), y(n), v
        integer(kind=8), intent(in) :: n
        integer(kind=8)             :: sum_up_left_hybrid_i8
        ! if (n .le. 256_8) then
        !     sum_up_left_hybrid_i8 = sum_up_left_assembler_08_c_i8_i8(x, y, n, v)
        ! elseif ( n .le. 10000000_8 ) then
        !     sum_up_left_hybrid_i8 = sum_up_left_assembler_24_c_i8_i8(x, y, n, v)
        ! else
        !     sum_up_left_hybrid_i8 = sum_up_left_assembler_08_c_i8_i8(x, y, n, v)
        ! end if
    end function sum_up_left_hybrid_i8

    !> A function to sum up vector, integer 64bit.
    !! Change the function to be executed depending on the size of the vector.
    !! \param sum_up_hybrid_i8 Sum of vector elements
    !! \param x vector
    !! \param n size of vector
    function sum_up_left_hybrid_r8(x, y, n, v)
        implicit none
        real(kind=8), intent(in)    :: x(n), y(n), v
        integer(kind=8), intent(in) :: n
        real(kind=8)                :: sum_up_left_hybrid_r8
        ! if (n .le. 3000_8) then
        !     sum_up_left_hybrid_r8 = sum_up_left_assembler_08_c_r8_r8(x, y, n, v)
        ! elseif ( n .le. 10000000_8 ) then
        !     sum_up_left_hybrid_r8 = sum_up_left_assembler_20_c_r8_r8(x, y, n, v)
        ! else
        !     sum_up_left_hybrid_r8 = sum_up_left_assembler_04_c_r8_r8(x, y, n, v)
        ! end if
    end function sum_up_left_hybrid_r8

    !> A subroutine to the sum of values less than or equal the threshold from a 1-dim array. \n
    !! \return returns the sum of values less than or equal the threshold.
    !! \param sum_left value that is less than or equal to the threshold value in a 1D vector added up
    !! \param vector input 1-dim vector
    !! \param num size of input 1-dim vector
    !! \param threshold threshold value
    function sum_up_left_r4(x, y, threshold_y, n_samples)
        implicit none
        real(kind=4)                :: sum_up_left_r4
        real(kind=4), intent(in)    :: x(n_samples), y(n_samples)
        real(kind=4), intent(in)    :: threshold_y
        integer(kind=4), intent(in) :: n_samples
        integer(kind=4)             :: n, factor
        real(kind=4)                :: val_x, val_y, tmp_sum, zero=0
        include "./include/stats/sum_up_gt/inc_sum_up_gt_detail.f90"
        sum_up_left_r4 = tmp_sum
    end function sum_up_left_r4
    include "./include/stats/sum_up_gt/inc_sum_up_gt.f90"


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


    !> A function to calculate median value of vector.
    !! \return median_value_of_vector_r8 median value of vector
    !! \param vector input 1-dim vector
    !! \param n_samples size to input vector
    function median_value_of_vector_r8(vector, n_samples)
        implicit none
        real(kind=8)                :: median_value_of_vector_r8
        real(kind=8), intent(in)    :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples

        integer(kind=8)             :: half_position
        real(kind=8), allocatable   :: vector_copy(:)
        real(kind=8)                :: min_val_half
        logical(kind=4)             :: is_even

        allocate(vector_copy(n_samples))
        vector_copy(:) = vector(:)

        is_even = mod(n_samples, 2_8) == 0_8
        half_position = n_samples / 2_8

        call quick_select(vector_copy, n_samples, half_position)
        if (is_even) then
            min_val_half = minval(vector_copy(half_position+1:n_samples))
            median_value_of_vector_r8 = (vector_copy(half_position)+min_val_half) * .5d0
        else
            median_value_of_vector_r8 = vector_copy(half_position+1)
        end if
    end function median_value_of_vector_r8


    !> A function to calculate weighted median value of vector.
    !! \return weighted_median_value_of_vector_r8_r8 weighted median value of vector
    !! \param vector input 1-dim vector
    !! \param weight input 1-dim weight
    !! \param n_samples size to input vector
    !! \todo implement prune and search algorithm, https://nscpolteksby.ac.id/ebook/files/Ebook/Computer%20Engineering/Algorithm%20Design%20and%20Applications%20A4%20(2015)/10.%20Chapter%209%20-%20Fast%20Sorting%20and%20Selection.pdf
    function weighted_median_value_of_vector_r8_r8(vector, weight, n_samples)
        implicit none
        real(kind=8) :: weighted_median_value_of_vector_r8_r8
        real(kind=8), intent(in) :: vector(n_samples)
        real(kind=8), intent(in) :: weight(n_samples)
        integer(kind=8), intent(in) :: n_samples

        integer(kind=8) :: i
        real(kind=8) :: w_half, w_sum, min_w, max_w
        real(kind=8), allocatable :: vector_copy(:), weight_copy(:)

        call get_minmax(min_w, max_w, weight, n_samples)
        if (min_w .eq. max_w) then
            weighted_median_value_of_vector_r8_r8 = median(vector, n_samples)
            return
        end if

        allocate(vector_copy(n_samples))
        allocate(weight_copy(n_samples))
        vector_copy(:) = vector(:)
        weight_copy(:) = weight(:)

        call quick_argsort(vector_copy, weight_copy, n_samples)
        
        w_sum = 0d0
        w_half = sum(weight_copy) * .5d0
        do i=1, n_samples, 1
            w_sum = w_sum + weight_copy(i)
            if (w_sum >= w_half) exit
        end do
        weighted_median_value_of_vector_r8_r8 = vector_copy(i)
    end function weighted_median_value_of_vector_r8_r8

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