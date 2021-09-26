!> A module for Linear Algebra.
module mod_linalg
    use iso_c_binding
    use mod_common
    use mod_const
    use mod_stats
    implicit none


    !> An interface to call cholesky_decomposition_r4, cholesky_decomposition_r8, cholesky_decomposition_i4, cholesky_decomposition_i8
    interface cholesky_decomposition
        module procedure :: cholesky_decomposition_r4
        module procedure :: cholesky_decomposition_r8
        module procedure :: cholesky_decomposition_i4
        module procedure :: cholesky_decomposition_i8
    end interface cholesky_decomposition

    !> An interface to call cholesky_decomposition_modified_r4, cholesky_decomposition_modified_r8, cholesky_decomposition_modified_i4, cholesky_decomposition_modified_i8
    interface cholesky_decomposition_modified
        module procedure :: cholesky_decomposition_modified_r4
        module procedure :: cholesky_decomposition_modified_r8
        module procedure :: cholesky_decomposition_modified_i4
        module procedure :: cholesky_decomposition_modified_i8
    end interface cholesky_decomposition_modified

    !> An interface to call inner_product_r4, inner_product_r8, inner_product_i4, inner_product_i8
    interface inner_product
        module procedure :: inner_product_r4
        module procedure :: inner_product_r8
        module procedure :: inner_product_i4
        module procedure :: inner_product_i8
    end interface inner_product

    !> An interface to call inv_unit_lower_matrix_r4, inv_unit_lower_matrix_r8, inv_unit_lower_matrix_i4, inv_unit_lower_matrix_i8
    interface inv_unit_lower_matrix
        module procedure :: inv_unit_lower_matrix_r4
        module procedure :: inv_unit_lower_matrix_r8
        module procedure :: inv_unit_lower_matrix_i4
        module procedure :: inv_unit_lower_matrix_i8
    end interface inv_unit_lower_matrix

    interface inversion
        module procedure :: inversion_r8
    end interface inversion

    !> Interface to call mattxmat_r4, mattxmat_r8, mattxmat_i4, mattxmat_i8
    interface mattxmat
        module procedure mattxmat_r4
        module procedure mattxmat_r8
        module procedure mattxmat_i4
        module procedure mattxmat_i8
    end interface mattxmat

    !> An interface to calculate square sum of matrix by row. 
    interface matrix_sqsum_row
        module procedure matrix_sqsum_row_r8
    end interface matrix_sqsum_row
    include "./include/linalg_matrix_sqsum_row/inc_matrix_sqsum_row_interface_to_C.f90"

    !> An interface to call multi_mat_vec_r4, multi_mat_vec_r8, multi_mat_vec_i4, multi_mat_vec_i8
    interface multi_mat_vec
        module procedure :: multi_mat_vec_r4
        module procedure :: multi_mat_vec_r8
        module procedure :: multi_mat_vec_i4
        module procedure :: multi_mat_vec_i8
    end interface multi_mat_vec
    include "./include/linalg_multi_mat_vec/inc_multi_mat_vec_interface_to_C.f90"

    !> Interface to call rotate_from_both_side
    interface rotate_from_both_side
        module procedure rotate_from_both_side_r4
        module procedure rotate_from_both_side_r8
    end interface rotate_from_both_side

    !> Interface to call rotate_from_left
    interface rotate_from_left
        module procedure rotate_from_left_r4
        module procedure rotate_from_left_r8
    end interface rotate_from_left

    !> Interface to call rotate_from_right
    interface rotate_from_right
        module procedure rotate_from_right_r4
        module procedure rotate_from_right_r8
    end interface rotate_from_right

    !> Interface to call random_rotation
    interface random_rotation
        module procedure random_rotation_r4
        module procedure random_rotation_r8
    end interface random_rotation

    !> An interface to call supremum_eigen_value_r4, supremum_eigen_value_r8, supremum_eigen_value_i4, supremum_eigen_value_i8
    interface supremum_eigen_value
        module procedure :: supremum_eigen_value_r4
        module procedure :: supremum_eigen_value_r8
        module procedure :: supremum_eigen_value_i4
        module procedure :: supremum_eigen_value_i8
    end interface supremum_eigen_value

    interface vector2sum1
        module procedure vector2sum1_r4
        module procedure vector2sum1_r8
    end interface vector2sum1

    include "./include/linalg_ax_plus_y/inc_ax_plus_y_interface_to_C.f90"
    interface ax_plus_y
        module procedure ax_plus_y_r8
    end interface ax_plus_y


contains

    include "./include/linalg_ax_plus_y/inc_ax_plus_y.f90"
    subroutine ax_plus_y_r8(a, x, y, n)
        implicit none
        real(kind=8), intent(in)    :: a
        real(kind=8), intent(in)    :: x(n)
        real(kind=8), intent(inout) :: y(n)
        integer(kind=8), intent(in) :: n
#if _default
        call ax_plus_y_16_F_r8(a, x, y, n)
#elif _x86_64
        if (n .le. 32) then
            call ax_plus_y_32z_A(a, x, y, n)
        elseif ( n .le. 500000 ) then
            call ax_plus_y_32z_A(a, x, y, n)
        else
            call ax_plus_y_16_F_r8(a, x, y, n)
        end if
#else 
#error "CPU Architecture is not supported. Use '-D_default'."
#endif        
    end subroutine ax_plus_y_r8



    !> A subroutine to the Cholesky decomposition of a real symmetric matrix
    !> \todo Optimization using matrix flattening
    !> \return retuns lower triangular matrix
    !> \param matrix_lower lower triangular matrix
    !> \param matrix input positive definite symmetrix matrix
    !> \param rank of input/output matrix
    subroutine cholesky_decomposition_r4(matrix_lower, matrix, n_dim)
        implicit none
        real(kind=4), intent(inout) :: matrix_lower(n_dim, n_dim)
        real(kind=4), intent(in)    :: matrix(n_dim, n_dim)
        integer(kind=4), intent(in) :: n_dim
        integer(kind=4)             :: i, j, k
        real(kind=4)                :: tmp_sum

        include "./include/linalg_cholesky_decomposition/inc_cholesky_decomposition_detail.f90"
    end subroutine cholesky_decomposition_r4
    include "./include/linalg_cholesky_decomposition/inc_cholesky_decomposition.f90"


    !> A subroutine to the Modified Cholesky decomposition of a real symmetric matrix
    !> \todo Optimization using matrix flattening
    !> \return retuns unit lower triangular matrix and diagonal elements
    !> \param matrix_lower unit lower triangular matrix
    !> \param diagonal_elements diagonal elements
    !> \param matrix input positive definite symmetrix matrix
    !> \param rank of input/output matrix
    subroutine cholesky_decomposition_modified_r4(matrix_lower, diagonal_elements, matrix, n_dim)
        implicit none
        real(kind=4), intent(inout) :: matrix_lower(n_dim, n_dim)
        real(kind=4), intent(inout) :: diagonal_elements(n_dim)
        real(kind=4), intent(in)    :: matrix(n_dim, n_dim)
        integer(kind=4), intent(in) :: n_dim
        integer(kind=4)             :: i, j, k
        real(kind=4)                :: tmp_sum, tmp_minus

        include "./include/linalg_cholesky_decomposition_modified/inc_cholesky_decomposition_modified_detail.f90"
    end subroutine cholesky_decomposition_modified_r4
    include "./include/linalg_cholesky_decomposition_modified/inc_cholesky_decomposition_modified.f90"


    !> A function to calculate inner product
    !> \return retuns inner product
    !> \param vector_1 1-dim array 
    !> \param vector_2 1-dim array
    !> \param num size of vector_1 and vector_2
    function inner_product_r4(vector_1, vector_2, num)
        real(kind=4), intent(in)    :: vector_1(num), vector_2(num)
        integer(kind=4), intent(in) :: num
        real(kind=4)                :: inner_product_r4
        real(kind=4)                :: tmp_sum

        include "./include/linalg_inner_product/inc_inner_product_detail.f90"
        inner_product_r4 = tmp_sum
    end function inner_product_r4
    include "./include/linalg_inner_product/inc_inner_product.f90"


    !> A subroutine to calculate inverse matrix of unit lower matrix,
    !> unit lower matrix is calculated by 'cholesky_decomposition' or 'cholesky_decomposition_modified'.
    !> \return retuns inverse matrix of unit lower matrix
    !> \param matrix_inv inverse matrix of unit lower matrix
    !> \param matrix_unit_lower unit lower matrix
    !> \param n_dim rank of input/output matrix
    subroutine inv_unit_lower_matrix_r4(matrix_inv, matrix_unit_lower, n_dim)
        implicit none
        real(kind=4), intent(inout) :: matrix_inv(n_dim, n_dim)
        real(kind=4), intent(inout) :: matrix_unit_lower(n_dim, n_dim)
        integer(kind=4), intent(in) :: n_dim

        integer(kind=4) :: i, j

        include "./include/linalg_inv_unit_lower_matrix/inc_inv_unit_lower_matrix_detail.f90"
    end subroutine inv_unit_lower_matrix_r4
    include "./include/linalg_inv_unit_lower_matrix/inc_inv_unit_lower_matrix.f90"


    !> A subroutine to calculate positive definite symmetric matrix inversion
    !! \param mat_inv inversion of input matrix 'mat'
    !! \param mat input matrix
    !! \param n_columns rank of 'mat'
    subroutine inversion_r8(mat_inv, mat, n_columns)
        implicit none
        real(kind=8), intent(inout) :: mat_inv(n_columns,n_columns)
        real(kind=8), intent(in)    :: mat(n_columns,n_columns)
        integer(kind=8)             :: n_columns

        real(kind=8), allocatable :: mat_lower(:,:), diagonals(:)
        real(kind=8), allocatable :: mat_lower_inv(:,:), diagonals_inv(:,:)
        integer(kind=8) :: i

        allocate(mat_lower(n_columns, n_columns))
        allocate(diagonals(n_columns))
        call cholesky_decomposition_modified(mat_lower, diagonals, mat, n_columns)

        ! Compute inverse of unit lower triangular matrix
        allocate(mat_lower_inv(n_columns, n_columns))
        call inv_unit_lower_matrix(mat_lower_inv, mat_lower, n_columns)

        ! Compute inverse of X^T X
        allocate(diagonals_inv(n_columns, n_columns))
        call identity(diagonals_inv, n_columns)
        do i=1, n_columns, 1
            diagonals_inv(i,i) = 1.0d0 / diagonals(i)
        end do
        mat_inv = matmul(matmul(transpose(mat_lower_inv), diagonals_inv), mat_lower_inv)
    end subroutine inversion_r8


    !> A subroutine to compute the product of tranposed matrix and original matrix
    !! \return returns the product of tranposed matrix and original matrix
    !! \param mat_out computed matrix product
    !! \param mat_in input matrix
    !! \param n_rows the number of rows of 'mat_in'
    !! \param n_cols the number of columns of 'mat_in'
    subroutine mattxmat_r4(mat_out, mat_in, n_samples, n_columns, with_intercept)
        implicit none
        real(kind=4), ALLOCATABLE, intent(inout) :: mat_out(:,:)
        real(kind=4), intent(in)               :: mat_in(n_samples, n_columns)
        integer(kind=4), intent(in)            :: n_samples, n_columns
        logical(kind=4), intent(in)            :: with_intercept
        real(kind=4), allocatable              :: col_sums(:)
        real(kind=4)                           :: tmp_sums(3), tmp_sum, tmp_val
        integer(kind=4) :: buffer_size=3

        integer(kind=4) :: i, j, k, l, one=1
        integer(kind=4) :: n_columns_unroll, j_unroll
        real(kind=4)    :: zero=0
        include "./include/linalg_mattxmat/inc_linalg_mattxmat_detail.f90"
    end subroutine mattxmat_r4
    include "./include/linalg_mattxmat/inc_linalg_mattxmat.f90"    

    !> A subroutine to calculate square sum of matrix by row.
    !! \return square sum of matrix by row
    !! \param matrix input matrix
    !! \param matrix_sqsum_vals calculated square sum values of matrix by row
    !! \param n_samples number of samples
    !! \param n_columns number of columns
    subroutine matrix_sqsum_row_r8(matrix, matrix_sqsum_vals, n_samples, n_columns)
        implicit none
        real(kind=8), intent(in)    :: matrix(n_samples, n_columns)
        real(kind=8), intent(inout) :: matrix_sqsum_vals(n_samples)
        integer(kind=8), intent(in) :: n_samples, n_columns
    end subroutine matrix_sqsum_row_r8
    include "./include/linalg_matrix_sqsum_row/inc_matrix_sqsum_row.f90"



    !> A subroutine to multiply matrix(M,K) and vector(K).
    !> \return returns multiply matrix(M,K) and vector(K).
    !> \param matrix input matrix
    !> \param input_vector input vector
    !> \param output_vector output vector
    !> \param n_rows number of rows in the matrix and size of 'output_vector'
    !> \param n_cols number of columns in the matrix and size of 'input_vector'
    subroutine multi_mat_vec_r4(matrix, input_vector, output_vector, n_rows, n_cols)
        implicit none
        real(kind=4), intent(in)      :: matrix(n_rows, n_cols)
        real(kind=4), intent(in)      :: input_vector(n_cols)
        real(kind=4), intent(inout)   :: output_vector(n_rows)
        integer(kind=4), intent(in) :: n_rows, n_cols

        integer(kind=4) :: i, j, k, n_cols_unroll
        real(kind=4)      :: tmp_out, tmp_input, buffer_input(cache_size_multi_mat_vec)
        output_vector = 0

        include "./include/linalg_multi_mat_vec/inc_multi_mat_vec_detail.f90"
    end subroutine multi_mat_vec_r4
    include "./include/linalg_multi_mat_vec/inc_multi_mat_vec.f90"
    include "./include/linalg_multi_mat_vec/inc_multi_mat_vec_variants.f90"


    subroutine multi_mat_vec_with_indices_r8(matrix, input_vector, output_vector, indices, n_rows, n_cols, n_samples)
        implicit none
        real(kind=8), intent(in)      :: matrix(n_rows, n_cols)
        real(kind=8), intent(in)      :: input_vector(n_cols)
        real(kind=8), intent(inout)   :: output_vector(n_samples)
        integer(kind=8), intent(in)   :: indices(n_samples)
        integer(kind=8), intent(in) :: n_rows, n_cols, n_samples

        integer(kind=8) :: i, j, k, n_cols_unroll, idx
        real(kind=8)      :: tmp_out, tmp_input, buffer_input(cache_size_multi_mat_vec)
        output_vector = 0

        n_cols_unroll = n_cols - mod(n_cols, cache_size_multi_mat_vec)
        do j=1, n_cols_unroll, cache_size_multi_mat_vec
            do k=0, cache_size_multi_mat_vec-1, 1
                buffer_input(k+1) = input_vector(j+k)
            end do

            do i=1, n_samples, 1
                tmp_out = 0
                idx = indices(i)
                do k=0, cache_size_multi_mat_vec-1, 1
                    tmp_out = tmp_out + matrix(idx,j+k) * buffer_input(k+1)
                end do
                output_vector(i) = output_vector(i) + tmp_out
            end do
        end do

        do j=n_cols_unroll+1, n_cols, 1
            tmp_input = input_vector(j)
            do i=1, n_samples, 1
                idx = indices(i)
                output_vector(i) = output_vector(i) + matrix(idx,j) * tmp_input
            end do
        end do
    end subroutine multi_mat_vec_with_indices_r8


    !> A subroutine to rotate input matrix from both side.
    !> matrix_rotated = transpose(rotation_matrix) x matrix x rotation_matrix.
    !! \return returns rotated matrix
    !! \param mat input squared matrix to be rotated
    !! \param n_dim rank of matrix
    !! \param loc location of columns to be rotated
    !! \param c_theta cos(theta)
    !! \param s_theta sin(theta)
    subroutine rotate_from_both_side_r4(mat, n_dim, loc, c_theta, s_theta)
        implicit none
        real(kind=4), intent(inout) :: mat(n_dim, n_dim)
        integer(kind=4), intent(in) :: n_dim
        integer(kind=4), intent(in) :: loc(2)
        real(kind=4), intent(in)    :: c_theta, s_theta

        integer(kind=4) :: i, p, q, posi
        real(kind=4)    :: mat_p, mat_q
        include "./include/linalg_rotate_from_both_side/inc_rotate_from_both_side_detail.f90"
    end subroutine rotate_from_both_side_r4
    include "./include/linalg_rotate_from_both_side/inc_rotate_from_both_side.f90"

    !> A subroutine to rotate input matrix from left.
    !> matrix_rotated = matrix x rotation_matrix.
    !! \return returns rotated matrix
    !! \param mat input squared matrix to be rotated
    !! \param n_dim rank of matrix
    !! \param loc location of columns to be rotated
    !! \param c_theta cos(theta)
    !! \param s_theta sin(theta)
    subroutine rotate_from_left_r4(mat, n_dim, loc, c_theta, s_theta)
        implicit none
        real(kind=4), intent(inout) :: mat(n_dim, n_dim)
        integer(kind=4), intent(in) :: n_dim
        integer(kind=4), intent(in) :: loc(2)
        real(kind=4), intent(in)    :: c_theta, s_theta

        integer(kind=4) :: i, p, q
        real(kind=4)    :: mat_p, mat_q
        include "./include/linalg_rotate_from_left/inc_rotate_from_left_detail.f90"
    end subroutine rotate_from_left_r4
    include "./include/linalg_rotate_from_left/inc_rotate_from_left.f90"


    !> A subroutine to rotate input matrix from right.
    !> matrix_rotated = transpose(rotation_matrix) x matrix.
    !! \return returns rotated matrix
    !! \param mat input squared matrix to be rotated
    !! \param n_dim rank of matrix
    !! \param loc location of columns to be rotated
    !! \param c_theta cos(theta)
    !! \param s_theta sin(theta)
    subroutine rotate_from_right_r4(mat, n_dim, loc, c_theta, s_theta)
        implicit none
        real(kind=4), intent(inout) :: mat(n_dim, n_dim)
        integer(kind=4), intent(in) :: n_dim
        integer(kind=4), intent(in) :: loc(2)
        real(kind=4), intent(in)    :: c_theta, s_theta

        integer(kind=4) :: i, p, q
        real(kind=4)    :: mat_p, mat_q
        include "./include/linalg_rotate_from_right/inc_rotate_from_right_detail.f90"
    end subroutine rotate_from_right_r4
    include "./include/linalg_rotate_from_right/inc_rotate_from_right.f90"


    !> A subroutine to generate random rotation matrix. For detail, see below.
    !> http://article.sapub.org/10.5923.j.ajcam.20170702.04.html
    !! \return returns random rotation matrix
    !! \param rotation_matrix generated random rotation matrix
    !! \param n_dim rank of rotation_matrix
    subroutine random_rotation_r4(rotation_matrix, n_dim)
        implicit none
        real(kind=4), intent(inout) :: rotation_matrix(n_dim, n_dim)
        integer(kind=4), intent(in) :: n_dim

        real(kind=4) :: x(n_dim), y(n_dim) ! y = rotation_matrix * x
        real(kind=4) :: xx(n_dim) ! y = rotation_matrix * x
        real(kind=4) :: c_theta, s_theta, x_norm, y_norm
        integer(kind=4) :: loc(2), i
        real(kind=4) :: rotation_matrix_y(n_dim, n_dim), y_pre, y_c, y_s
        real(kind=4) :: one, two
        one = 1.0
        two = 2.0
        include "./include/linalg_rotation_matrix/inc_rotation_matrix_detail.f90"
    end subroutine random_rotation_r4
    include "./include/linalg_rotation_matrix/inc_rotation_matrix.f90"


    !> A function to compute supremum eigen value for symmetric matrix
    !> \return returns supremum eigen value
    !> \param matrix input symmetric matrix
    !> \param n_dim rank of matrix
    function supremum_eigen_value_r4(matrix, n_dim)
        implicit none
        real(kind=4), intent(in) :: matrix(n_dim, n_dim)
        integer(kind=4), intent(in) :: n_dim
        real(kind=4) :: supremum_eigen_value_r4
        real(kind=4) :: tmp
        include "./include/linalg_supremum_eigen_value/inc_supremum_eigen_value_detail.f90"
        supremum_eigen_value_r4 = tmp
    end function supremum_eigen_value_r4
    include "./include/linalg_supremum_eigen_value/inc_supremum_eigen_value.f90"


    !> A subroutine to normalize vector
    subroutine vector2sum1_r4(vector, n_samples)
        implicit none
        real(kind=4), intent(inout) :: vector(n_samples)
        integer(kind=4), intent(in) :: n_samples
        real(kind=4)                :: sum_vec
        integer(kind=4)             :: n
        include "./include/linalg_vector2sum1/inc_vector2sum1_detail.f90"
    end subroutine vector2sum1_r4
    include "./include/linalg_vector2sum1/inc_vector2sum1.f90"




end module mod_linalg