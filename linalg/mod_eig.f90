module mod_eig
    use mod_stats
    use mod_linalg
    implicit none
    
    type eigen_system
        real(kind=4), allocatable :: eigen_values_r4(:)
        real(kind=8), allocatable :: eigen_values_r8(:)
        real(kind=4), allocatable :: eigen_vectors_r4(:,:)
        real(kind=8), allocatable :: eigen_vectors_r8(:,:)
    contains
        procedure :: dealloc_all_eigen_system
    end type eigen_system

    !> A type for Jacobi Method
    type jacobi_method
        integer(kind=8)   :: max_iter=100000
        real(kind=8)      :: tol = 1d-12
    contains
        procedure, pass :: fit_jacobi_r4
        procedure, pass :: fit_jacobi_r8
        generic         :: fit => fit_jacobi_r4, fit_jacobi_r8
    end type jacobi_method

contains

    !> A subroutine to deallocate allocated arrays in 'eigen_system'.
    subroutine dealloc_all_eigen_system(this)
        implicit none
        class(eigen_system) :: this
        if (allocated(this%eigen_values_r4))  deallocate(this%eigen_values_r4)
        if (allocated(this%eigen_values_r8))  deallocate(this%eigen_values_r8)
        if (allocated(this%eigen_vectors_r4)) deallocate(this%eigen_vectors_r4)
        if (allocated(this%eigen_vectors_r8)) deallocate(this%eigen_vectors_r8)
    end subroutine dealloc_all_eigen_system

    !> A subroutine to fit 'jacobi_method'.
    !! \returns 'eig' with eigen values and vectors of 'mat'.
    !! \param eig eigen system to be stored eigen values and vectors
    !! \param mat symmetric matrix
    subroutine fit_jacobi_r4(this, eig, mat)
        implicit none
        class(jacobi_method)              :: this
        type(eigen_system), intent(inout) :: eig
        real(kind=4), intent(inout)       :: mat(:,:)

        integer(kind=4)           :: x_shape(2), n_rows, n_cols
        integer(kind=4)           :: iter, i
        integer(kind=4)           :: absmax_loc(2), min_idx, max_idx, loc(2)
        real(kind=4)              :: absmax_val, diag_vals(2), theta, c_theta, s_theta

        call eig%dealloc_all_eigen_system()

        x_shape = shape(mat)
        n_cols = x_shape(1)

        allocate(eig%eigen_values_r4(n_cols))
        allocate(eig%eigen_vectors_r4(n_cols, n_cols))
        call identity(eig%eigen_vectors_r4, n_cols)
        
        do iter=1, this%max_iter
            ! Location of absolute maxmimum value
            call get_abs_maxloc(absmax_loc, absmax_val, mat, n_cols)
            if (abs(absmax_val) .le. this%tol) exit
            min_idx = minval(absmax_loc)
            max_idx = maxval(absmax_loc)
            diag_vals(1) = mat(min_idx, min_idx)
            diag_vals(2) = mat(max_idx, max_idx)

            ! Compute angle 'theta'
            theta = 0.5d0 * atan(2d0 * absmax_val / (diag_vals(1)-diag_vals(2)))
            if (diag_vals(1) .eq. diag_vals(2)) theta = pi_ * 0.25d0 
            c_theta = cos(theta)
            s_theta = sin(theta)
            
            ! Fast Rotate From Left and Right at the same time
            loc = (/min_idx, max_idx/)
            call rotate_from_left(eig%eigen_vectors_r4, n_cols, loc, c_theta, s_theta)
            call rotate_from_both_side(mat, n_cols, loc, c_theta, s_theta)
        end do

        do i=1, n_cols, 1
            eig%eigen_values_r4(i) = mat(i,i)
        end do
    end subroutine fit_jacobi_r4
    include "./include/eig_fit_jacobi/inc_fit_jacobi.f90"


end module mod_eig
