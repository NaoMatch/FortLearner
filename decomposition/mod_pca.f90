module mod_pca
    use mod_stats
    use mod_linalg
    use mod_eig
    use mod_error
    use mod_sort
    use mod_const
    use mod_data_holder
    implicit none

    !> Type for Priciple Component Analysis
    type pca
        logical(kind=4)           :: is_fitted = f_           !< is trained or not. If not, cannot predict and dump.
        integer(kind=8)           :: n_columns                 !< number of columns
        type(eigen_system)        :: eig                       !< type for eigen_system to diagonalization covariance matirx
        real(kind=4), allocatable :: means_of_matrix_r4(:)     !< mean values of input explanatory variable by column (#columns), kind=4
        real(kind=8), allocatable :: means_of_matrix_r8(:)     !< mean values of input explanatory variable by column (#columns), kind=4
        real(kind=4), allocatable :: cov_mat_r4(:,:)           !< covariance matrix of input explanatory (#columns, #columns), kind=4
        real(kind=8), allocatable :: cov_mat_r8(:,:)           !< covariance matrix of input explanatory (#columns, #columns), kind=8
    contains
        procedure       :: dealloc_all
        procedure, pass :: fit_pca_x_r4
        procedure, pass :: fit_pca_x_r8
        procedure, pass :: fit_pca_dholder
        generic         :: fit => fit_pca_x_r4, fit_pca_x_r8, fit_pca_dholder
        procedure, pass :: transform_pca_x_r4
        procedure, pass :: transform_pca_x_r8
        procedure, pass :: transform_pca_dholder
        generic         :: transform => transform_pca_x_r4, transform_pca_x_r8, transform_pca_dholder
    end type pca

    !> An interface to create new 'pca' object
    interface pca
        procedure :: new_pca
    end interface pca

contains

    !> A function to create new 'pca' object
    function new_pca()
        implicit none
        type(pca) :: new_pca
    end function new_pca


    !> A subroutine to deallocate allocated arrays in 'pca'.
    subroutine dealloc_all(this)
        implicit none
        class(pca) :: this
        if (allocated(this%means_of_matrix_r4)) deallocate(this%means_of_matrix_r4)
        if (allocated(this%means_of_matrix_r8)) deallocate(this%means_of_matrix_r8)
        if (allocated(this%cov_mat_r4))         deallocate(this%cov_mat_r4)
        if (allocated(this%cov_mat_r8))         deallocate(this%cov_mat_r8)
    end subroutine dealloc_all


    !> A subroutine to fit 'pca'
    !! \returns fitted 'pca' object.
    !! \param x input 2-dim array
    subroutine fit_pca_x_r4(this, x)
        implicit none
        class(pca)                :: this
        real(kind=4), intent(in)  :: x(:,:)

        integer(kind=4)           :: x_shape(2), n_samples, n_columns
        integer(kind=4)           :: iter, i
        integer(kind=4)           :: absmax_loc(2), min_idx, max_idx, loc(2)
        real(kind=4)              :: absmax_val, diag_vals(2), theta, c_theta, s_theta
        real(kind=4), allocatable :: cov_mat_copy(:,:)
        type(jacobi_method)       :: jacobi
        integer(kind=4), allocatable :: eig_idx(:)

        call this%dealloc_all()

        x_shape = shape(x)
        n_samples = x_shape(1)
        n_columns = x_shape(2)

        allocate(this%means_of_matrix_r4(n_columns))
        this%means_of_matrix_r4 = mean(x, n_samples, n_columns)

        allocate(this%cov_mat_r4(n_columns, n_columns))
        call covariance_matrix(this%cov_mat_r4, x, n_samples, n_columns, means_of_matrix=this%means_of_matrix_r4)

        allocate(cov_mat_copy(n_columns, n_columns))
        cov_mat_copy = this%cov_mat_r4

        call jacobi%fit(this%eig, cov_mat_copy)

        eig_idx = (/(i, i=1, n_columns, 1)/)
        call quick_argsort(this%eig%eigen_values_r4, eig_idx, n_columns)
        eig_idx = eig_idx(n_columns:1:-1)
        this%eig%eigen_values_r4 = this%eig%eigen_values_r4(n_columns:1:-1)
        this%eig%eigen_vectors_r4 = this%eig%eigen_vectors_r4(:,eig_idx)

        deallocate(cov_mat_copy)
        this%is_fitted = t_
        this%n_columns = n_columns
    end subroutine fit_pca_x_r4
    include "./include/pca_fit_pca/inc_fit_pca.f90"

    subroutine fit_pca_dholder(this, dholder)
        implicit none
        class(pca) :: this
        type(data_holder), intent(in) :: dholder
        if (associated(dholder%x_ptr%x_r4_ptr)) then
            call this%fit_pca_x_r4(dholder%x_ptr%x_r4_ptr)
        elseif (associated(dholder%x_ptr%x_r8_ptr)) then
            call this%fit_pca_x_r8(dholder%x_ptr%x_r8_ptr)
        end if
    end subroutine fit_pca_dholder


    !> A function to transform by fitted 'pca'
    !! \returns transformed x
    !! \param x input 2-dim array
    function transform_pca_x_r4(this, x)
        implicit none
        class(pca)                :: this
        real(kind=4), intent(in)  :: x(:,:)
        real(kind=4), allocatable :: transform_pca_x_r4(:,:)

        integer(kind=4) :: x_shape(2), n_samples, n_columns, i
        type(error) :: err

        x_shape = shape(x)
        n_samples = x_shape(1)
        n_columns = x_shape(2)
        call err%check_estimator_is_fitted(this%is_fitted, "pca")
        call err%check_number_of_features_mismatch(int(this%n_columns, kind=kind(n_columns)), n_columns, "pca")

        allocate(transform_pca_x_r4(n_samples, n_columns))
        do i=1, n_columns
            call multi_mat_vec(x, this%eig%eigen_vectors_r4(:,i), transform_pca_x_r4(:,i), n_samples, n_columns)
            transform_pca_x_r4(:,i) = transform_pca_x_r4(:,i) - sum(this%eig%eigen_vectors_r4(:,i)*this%means_of_matrix_r4)
        end do
    end function transform_pca_x_r4
    include "./include/pca_transform_pca/inc_transform_pca.f90"

    function transform_pca_dholder(this, dholder)
        implicit none
        class(pca) :: this
        type(data_holder), intent(in) :: dholder
        real(kind=8), allocatable :: transform_pca_dholder(:,:)
        if (associated(dholder%x_ptr%x_r4_ptr)) then
            transform_pca_dholder = this%transform_pca_x_r4(dholder%x_ptr%x_r4_ptr)
        elseif (associated(dholder%x_ptr%x_r8_ptr)) then
            transform_pca_dholder = this%transform_pca_x_r8(dholder%x_ptr%x_r8_ptr)
        end if
    end function transform_pca_dholder

end module mod_pca
