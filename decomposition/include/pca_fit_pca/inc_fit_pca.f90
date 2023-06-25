subroutine fit_pca_x_r8(this, x)
    implicit none
    class(pca)                :: this
    real(kind=8), intent(in)  :: x(:,:)

    integer(kind=8)           :: x_shape(2), n_samples, n_columns
    integer(kind=8)           :: iter, i
    integer(kind=8)           :: absmax_loc(2), min_idx, max_idx, loc(2)
    real(kind=8)              :: absmax_val, diag_vals(2), theta, c_theta, s_theta
    real(kind=8), allocatable :: cov_mat_copy(:,:)
    type(jacobi_method)       :: jacobi
    integer(kind=8), allocatable :: eig_idx(:)

    call this%dealloc_all()

    x_shape = shape(x)
    n_samples = x_shape(1)
    n_columns = x_shape(2)

    allocate(this%means_of_matrix_r8(n_columns))
    this%means_of_matrix_r8 = mean(x, n_samples, n_columns)

    allocate(this%cov_mat_r8(n_columns, n_columns))
    call covariance_matrix(this%cov_mat_r8, x, n_samples, n_columns, means_of_matrix=this%means_of_matrix_r8)

    allocate(cov_mat_copy(n_columns, n_columns))
    cov_mat_copy = this%cov_mat_r8

    call jacobi%fit_jacobi_r8(this%eig, cov_mat_copy)

    eig_idx = (/(i, i=1, n_columns, 1)/)
    call quick_argsort(this%eig%eigen_values_r8, eig_idx, n_columns)
    eig_idx = eig_idx(n_columns:1:-1)
    this%eig%eigen_values_r8 = this%eig%eigen_values_r8(n_columns:1:-1)
    this%eig%eigen_vectors_r8 = this%eig%eigen_vectors_r8(:,eig_idx)

    deallocate(cov_mat_copy)
    this%is_fitted = t_
    this%n_columns = n_columns
end subroutine fit_pca_x_r8

