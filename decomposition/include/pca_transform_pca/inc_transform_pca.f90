function transform_pca_r8(this, x)
    implicit none
    class(pca)                :: this
    real(kind=8), intent(in)  :: x(:,:)
    real(kind=8), allocatable :: transform_pca_r8(:,:)

    integer(kind=8) :: x_shape(2), n_samples, n_columns, i
    type(error) :: err

    x_shape = shape(x)
    n_samples = x_shape(1)
    n_columns = x_shape(2)
    call err%check_estimator_is_fitted(this%is_fitted, "pca")
    call err%check_number_of_features_mismatch(int(this%n_columns, kind=kind(n_columns)), n_columns, "pca")

    allocate(transform_pca_r8(n_samples, n_columns))
    do i=1, n_columns
        call multi_mat_vec(x, this%eig%eigen_vectors_r8(:,i), transform_pca_r8(:,i), n_samples, n_columns)
        transform_pca_r8(:,i) = transform_pca_r8(:,i) - sum(this%eig%eigen_vectors_r8(:,i)*this%means_of_matrix_r8)
    end do
end function transform_pca_r8
