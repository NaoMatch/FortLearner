module mod_minibatch_kmeans
    use mod_const
    use mod_hyperparameter
    use mod_base_kmeans
    use mod_random
    implicit none


    type, extends(base_kmeans) :: minibatch_kmeans
    contains
        procedure :: fit => fit_minibatch_kmeans
    end type minibatch_kmeans

    !> An interface to create new 'kmeans' object.
    interface minibatch_kmeans
        module procedure :: new_minibatch_kmeans
    end interface minibatch_kmeans

contains
    !> A function to create new 'kmeans' object
    !! \param n_cluster number of clusters. must be greater equal 2
    !! \param max_iter maximum number of iteration. must be greater equal 2
    !! \param tolerance maximum tolerance
    function new_minibatch_kmeans(n_clusters, max_iter, tolerance, max_samples, random_state)
        implicit none
        integer(kind=8), optional :: n_clusters
        integer(kind=8), optional :: max_iter
        real(kind=8), optional    :: tolerance
        integer(kind=8), optional :: max_samples
        integer(kind=8), optional :: random_state
        type(minibatch_kmeans)    :: new_minibatch_kmeans, tmp

        tmp%fix_seed = f_
        tmp%algo_name = "minibatch_kmeans"
        tmp%hparam%algo_name = "minibatch_kmeans"
        if ( present(n_clusters) ) tmp%hparam%n_clusters = n_clusters
        if ( present(max_iter) )   tmp%hparam%max_iter   = max_iter
        if ( present(tolerance) )  tmp%hparam%tolerance  = tolerance
        if ( present(max_samples) )  tmp%hparam%max_samples  = max_samples
        if ( present(random_state) )   then
            tmp%hparam%random_state = random_state
            tmp%fix_seed = t_
        end if

        call tmp%hparam%validate_int_range("n_clusters", tmp%hparam%n_clusters, 2_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_iter",   tmp%hparam%max_iter,   2_8, huge(1_8))
        call tmp%hparam%validate_real_range("tolerance", tmp%hparam%tolerance,epsilon_, 1d0)
        call tmp%hparam%validate_int_range("max_samples",   tmp%hparam%max_samples,   tmp%hparam%n_clusters, huge(1_8))

        new_minibatch_kmeans = tmp
    end function new_minibatch_kmeans

    
    subroutine fit_minibatch_kmeans(this, x)
        implicit none
        class(minibatch_kmeans) :: this
        real(kind=8), intent(in) :: x(:,:)

        integer(kind=8)              :: x_shape(2), iter, c, i, j, idx, idx_old
        integer(kind=8)              :: input_shape(2)
        real(kind=8), allocatable    :: distance_matrix(:,:)
        real(kind=8), allocatable    :: x_sq_sum_row(:), c_sq_sum_row(:)
        real(kind=8), allocatable    :: x_minibatch(:,:), x_minibatch_sq_sum_row(:)
        integer(kind=8), allocatable :: counter(:), indices(:), cluster_labels(:)
        integer(kind=8)              :: label
        real(kind=8)                 :: eta
        if (this%fix_seed) call fix_random_seed(this%hparam%random_state)

        x_shape = shape(x)
        this % n_samples = x_shape(1)
        this % n_columns = x_shape(2)
        allocate(x_minibatch(this%hparam%max_samples, this%n_columns))
        allocate(x_minibatch_sq_sum_row(this%hparam%max_samples))
        allocate(x_sq_sum_row(this%n_samples))
        x_sq_sum_row(:) = 0d0
        call matrix_sqsum_row(x, x_sq_sum_row, this % n_samples, this % n_columns)
        call ifdealloc(this%cluster_centers)
        call this%select_initial_clusters(x, x_sq_sum_row, this%n_samples, this%n_columns)

        allocate(counter(this%hparam%n_clusters))
        allocate(distance_matrix(this%hparam%max_samples, this%hparam%n_clusters))
        allocate(c_sq_sum_row(this%hparam%n_clusters))
        allocate(indices(this%n_samples))
        allocate(cluster_labels(this%hparam%max_samples))
        do i=1, this%n_samples, 1
            indices(i) = i
        end do

        c_sq_sum_row(:) = sum(this%cluster_centers**2d0, dim=1)
        counter(:) = 1_8
        do iter=1, this%hparam%max_iter, 1
            call permutation(indices, this%n_samples)
            x_minibatch(:,:)          = x([indices(this%hparam%max_samples)],:)
            x_minibatch_sq_sum_row(:) = x_sq_sum_row(indices(this%hparam%max_samples))

            distance_matrix(:,:) = spread(x_minibatch_sq_sum_row, dim=2, ncopies=this%hparam%n_clusters) & 
                                 + spread(c_sq_sum_row,           dim=1, ncopies=this%hparam%max_samples)
            call dgemm("N", "N", & 
                this%hparam%max_samples, this%hparam%n_clusters, this % n_columns, &
                -2d0, & 
                x_minibatch, this%hparam%max_samples, &
                this%cluster_centers, this % n_columns, &
                1d0, &
                distance_matrix, this%hparam%max_samples)
                 
            cluster_labels(:) = minloc(distance_matrix, dim=2)

            do j=1, this%hparam%max_samples, 1
                label = cluster_labels(j)
                counter(label) = counter(label) + 1
                eta = 1d0 / counter(label)
                this%cluster_centers(:,label) = (1d0-eta) * this%cluster_centers(:,label) &
                    + eta * x_minibatch(j,:)
            end do
            c_sq_sum_row(:) = sum(this%cluster_centers**2d0, dim=1)
        end do
        this % is_trained = t_
        if (this%fix_seed) call release_random_seed()
    end subroutine fit_minibatch_kmeans

end module mod_minibatch_kmeans