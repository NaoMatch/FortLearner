module mod_kmeans
    use mod_const
    use mod_common
    use mod_error
    use mod_random
    use mod_sort
    use mod_stats
    use mod_timer
    use mod_hyperparameter
    use mod_linalg
    use mod_math
    use mod_base_kmeans
    use mod_data_holder
    implicit none
    
    !> A type for kmeans. initial centroids are selected by 'kmeans++'.
    type, extends(base_kmeans) :: kmeans
    contains
        procedure, pass :: fit_elkan_x
        procedure, pass :: fit_elkan_dholder
        generic :: fit_elkan => fit_elkan_x, fit_elkan_dholder
        procedure, pass :: fit_kmeans_x
        procedure, pass :: fit_kmeans_dholder
        generic :: fit => fit_kmeans_x, fit_kmeans_dholder
        procedure :: fit_slow => fit_slow_kmeans
        procedure, pass :: fit_dgemm_x
        procedure, pass :: fit_dgemm_dholder
        generic :: fit_dgemm => fit_dgemm_x, fit_dgemm_dholder
        procedure, pass :: fit_dgemv_x
        procedure, pass :: fit_dgemv_dholder
        generic :: fit_dgemv => fit_dgemv_x, fit_dgemv_dholder
    end type kmeans

    !> An interface to create new 'kmeans' object.
    interface kmeans
        module procedure :: new_kmeans
    end interface kmeans

contains

    !> A function to create new 'kmeans' object
    !! \param n_cluster number of clusters. must be greater equal 2
    !! \param max_iter maximum number of iteration. must be greater equal 2
    !! \param tolerance maximum tolerance
    function new_kmeans(n_clusters, max_iter, tolerance, random_state)
        implicit none
        integer(kind=8), optional :: n_clusters
        integer(kind=8), optional :: max_iter
        real(kind=8), optional    :: tolerance
        integer(kind=8), optional :: random_state
        type(kmeans)              :: new_kmeans, tmp

        tmp%algo_name = "kmeans"
        tmp%hparam%algo_name = "kmeans"
        tmp%fix_seed = f_
        if ( present(n_clusters) ) tmp%hparam%n_clusters = n_clusters
        if ( present(max_iter) )   tmp%hparam%max_iter   = max_iter
        if ( present(tolerance) )  tmp%hparam%tolerance  = tolerance
        if ( present(random_state) )   then
            tmp%hparam%random_state = random_state
            tmp%fix_seed = t_
        end if

        call tmp%hparam%validate_int_range("n_clusters", tmp%hparam%n_clusters, 2_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_iter",   tmp%hparam%max_iter,   2_8, huge(1_8))
        call tmp%hparam%validate_real_range("tolerance", tmp%hparam%tolerance,epsilon_, 1d0)
        new_kmeans = tmp
    end function new_kmeans

    !> A subroutine to fit 'kmeans' object by very naive method.
    !! \param x data to be fitted
    subroutine fit_slow_kmeans(this, x)
        class(kmeans) :: this
        real(kind=8), intent(in) :: x(:,:)

        integer(kind=8)              :: x_shape(2), iter, c, i, idx
        real(kind=8), allocatable    :: distance_from_cluster_center(:,:)
        real(kind=8), allocatable    :: new_cluster_centers(:,:), old_cluster_centers(:,:), dsp_cluster_centers(:)
        real(kind=8), allocatable    :: diff_update(:,:)
        integer(kind=8), allocatable :: nearest_cluster_indices(:), counter(:)

        integer(kind=8) :: date_value1(8), date_value2(8)
        print*, "Warning: 'kmeans%fit_slow(x)' is very slow for benchmark only, DO NOT USE!!!!"

        call ifdealloc(this%cluster_centers)

        x_shape = shape(x)
        this % n_samples = x_shape(1)
        this % n_columns = x_shape(2)

        call this%select_initial_clusters_slow(x)

        allocate(distance_from_cluster_center(this%n_samples, this%hparam%n_clusters))
        allocate(old_cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(new_cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(dsp_cluster_centers(this%hparam%n_clusters))

        allocate(diff_update(this%n_columns, this%hparam%n_clusters))
        allocate(nearest_cluster_indices(this%n_samples))
        allocate(counter(this%hparam%n_clusters))

        old_cluster_centers(:,:) = this%cluster_centers(:,:)
        do iter=1, this%hparam%max_iter, 1
            ! Calculate Distance From Cluster Centers, and Assign Cluster Index
            distance_from_cluster_center(:,:) = 0d0
            do c=1, this%hparam%n_clusters, 1
                call calculate_distance_from_center_slow(x, old_cluster_centers(:,c), distance_from_cluster_center(:,c), & 
                    this%n_samples, this%n_columns)
            end do
            nearest_cluster_indices(:) = minloc(distance_from_cluster_center(:,:), dim=2)

            ! Update Cluster Center Coordinates
            counter(:) = 0_8
            new_cluster_centers(:,:) = 0d0
            do i=1, this%n_samples, 1
                idx = nearest_cluster_indices(i)
                new_cluster_centers(:,idx) = new_cluster_centers(:,idx) + x(i,:)
                counter(idx) = counter(idx) + 1_8
            end do
            
            do c=1, this%hparam%n_clusters, 1
                new_cluster_centers(:,c) = new_cluster_centers(:,c)/dble(counter(c))
                diff_update(:,c) = abs(new_cluster_centers(:,c)-old_cluster_centers(:,c))
            end do
            dsp_cluster_centers(:) = sqrt(sum( (new_cluster_centers(:,:) - old_cluster_centers(:,:))**2d0, dim=1))
            ! print*, "Iteration: ", iter, dsp_cluster_centers
            old_cluster_centers(:,:) = new_cluster_centers(:,:)

            ! print*, "Iteration: ", iter, maxval(sqrt(sum(diff_update**2d0, dim=1)))
            if (maxval(sqrt(sum(diff_update**2d0, dim=1))) .le. this%hparam%tolerance) exit
        end do
        this % is_trained = t_
        this % cluster_centers(:,:) = new_cluster_centers(:,:)

        allocate(this%cluster_centers_t(this%hparam%n_clusters, this%n_columns))
        this % cluster_centers_t(:,:) = transpose(new_cluster_centers(:,:))
    end subroutine fit_slow_kmeans

    !> A subroutine to fit 'kmeans' object by optimized(heuristic) method.
    !! \param x data to be fitted
    subroutine fit_kmeans_x(this, x, centers)
        class(kmeans) :: this
        real(kind=8), intent(in) :: x(:,:)
        real(kind=8), optional, intent(in) :: centers(:,:)

        integer(kind=8)              :: x_shape(2), iter, c, i, idx, idx_old
        integer(kind=8)              :: input_shape(2)
        real(kind=8), allocatable    :: distance_from_cluster_center(:,:)
        real(kind=8), allocatable    :: new_cluster_centers(:,:), old_cluster_centers(:,:)
        real(kind=8), allocatable    :: diff_update(:,:)
        real(kind=8), allocatable    :: x_sq_sum_row(:)
        integer(kind=8), allocatable :: nearest_cluster_indices(:), nearest_cluster_indices_old(:), counter(:)

        if (this%fix_seed) call fix_random_seed(this%hparam%random_state)
        x_shape = shape(x)
        this % n_samples = x_shape(1)
        this % n_columns = x_shape(2)

        allocate(x_sq_sum_row(this%n_samples))
        x_sq_sum_row(:) = 0d0
        call matrix_sqsum_row(x, x_sq_sum_row, this % n_samples, this % n_columns)
        call ifdealloc(this%cluster_centers)
        if (present(centers)) then
            input_shape(:) = shape(centers)
            if (input_shape(2) .ne. this % n_columns) then
                stop "Shape Mismatch, x and centers."
            end if
            allocate(this%cluster_centers(input_shape(2), input_shape(1)))
            this%cluster_centers = transpose(centers(:,:))
            this%hparam%n_clusters = input_shape(1)
        else
            call this%select_initial_clusters(x, x_sq_sum_row, this%n_samples, this%n_columns)
        end if

        allocate(distance_from_cluster_center(this%n_samples, this%hparam%n_clusters))
        allocate(old_cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(new_cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(diff_update(this%n_columns, this%hparam%n_clusters))
        allocate(nearest_cluster_indices(this%n_samples), nearest_cluster_indices_old(this%n_samples))
        allocate(counter(this%hparam%n_clusters))

        nearest_cluster_indices_old(:) = -1
        old_cluster_centers(:,:) = this%cluster_centers(:,:)
        do iter=1, this%hparam%max_iter, 1
            ! Calculate Distance From Cluster Centers, and Assign Cluster Index
            distance_from_cluster_center(:,:) = 0d0
            do c=1, this%hparam%n_clusters, 1
                call calculate_distance_from_center_dgemv(x, x_sq_sum_row, old_cluster_centers(:,c), & 
                    distance_from_cluster_center(:,c), this%n_samples, this%n_columns)
                ! call calculate_distance_from_center_slow(x, old_cluster_centers(:,c), & 
                !     distance_from_cluster_center(:,c), this%n_samples, this%n_columns)
            end do
            nearest_cluster_indices(:) = minloc(distance_from_cluster_center(:,:), dim=2)

            ! Update Cluster Center Coordinates
            if (iter .eq. 1_8) then
                counter(:) = 0_8
                new_cluster_centers(:,:) = 0d0
                do i=1, this%n_samples, 1
                    idx = nearest_cluster_indices(i)
                    new_cluster_centers(:,idx) = new_cluster_centers(:,idx) + x(i,:)
                    counter(idx) = counter(idx) + 1_8
                end do
            else
                do c=1, this%hparam%n_clusters, 1
                    new_cluster_centers(:,c) = new_cluster_centers(:,c)*dble(counter(c))
                end do
                do i=1, this%n_samples, 1
                    idx     = nearest_cluster_indices(i)
                    idx_old = nearest_cluster_indices_old(i)
                    if (idx .eq. idx_old) cycle
                    new_cluster_centers(:,idx_old) = new_cluster_centers(:,idx_old) - x(i,:)
                    new_cluster_centers(:,idx)     = new_cluster_centers(:,idx)     + x(i,:)
                    counter(idx_old) = counter(idx_old) - 1_8
                    counter(idx)     = counter(idx)     + 1_8
                end do
            end if
            nearest_cluster_indices_old = nearest_cluster_indices
            
            do c=1, this%hparam%n_clusters, 1
                new_cluster_centers(:,c) = new_cluster_centers(:,c)/dble(counter(c))
                diff_update(:,c) = abs(new_cluster_centers(:,c)-old_cluster_centers(:,c))
            end do
            old_cluster_centers(:,:) = new_cluster_centers(:,:)

            ! print*, "Iteration: ", iter, counter(:)
            if (maxval(sum(diff_update**2d0, dim=1)) .le. this%hparam%tolerance) exit
        end do
        this % is_trained = t_
        this % cluster_centers(:,:) = new_cluster_centers(:,:)

        if (allocated(this%cluster_centers_t)) deallocate(this%cluster_centers_t)
        allocate(this%cluster_centers_t(this%hparam%n_clusters, this%n_columns))
        this % cluster_centers_t(:,:) = transpose(new_cluster_centers(:,:))
        if (this%fix_seed) call release_random_seed()
    end subroutine fit_kmeans_x

    subroutine fit_kmeans_dholder(this, dholder, centers)
        class(kmeans) :: this
        type(data_holder), intent(in) :: dholder
        real(kind=8), optional, intent(in) :: centers(:,:)
        call this%fit_kmeans_x(dholder%x_ptr%x_r8_ptr)
    end subroutine fit_kmeans_dholder

    !> A subroutine to fit 'kmeans' object by optimized(heuristic) method and dgemv.
    !! \param x data to be fitted
    subroutine fit_dgemv_x(this, x)
        class(kmeans) :: this
        real(kind=8), intent(in) :: x(:,:)

        integer(kind=8)              :: x_shape(2), iter, c, i, idx, idx_old
        real(kind=8), allocatable    :: distance_from_cluster_center(:,:)
        real(kind=8), allocatable    :: new_cluster_centers(:,:), old_cluster_centers(:,:)
        real(kind=8), allocatable    :: diff_update(:,:)
        real(kind=8), allocatable    :: x_sq_sum_row(:)
        integer(kind=8), allocatable :: nearest_cluster_indices(:), nearest_cluster_indices_old(:), counter(:)

        if (this%fix_seed) call fix_random_seed(this%hparam%random_state)
        call ifdealloc(this%cluster_centers)

        x_shape = shape(x)
        this % n_samples = x_shape(1)
        this % n_columns = x_shape(2)

        allocate(x_sq_sum_row(this%n_samples))
        x_sq_sum_row(:) = 0d0
        call matrix_sqsum_row(x, x_sq_sum_row, this % n_samples, this % n_columns)
        call this%select_initial_clusters(x, x_sq_sum_row, this%n_samples, this%n_columns)

        allocate(distance_from_cluster_center(this%n_samples, this%hparam%n_clusters))
        allocate(old_cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(new_cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(diff_update(this%n_columns, this%hparam%n_clusters))
        allocate(nearest_cluster_indices(this%n_samples), nearest_cluster_indices_old(this%n_samples))
        allocate(counter(this%hparam%n_clusters))

        nearest_cluster_indices_old(:) = -1
        old_cluster_centers(:,:) = this%cluster_centers(:,:)
        do iter=1, this%hparam%max_iter, 1
            ! Calculate Distance From Cluster Centers, and Assign Cluster Index
            distance_from_cluster_center(:,:) = 0d0
            do c=1, this%hparam%n_clusters, 1
                call calculate_distance_from_center_dgemv(x, x_sq_sum_row, old_cluster_centers(:,c), & 
                    distance_from_cluster_center(:,c), this%n_samples, this%n_columns)
            end do
            nearest_cluster_indices(:) = minloc(distance_from_cluster_center(:,:), dim=2)

            ! Update Cluster Center Coordinates
            if (iter .eq. 1_8) then
                counter(:) = 0_8
                new_cluster_centers(:,:) = 0d0
                do i=1, this%n_samples, 1
                    idx = nearest_cluster_indices(i)
                    new_cluster_centers(:,idx) = new_cluster_centers(:,idx) + x(i,:)
                    counter(idx) = counter(idx) + 1_8
                end do
            else
                do c=1, this%hparam%n_clusters, 1
                    new_cluster_centers(:,c) = new_cluster_centers(:,c)*dble(counter(c))
                end do
                do i=1, this%n_samples, 1
                    idx     = nearest_cluster_indices(i)
                    idx_old = nearest_cluster_indices_old(i)
                    if (idx .eq. idx_old) cycle
                    new_cluster_centers(:,idx_old) = new_cluster_centers(:,idx_old) - x(i,:)
                    new_cluster_centers(:,idx)     = new_cluster_centers(:,idx)     + x(i,:)
                    counter(idx_old) = counter(idx_old) - 1_8
                    counter(idx)     = counter(idx)     + 1_8
                end do
            end if
            nearest_cluster_indices_old = nearest_cluster_indices
            
            do c=1, this%hparam%n_clusters, 1
                new_cluster_centers(:,c) = new_cluster_centers(:,c)/dble(counter(c))
                diff_update(:,c) = abs(new_cluster_centers(:,c)-old_cluster_centers(:,c))
            end do
            old_cluster_centers(:,:) = new_cluster_centers(:,:)

            ! print*, "Iteration: ", iter, counter(:)
            if (maxval(sum(diff_update**2d0, dim=1)) .le. this%hparam%tolerance) exit
        end do
        this % is_trained = t_
        this % cluster_centers(:,:) = new_cluster_centers(:,:)

        if (allocated(this%cluster_centers_t)) deallocate(this%cluster_centers_t)
        allocate(this%cluster_centers_t(this%hparam%n_clusters, this%n_columns))
        this % cluster_centers_t(:,:) = transpose(new_cluster_centers(:,:))
        call release_random_seed()
    end subroutine fit_dgemv_x

    subroutine fit_dgemv_dholder(this, dholder)
        class(kmeans) :: this
        type(data_holder), intent(in) :: dholder
        call this%fit_dgemv_x(dholder%x_ptr%x_r8_ptr)
    end subroutine fit_dgemv_dholder

    !> A subroutine to fit 'kmeans' object by optimized(heuristic) method and dgemm.
    !! \param x data to be fitted
    subroutine fit_dgemm_x(this, x)
        class(kmeans) :: this
        real(kind=8), intent(in) :: x(:,:)

        integer(kind=8)              :: x_shape(2), iter, c, i, idx, idx_old
        real(kind=8), allocatable    :: distance_from_cluster_center(:,:)
        real(kind=8), allocatable    :: new_cluster_centers(:,:), old_cluster_centers(:,:)
        real(kind=8), allocatable    :: diff_update(:,:)
        real(kind=8), allocatable    :: x_sq_sum_row(:)
        integer(kind=8), allocatable :: nearest_cluster_indices(:), nearest_cluster_indices_old(:), counter(:)

        if (this%fix_seed) call fix_random_seed(this%hparam%random_state)
        call ifdealloc(this%cluster_centers)

        x_shape = shape(x)
        this % n_samples = x_shape(1)
        this % n_columns = x_shape(2)

        allocate(x_sq_sum_row(this%n_samples))
        x_sq_sum_row(:) = 0d0
        call matrix_sqsum_row(x, x_sq_sum_row, this % n_samples, this % n_columns)
        call this%select_initial_clusters(x, x_sq_sum_row, this%n_samples, this%n_columns)
        ! x_sq_sum_row = 0d0

        allocate(distance_from_cluster_center(this%n_samples, this%hparam%n_clusters))
        allocate(old_cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(new_cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(diff_update(this%n_columns, this%hparam%n_clusters))
        allocate(nearest_cluster_indices(this%n_samples), nearest_cluster_indices_old(this%n_samples))
        allocate(counter(this%hparam%n_clusters))

        nearest_cluster_indices_old(:) = -1
        old_cluster_centers(:,:) = this%cluster_centers(:,:)
        do iter=1, this%hparam%max_iter, 1
            ! Calculate Distance From Cluster Centers, and Assign Cluster Index
            distance_from_cluster_center(:,:) = spread(sum(old_cluster_centers**2, dim=1), dim=1, ncopies=this % n_samples)
            call dgemm("N", "N", & 
                this % n_samples, this%hparam%n_clusters, this % n_columns, &
                -2d0, & 
                x, this % n_samples, &
                old_cluster_centers, this % n_columns, &
                1d0, &
                distance_from_cluster_center, this % n_samples)
            nearest_cluster_indices(:) = minloc(distance_from_cluster_center(:,:), dim=2)

            ! Update Cluster Center Coordinates
            if (iter .eq. 1_8) then
                counter(:) = 0_8
                new_cluster_centers(:,:) = 0d0
                do i=1, this%n_samples, 1
                    idx = nearest_cluster_indices(i)
                    new_cluster_centers(:,idx) = new_cluster_centers(:,idx) + x(i,:)
                    counter(idx) = counter(idx) + 1_8
                end do
            else
                do c=1, this%hparam%n_clusters, 1
                    new_cluster_centers(:,c) = new_cluster_centers(:,c)*dble(counter(c))
                end do
                do i=1, this%n_samples, 1
                    idx     = nearest_cluster_indices(i)
                    idx_old = nearest_cluster_indices_old(i)
                    if (idx .eq. idx_old) cycle
                    new_cluster_centers(:,idx_old) = new_cluster_centers(:,idx_old) - x(i,:)
                    new_cluster_centers(:,idx)     = new_cluster_centers(:,idx)     + x(i,:)
                    counter(idx_old) = counter(idx_old) - 1_8
                    counter(idx)     = counter(idx)     + 1_8
                end do
            end if
            nearest_cluster_indices_old = nearest_cluster_indices
            
            do c=1, this%hparam%n_clusters, 1
                new_cluster_centers(:,c) = new_cluster_centers(:,c)/dble(counter(c))
                diff_update(:,c) = abs(new_cluster_centers(:,c)-old_cluster_centers(:,c))
            end do
            old_cluster_centers(:,:) = new_cluster_centers(:,:)

            ! print*, "Iteration: ", iter, counter(:)
            if (maxval(sum(diff_update**2d0, dim=1)) .le. this%hparam%tolerance) exit
        end do
        this % is_trained = t_
        this % cluster_centers(:,:) = new_cluster_centers(:,:)

        if (allocated(this%cluster_centers_t)) deallocate(this%cluster_centers_t)
        allocate(this%cluster_centers_t(this%hparam%n_clusters, this%n_columns))
        this % cluster_centers_t(:,:) = transpose(new_cluster_centers(:,:))
        call release_random_seed()
    end subroutine fit_dgemm_x

    subroutine fit_dgemm_dholder(this, dholder)
        class(kmeans) :: this
        type(data_holder), intent(in) :: dholder
        call this%fit_dgemm_x(dholder%x_ptr%x_r8_ptr)
    end subroutine fit_dgemm_dholder

    !> A subroutine to fit 'kmeans' object by elkan's method.
    !! \param x data to be fitted
    subroutine fit_elkan_x(this, x)

        integer(kind=8) :: date_value1(8), date_value2(8)
        integer(kind=8) :: time_calculate_distance_from_center_slow
        integer(kind=8) :: time_update_cluster_centers
        integer(kind=8) :: time_update_lower_upper_bounds
        integer(kind=8) :: time_calculate_inter_cluster_distance
        integer(kind=8) :: time_update_cluster_indices

        class(kmeans) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8)              :: x_shape(2), iter, c, k, i, idx, idx_old, c_idx, counter, current_c_idx, new_c_idx, old_c_idx
        real(kind=8)                 :: dist, min_dist, upper_dist, lower_dist, inter_dist
        ! Triangle Inequality
        real(kind=8), allocatable    :: lower_bounds(:,:)         ! (n_samples, n_clusters)
        real(kind=8), allocatable    :: upper_bounds(:)           ! (n_samples)
        real(kind=8), allocatable    :: nearest_inter_cluster_distance(:) ! (n_clusters)

        real(kind=8), allocatable    :: x_sq_sum_row(:)                   ! (n_samples)
        real(kind=8), allocatable    :: cluster_sq_sum_row(:)                   ! (n_clusters)
        real(kind=8), allocatable    :: distance_from_cluster_center(:,:) ! (n_samples, n_clusters)
        real(kind=8), allocatable    :: old_cluster_centers(:,:)          ! (n_columns, n_clusters)
        real(kind=8), allocatable    :: new_cluster_centers(:,:)          ! (n_columns, n_clusters)
        real(kind=8), allocatable    :: dsp_cluster_centers(:)            ! (n_clusters)
        integer(kind=8), allocatable :: old_nearest_cluster_indices(:)          ! (n_samples)
        integer(kind=8), allocatable :: new_nearest_cluster_indices(:)          ! (n_samples)
        logical(kind=4), allocatable :: skip_calculation(:)               ! (n_samples)
        integer(kind=8), allocatable :: new_counters(:)               ! (n_clusters)
        logical(kind=4), allocatable :: r(:)               ! (n_samples)
        real(kind=8), allocatable    :: inter_cluster_distance(:,:)          ! (n_columns, n_clusters)

        time_calculate_distance_from_center_slow = 0_8
        time_update_cluster_centers = 0_8
        time_update_lower_upper_bounds = 0_8
        time_calculate_inter_cluster_distance = 0_8
        time_update_cluster_indices = 0_8

        if (this%fix_seed) call fix_random_seed(this%hparam%random_state)
        ! deallocate cluster center array
        call ifdealloc(this%cluster_centers)

        ! get data shapes
        x_shape = shape(x)
        this % n_samples = x_shape(1)
        this % n_columns = x_shape(2)

        ! calculate square sum of x by row
        allocate(x_sq_sum_row(this%n_samples))
        allocate(cluster_sq_sum_row(this%hparam%n_clusters))
        x_sq_sum_row(:) = 0d0
        call matrix_sqsum_row(x, x_sq_sum_row, this%n_samples, this%n_columns)

        ! select initial centers
        call this%select_initial_clusters(x, x_sq_sum_row, this%n_samples, this%n_columns)

        ! calculate distance matrix
        allocate(inter_cluster_distance(this%hparam%n_clusters, this%hparam%n_clusters))
        allocate(nearest_inter_cluster_distance(this%hparam%n_clusters))
        allocate(new_counters(this%hparam%n_clusters))
        allocate(skip_calculation(this%n_samples))
        allocate(r(this%n_samples))
        allocate(distance_from_cluster_center(this%n_samples, this%hparam%n_clusters))
        allocate(old_nearest_cluster_indices(this%n_samples))
        allocate(new_nearest_cluster_indices(this%n_samples))
        allocate(lower_bounds(this%n_samples, this%hparam%n_clusters))
        allocate(upper_bounds(this%n_samples))
        allocate(old_cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(new_cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(dsp_cluster_centers(this%hparam%n_clusters))
        old_cluster_centers(:,:) = this%cluster_centers(:,:)

        ! Calculate Distance from points and clusters
        call date_and_time(values=date_value1)
        do c=1, this%hparam%n_clusters, 1
            ! call calculate_distance_from_center_slow(x, old_cluster_centers(:,c), & 
            !     distance_from_cluster_center(:,c), this%n_samples, this%n_columns)
            call calculate_distance_from_center_dgemv_use_sqsum(x, x_sq_sum_row, old_cluster_centers(:,c), & 
                distance_from_cluster_center(:,c), this%n_samples, this%n_columns)
        end do

        call date_and_time(values=date_value2)
        time_calculate_distance_from_center_slow = time_calculate_distance_from_center_slow + time_diff(date_value1, date_value2)
        old_nearest_cluster_indices(:) = minloc(sqrt(distance_from_cluster_center(:,:)), dim=2)
        r(:) = f_

        ! Update Centroids
        call date_and_time(values=date_value1)
        new_counters(:) = 0_8
        new_nearest_cluster_indices(:) = -1_8
        call update_cluster_centers_diff_only(new_cluster_centers, old_cluster_centers, new_counters, x, & 
                new_nearest_cluster_indices, old_nearest_cluster_indices, & 
                this%n_samples, this%hparam%n_clusters, this%n_columns)
        dsp_cluster_centers(:) = sqrt(sum((new_cluster_centers(:,:) - old_cluster_centers(:,:))**2d0, dim=1))
        old_cluster_centers(:,:) = new_cluster_centers(:,:)
        call date_and_time(values=date_value2)
        time_update_cluster_centers = time_update_cluster_centers + time_diff(date_value1, date_value2)

        lower_bounds(:,:)          = sqrt(distance_from_cluster_center(:,:))
        upper_bounds(:)            = minval(sqrt(distance_from_cluster_center(:,:)), dim=2)
        old_nearest_cluster_indices(:) = minloc(distance_from_cluster_center(:,:), dim=2)

        do iter=1, this%hparam%max_iter, 1
            ! Update Lower&Upper Bound
            call date_and_time(values=date_value1)
            do i=1, this%n_samples, 1
                do c_idx=1, this%hparam%n_clusters, 1
                    lower_bounds(i,c_idx) = maxval((/lower_bounds(i,c_idx) - dsp_cluster_centers(c_idx), 0d0/))
                end do
                c_idx = old_nearest_cluster_indices(i)
                upper_bounds(i) = upper_bounds(i) + dsp_cluster_centers(c_idx)
            end do
            call date_and_time(values=date_value2)
            time_update_lower_upper_bounds = time_update_lower_upper_bounds + time_diff(date_value1, date_value2)

            call date_and_time(values=date_value1)
            call calculate_inter_cluster_distance(inter_cluster_distance, old_cluster_centers, &
                    this%hparam%n_clusters, this%n_columns)
            cluster_sq_sum_row(:) = sum( old_cluster_centers**2d0, dim=1 )
            call date_and_time(values=date_value2)
            time_calculate_inter_cluster_distance = time_calculate_inter_cluster_distance + time_diff(date_value1, date_value2)
            inter_cluster_distance(:,:)       = sqrt(inter_cluster_distance(:,:))
            nearest_inter_cluster_distance(:) = minval(inter_cluster_distance(:,:), dim=1)

            call date_and_time(values=date_value1)
            r(:) = t_
            counter=0
            new_nearest_cluster_indices(:) = old_nearest_cluster_indices(:)
            do i=1, this%n_samples, 1
                current_c_idx = new_nearest_cluster_indices(i)
                if (upper_bounds(i) .le. 0.5d0*nearest_inter_cluster_distance(current_c_idx)) cycle

                do c_idx=1, this%hparam%n_clusters, 1
                    if (c_idx .eq. current_c_idx) cycle
                    if (r(i)) then
                        counter = counter + 1
                        ! min_dist = sqrt( sum( (x(i,:)-old_cluster_centers(:,current_c_idx))**2d0 ))
                        min_dist = sqrt( x_sq_sum_row(i) + cluster_sq_sum_row(current_c_idx) & 
                            - 2d0 * sum(x(i,:)*old_cluster_centers(:,current_c_idx)) )
                        upper_bounds(i) = min_dist
                        r(i) = f_
                    else
                        min_dist = upper_bounds(i)
                    end if

                    upper_dist = upper_bounds(i)
                    lower_dist = lower_bounds(i,c_idx)
                    inter_dist = 0.5d0*inter_cluster_distance(current_c_idx, c_idx)
                    if (upper_dist        .gt. lower_dist) then
                        if (upper_dist    .gt. inter_dist) then
                            if ((min_dist .gt. lower_dist) .or. & 
                                (min_dist .gt. inter_dist)) then
                                    ! dist = sqrt( sum( (x(i,:)-old_cluster_centers(:,c_idx))**2d0 ))
                                    dist = sqrt( x_sq_sum_row(i) + cluster_sq_sum_row(c_idx) & 
                                            - 2d0 * sum(x(i,:)*old_cluster_centers(:,c_idx)) )
                                    lower_bounds(i,c_idx) = dist
                                    if (min_dist .gt. dist) then
                                        min_dist = dist
                                        upper_bounds(i) = min_dist
                                        new_nearest_cluster_indices(i) = c_idx
                                    end if
                            end if
                        end if
                    end if
                end do
            end do
            call date_and_time(values=date_value2)
            time_update_cluster_indices = time_update_cluster_indices + time_diff(date_value1, date_value2)


            call date_and_time(values=date_value1)
            call update_cluster_centers_diff_only(new_cluster_centers, old_cluster_centers, new_counters, x, & 
                    new_nearest_cluster_indices, old_nearest_cluster_indices, & 
                    this%n_samples, this%hparam%n_clusters, this%n_columns)
            call date_and_time(values=date_value2)
            time_update_cluster_centers = time_update_cluster_centers + time_diff(date_value1, date_value2)
            dsp_cluster_centers(:) = sqrt(sum( (new_cluster_centers(:,:) - old_cluster_centers(:,:))**2d0, dim=1))
            ! print*, "Iteration: ", iter, dsp_cluster_centers, &
            !     count(new_nearest_cluster_indices .ne. old_nearest_cluster_indices), &
            !     counter
            ! print*, "Iteration: ", iter, maxval(dsp_cluster_centers)
            if ( (maxval(dsp_cluster_centers) .le. this%hparam%tolerance) .and. (iter .gt. 10_8) ) exit
            old_cluster_centers(:,:)       = new_cluster_centers(:,:)
            old_nearest_cluster_indices(:) = new_nearest_cluster_indices(:)

            ! stop
        end do
        ! print*, "time_calculate_distance_from_center_slow: ", time_calculate_distance_from_center_slow
        ! print*, "time_update_cluster_centers             : ", time_update_cluster_centers
        ! print*, "time_update_lower_upper_bounds          : ", time_update_lower_upper_bounds
        ! print*, "time_calculate_inter_cluster_distance   : ", time_calculate_inter_cluster_distance
        ! print*, "time_update_cluster_indices             : ", time_update_cluster_indices
        this%is_trained = t_
        this%cluster_centers(:,:) = new_cluster_centers(:,:)

        if (allocated(this%cluster_centers_t)) deallocate(this%cluster_centers_t)
        allocate(this%cluster_centers_t(this%hparam%n_clusters, this%n_columns))
        this % cluster_centers_t(:,:) = transpose(new_cluster_centers(:,:))
        call release_random_seed()
    end subroutine fit_elkan_x

    subroutine fit_elkan_dholder(this, dholder)
        class(kmeans) :: this
        type(data_holder), intent(in) :: dholder
        call this%fit_elkan_x(dholder%x_ptr%x_r8_ptr)
    end subroutine fit_elkan_dholder



end module mod_kmeans
