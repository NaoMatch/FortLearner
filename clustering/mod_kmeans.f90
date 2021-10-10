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
    implicit none
    
    !> A type for kmeans. initial centroids are selected by 'kmeans++'.
    type kmeans
        ! base
        character(len=256) :: algo_name = "kmeans" !< algorithm name

        ! parameters
        type(hparam_kmeans) :: hparam !< type of hyperparameter for 'kmeans'

        ! train results
        integer(kind=8)              :: n_samples=-1               !< training results. #samples
        integer(kind=8)              :: n_columns=-1               !< training results. #columns
        logical(kind=4)              :: is_trained = f_            !< training results. already fitted or not
        real(kind=8), allocatable    :: cluster_centers(:,:)       !< training results. coordinates of cluster centroids. dimension=(#centroids, #columns)
    contains
        procedure :: fit_faster  => fit_faster_kmeans
        procedure :: fit_faster2 => fit_faster2_kmeans
        procedure :: fit      => fit_kmeans
        procedure :: fit_slow => fit_slow_kmeans
        procedure :: predict  => predict_kmeans
        procedure :: dump     => dump_kmeans
        procedure :: load     => load_kmeans
        procedure :: select_initial_clusters
        procedure :: select_initial_clusters_slow
        procedure :: score    => score_kmeans
    end type kmeans

    !> An interface to create new 'kmeans' object.
    interface kmeans
        module procedure :: new_kmeans
    end interface kmeans

contains

    function score_kmeans(this, x)
        implicit none
        class(kmeans)            :: this
        real(kind=8)             :: score_kmeans
        real(kind=8), intent(in) :: x(:,:)

        integer(kind=8), allocatable :: cluster_indices(:)
        integer(kind=8)              :: n_samples, n_columns, i, c_idx, step
        cluster_indices = this%predict(x)
        score_kmeans = 0d0
        n_samples = size(cluster_indices)
        do i=1, n_samples, 1
            c_idx = cluster_indices(i)
            score_kmeans = score_kmeans + sum( (x(i,:) - this%cluster_centers(:,c_idx))**2d0 )
        end do
    end function score_kmeans

    !> A function to create new 'kmeans' object
    !! \param n_cluster number of clusters. must be greater equal 2
    !! \param max_iter maximum number of iteration. must be greater equal 2
    !! \param tolerance maximum tolerance
    function new_kmeans(n_clusters, max_iter, tolerance)
        implicit none
        integer(kind=8), optional :: n_clusters
        integer(kind=8), optional :: max_iter
        real(kind=8), optional    :: tolerance
        type(kmeans)              :: new_kmeans, tmp

        tmp%hparam%algo_name = "kmeans"
        if ( present(n_clusters) ) tmp%hparam%n_clusters = n_clusters
        if ( present(max_iter) )   tmp%hparam%max_iter   = max_iter
        if ( present(tolerance) )  tmp%hparam%tolerance  = tolerance

        call tmp%hparam%validate_int_range("n_clusters", tmp%hparam%n_clusters, 2_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_iter",   tmp%hparam%max_iter,   2_8, huge(1_8))
        call tmp%hparam%validate_real_range("tolerance", tmp%hparam%tolerance,epsilon_, 1d0)
        new_kmeans = tmp
    end function new_kmeans

    !> A subroutine to dump fitted 'kmeans' object
    !> If not fitted, cannot dump training results.
    !! \param file_name output file name
    subroutine dump_kmeans(this, file_name)
        class(kmeans) :: this
        character(len=*) :: file_name
        open(10, file=file_name, form="unformatted", status="replace")
        if (.not. this % is_trained) then
            print*, trim(this % hparam % algo_name),  " is not trained. Cannot dump model."
            write(10) f_ ! dump fail
            close(10)
            stop
        end if
        write(10) t_ ! dump succes
        write(10) this%hparam%algo_name
        write(10) this%hparam%n_clusters
        write(10) this%hparam%max_iter
        write(10) this%hparam%tolerance
        write(10) this%n_columns
        write(10) this%cluster_centers
        close(10)
    end subroutine dump_kmeans

    !> A subroutine to load 'kmeans' object
    !> If not fitted, cannot load.
    !! \param file_name loading file name
    subroutine load_kmeans(this, file_name)
        implicit none
        class(kmeans)    :: this
        character(len=*) :: file_name
        logical(kind=4)  :: is_dump_successed
        open(10, file=file_name, form="unformatted")
        read(10) is_dump_successed
        if (.not. is_dump_successed) then
            print*, trim(this % algo_name),  " failed dump of the model."
            stop
        end if
        this%is_trained = is_dump_successed
        read(10) this%hparam%algo_name
        read(10) this%hparam%n_clusters
        read(10) this%hparam%max_iter
        read(10) this%hparam%tolerance
        read(10) this%n_columns
        allocate(this%cluster_centers(this%n_columns,this%hparam%n_clusters))
        read(10) this%cluster_centers
        close(10)
    end subroutine load_kmeans

    !> A subroutine to fit 'kmeans' object
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
    end subroutine fit_slow_kmeans

    subroutine fit_faster_kmeans(this, x)

        integer(kind=8) :: date_value1(8), date_value2(8)
        integer(kind=8) :: time_calculate_distance_from_center_slow
        integer(kind=8) :: time_update_cluster_centers
        integer(kind=8) :: time_update_lower_upper_bounds
        integer(kind=8) :: time_calculate_inter_cluster_distance
        integer(kind=8) :: time_update_cluster_indices

        class(kmeans) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8)              :: x_shape(2), iter, c, k, i, idx, idx_old, c_idx, counter, current_c_idx, new_c_idx
        real(kind=8)                 :: dist, min_dist, upper_dist, lower_dist
        ! Triangle Inequality
        real(kind=8), allocatable    :: lower_bounds(:,:)         ! (n_samples, n_clusters)
        real(kind=8), allocatable    :: upper_bounds(:)           ! (n_samples)
        real(kind=8), allocatable    :: nearest_inter_cluster_distance(:) ! (n_clusters)

        real(kind=8), allocatable    :: x_sq_sum_row(:)                   ! (n_samples)
        real(kind=8), allocatable    :: distance_from_cluster_center(:,:) ! (n_samples, n_clusters)
        real(kind=8), allocatable    :: old_cluster_centers(:,:)          ! (n_columns, n_clusters)
        real(kind=8), allocatable    :: new_cluster_centers(:,:)          ! (n_columns, n_clusters)
        real(kind=8), allocatable    :: dsp_cluster_centers(:)            ! (n_clusters)
        integer(kind=8), allocatable :: old_nearest_cluster_indices(:)          ! (n_samples)
        integer(kind=8), allocatable :: new_nearest_cluster_indices(:)          ! (n_samples)
        logical(kind=4), allocatable :: skip_calculation(:)               ! (n_samples)
        logical(kind=4), allocatable :: r(:)               ! (n_samples)
        real(kind=8), allocatable    :: inter_cluster_distance(:,:)          ! (n_columns, n_clusters)

        time_calculate_distance_from_center_slow = 0_8
        time_update_cluster_centers = 0_8
        time_update_lower_upper_bounds = 0_8
        time_calculate_inter_cluster_distance = 0_8
        time_update_cluster_indices = 0_8

        ! deallocate cluster center array
        call ifdealloc(this%cluster_centers)

        ! get data shapes
        x_shape = shape(x)
        this % n_samples = x_shape(1)
        this % n_columns = x_shape(2)

        ! calculate square sum of x by row
        allocate(x_sq_sum_row(this%n_samples))
        x_sq_sum_row(:) = 0d0
        call matrix_sqsum_row(x, x_sq_sum_row, this%n_samples, this%n_columns)

        ! select initial centers
        call this%select_initial_clusters(x, x_sq_sum_row, this%n_samples, this%n_columns)

        ! calculate distance matrix
        allocate(inter_cluster_distance(this%hparam%n_clusters, this%hparam%n_clusters))
        allocate(nearest_inter_cluster_distance(this%hparam%n_clusters))
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
        ! print*, '============================================================='
        ! print*, '============================================================='
        ! print*, '============================================================='
        ! print*, '============================================================='
        call date_and_time(values=date_value1)
        do c=1, this%hparam%n_clusters, 1
            call calculate_distance_from_center_slow(x, old_cluster_centers(:,c), & 
                distance_from_cluster_center(:,c), this%n_samples, this%n_columns)
        end do
        call date_and_time(values=date_value2)
        time_calculate_distance_from_center_slow = time_calculate_distance_from_center_slow + time_diff(date_value1, date_value2)
        old_nearest_cluster_indices(:) = minloc(sqrt(distance_from_cluster_center(:,:)), dim=2)
        r(:) = f_

        ! Update Centroids
        call date_and_time(values=date_value1)
        call update_cluster_centers(new_cluster_centers, x, old_nearest_cluster_indices, &
                this%n_samples, this%hparam%n_clusters, this%n_columns)
        dsp_cluster_centers(:) = sqrt(sum((new_cluster_centers(:,:) - old_cluster_centers(:,:))**2d0, dim=1))
        old_cluster_centers(:,:) = new_cluster_centers(:,:)
        call date_and_time(values=date_value2)
        time_update_cluster_centers = time_update_cluster_centers + time_diff(date_value1, date_value2)

        call date_and_time(values=date_value1)
        do c_idx=1, this%hparam%n_clusters, 1
            call calculate_distance_from_center_slow(x, old_cluster_centers(:,c_idx), & 
                distance_from_cluster_center(:,c_idx), this%n_samples, this%n_columns)
        end do
        call date_and_time(values=date_value2)
        time_calculate_distance_from_center_slow = time_calculate_distance_from_center_slow + time_diff(date_value1, date_value2)
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
                upper_bounds(i)       =          upper_bounds(i)       + dsp_cluster_centers(c_idx)
            end do
            call date_and_time(values=date_value2)
            time_update_lower_upper_bounds = time_update_lower_upper_bounds + time_diff(date_value1, date_value2)

            call date_and_time(values=date_value1)
            call calculate_inter_cluster_distance(inter_cluster_distance, old_cluster_centers, &
                    this%hparam%n_clusters, this%n_columns)
            call date_and_time(values=date_value2)
            time_calculate_inter_cluster_distance = time_calculate_inter_cluster_distance + time_diff(date_value1, date_value2)
            inter_cluster_distance(:,:)       = sqrt(inter_cluster_distance(:,:))
            nearest_inter_cluster_distance(:) = minval(inter_cluster_distance(:,:), dim=1)

            call date_and_time(values=date_value1)
            r(:) = t_
            new_nearest_cluster_indices(:) = old_nearest_cluster_indices(:)
            do i=1, this%n_samples, 1
                current_c_idx = new_nearest_cluster_indices(i)
                if (upper_bounds(i) .le. 0.5d0*nearest_inter_cluster_distance(current_c_idx)) cycle

                do c_idx=1, this%hparam%n_clusters, 1
                    if (c_idx .eq. current_c_idx) cycle
                    if (r(i)) then
                        min_dist = sqrt( sum( (x(i,:)-old_cluster_centers(:,current_c_idx))**2d0 ))
                        upper_bounds(i) = min_dist
                        r(i) = f_
                    else
                        min_dist = upper_bounds(i)
                    end if

                    if (upper_bounds(i)       .gt. lower_bounds(i,c_idx)) then
                        if (upper_bounds(i)   .gt. 0.5d0*inter_cluster_distance(current_c_idx, c_idx)) then
                            if ((min_dist .gt. lower_bounds(i,c_idx)) .or. & 
                                (min_dist .gt. 0.5d0*inter_cluster_distance(current_c_idx, c_idx))) then
                                    dist = sqrt( sum( (x(i,:)-old_cluster_centers(:,c_idx))**2d0 ))
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
            call update_cluster_centers(new_cluster_centers, x, new_nearest_cluster_indices, &
                    this%n_samples, this%hparam%n_clusters, this%n_columns)
            call date_and_time(values=date_value2)
            time_update_cluster_centers = time_update_cluster_centers + time_diff(date_value1, date_value2)
            dsp_cluster_centers(:) = sqrt(sum( (new_cluster_centers(:,:) - old_cluster_centers(:,:))**2d0, dim=1))
            ! print*, "Iteration: ", iter, dsp_cluster_centers, count(new_nearest_cluster_indices .ne. old_nearest_cluster_indices)
            ! print*, "Iteration: ", iter, maxval(dsp_cluster_centers)
            if (maxval(dsp_cluster_centers) .le. this%hparam%tolerance) exit
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
    end subroutine fit_faster_kmeans

    subroutine fit_faster2_kmeans(this, x)

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

        ! deallocate cluster center array
        call ifdealloc(this%cluster_centers)

        ! get data shapes
        x_shape = shape(x)
        this % n_samples = x_shape(1)
        this % n_columns = x_shape(2)

        ! calculate square sum of x by row
        allocate(x_sq_sum_row(this%n_samples))
        allocate(cluster_sq_sum_row(this%n_samples))
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
            call calculate_distance_from_center_slow(x, old_cluster_centers(:,c), & 
                distance_from_cluster_center(:,c), this%n_samples, this%n_columns)
        end do
        call date_and_time(values=date_value2)
        time_calculate_distance_from_center_slow = time_calculate_distance_from_center_slow + time_diff(date_value1, date_value2)
        old_nearest_cluster_indices(:) = minloc(sqrt(distance_from_cluster_center(:,:)), dim=2)
        r(:) = f_

        ! Update Centroids
        call date_and_time(values=date_value1)
        new_counters(:) = 0_8
        call update_cluster_centers2(new_cluster_centers, new_counters, x, old_nearest_cluster_indices, &
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
                upper_bounds(i)       =          upper_bounds(i)       + dsp_cluster_centers(c_idx)
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
            call update_cluster_centers3(new_cluster_centers, old_cluster_centers, new_counters, x, & 
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
    end subroutine fit_faster2_kmeans

    subroutine update_cluster_centers(new_cluster_centers, x, cluster_indices, n_samples, n_clusters, n_columns)
        real(kind=8), intent(inout) :: new_cluster_centers(n_columns, n_clusters)
        real(kind=8), intent(in)    :: x(n_samples, n_columns)
        integer(kind=8), intent(in) :: cluster_indices(n_samples)
        integer(kind=8), intent(in) :: n_samples, n_clusters, n_columns

        integer(kind=8) :: i, c, c_idx
        integer(kind=8), allocatable :: counter(:)

        allocate(counter(n_clusters))
        new_cluster_centers(:,:) = 0d0
        counter(:) = 0_8

        do i=1, n_samples, 1
            c_idx = cluster_indices(i)
            new_cluster_centers(:,c_idx) = new_cluster_centers(:,c_idx) + x(i,:)
            counter(c_idx) = counter(c_idx) + 1_8
        end do

        do c_idx=1, n_clusters, 1
            new_cluster_centers(:,c_idx) = new_cluster_centers(:,c_idx) / dble(counter(c_idx))
        end do
    end subroutine update_cluster_centers

    subroutine update_cluster_centers2(new_cluster_centers, new_counters, x, cluster_indices, n_samples, n_clusters, n_columns)
        real(kind=8), intent(inout) :: new_cluster_centers(n_columns, n_clusters)
        integer(kind=8), intent(inout) :: new_counters(n_clusters)
        real(kind=8), intent(in)    :: x(n_samples, n_columns)
        integer(kind=8), intent(in) :: cluster_indices(n_samples)
        integer(kind=8), intent(in) :: n_samples, n_clusters, n_columns

        integer(kind=8) :: i, c, c_idx

        new_cluster_centers(:,:) = 0d0
        new_counters(:) = 0_8

        do i=1, n_samples, 1
            c_idx = cluster_indices(i)
            new_cluster_centers(:,c_idx) = new_cluster_centers(:,c_idx) + x(i,:)
            new_counters(c_idx) = new_counters(c_idx) + 1_8
        end do

        do c_idx=1, n_clusters, 1
            new_cluster_centers(:,c_idx) = new_cluster_centers(:,c_idx) / dble(new_counters(c_idx))
        end do
    end subroutine update_cluster_centers2

    subroutine update_cluster_centers3(new_cluster_centers, old_cluster_centers, new_counters, x, & 
        new_cluster_indices, old_cluster_indices, n_samples, n_clusters, n_columns)
        real(kind=8), intent(inout) :: new_cluster_centers(n_columns, n_clusters)
        real(kind=8), intent(inout) :: old_cluster_centers(n_columns, n_clusters)
        integer(kind=8), intent(inout) :: new_counters(n_clusters)
        real(kind=8), intent(in)    :: x(n_samples, n_columns)
        integer(kind=8), intent(in) :: new_cluster_indices(n_samples)
        integer(kind=8), intent(in) :: old_cluster_indices(n_samples)
        integer(kind=8), intent(in) :: n_samples, n_clusters, n_columns

        integer(kind=8) :: i, c, c_idx, old_c_idx, new_c_idx

        new_cluster_centers(:,:) = old_cluster_centers(:,:)

        do c_idx=1, n_clusters, 1
            new_cluster_centers(:,c_idx) = new_cluster_centers(:,c_idx) * dble(new_counters(c_idx))
        end do

        do i=1, n_samples, 1
            old_c_idx = old_cluster_indices(i)
            new_c_idx = new_cluster_indices(i)
            if (old_c_idx .eq. new_c_idx) cycle
            new_counters(old_c_idx) = new_counters(old_c_idx) - 1_8
            new_counters(new_c_idx) = new_counters(new_c_idx) + 1_8
            new_cluster_centers(:,old_c_idx) = new_cluster_centers(:,old_c_idx) - x(i,:)
            new_cluster_centers(:,new_c_idx) = new_cluster_centers(:,new_c_idx) + x(i,:)
        end do

        do c_idx=1, n_clusters, 1
            new_cluster_centers(:,c_idx) = new_cluster_centers(:,c_idx) / dble(new_counters(c_idx))
        end do
    end subroutine update_cluster_centers3

    ! subroutine update_cluster_centers_diff_only(new_cluster_centers, old_cluster_centers, x, new_counters, old_counters, new_cluster_indices, old_cluster_indices, n_samples, n_clusters, n_columns)
    !     real(kind=8), intent(inout) :: new_cluster_centers(n_columns, n_clusters)
    !     real(kind=8), intent(inout) :: old_cluster_centers(n_columns, n_clusters)
    !     real(kind=8), intent(in)    :: x(n_samples, n_columns)
    !     integer(kind=8), intent(in) :: counters(n_clusters)
    !     integer(kind=8), intent(in) :: cluster_indices(n_samples)
    !     integer(kind=8), intent(in) :: n_samples, n_clusters, n_columns

    !     integer(kind=8) :: i, c, c_idx, new_c_idx, old_c_idx
    !     integer(kind=8), allocatable :: counter(:)

    !     allocate(counter(n_clusters))
    !     new_cluster_centers(:,:) = old_cluster_centers(:,:)
    !     do c_idx=1, n_clusters, 1
    !         new_cluster_centers(:,c_idx) = new_cluster_centers(:,c_idx) * dble(counters(c_idx))
    !     end do

    !     do i=1, n_samples, 1
    !         new_c_idx = new_cluster_indices(i)
    !         old_c_idx = new_cluster_indices(i)
    !         if (new_c_idx .eq. old_c_idx) continue
    !         new_cluster_centers(:,old_c_idx) = new_cluster_centers(:,old_c_idx) - x(i,:)
    !         new_cluster_centers(:,new_c_idx) = new_cluster_centers(:,new_c_idx) + x(i,:)
    !         counter(c_idx) = counter(c_idx) + 1_8
    !     end do

    !     do c_idx=1, n_clusters, 1
    !         new_cluster_centers(:,c_idx) = new_cluster_centers(:,c_idx) / dble(counter(c_idx))
    !     end do
    ! end subroutine update_cluster_centers_diff_only

    subroutine fit_kmeans(this, x)
        class(kmeans) :: this
        real(kind=8), intent(in) :: x(:,:)

        integer(kind=8)              :: x_shape(2), iter, c, i, idx, idx_old
        real(kind=8), allocatable    :: distance_from_cluster_center(:,:)
        real(kind=8), allocatable    :: new_cluster_centers(:,:), old_cluster_centers(:,:)
        real(kind=8), allocatable    :: diff_update(:,:)
        real(kind=8), allocatable    :: x_sq_sum_row(:)
        integer(kind=8), allocatable :: nearest_cluster_indices(:), nearest_cluster_indices_old(:), counter(:)

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
                call calculate_distance_from_center(x, x_sq_sum_row, old_cluster_centers(:,c), & 
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

            if (maxval(sum(diff_update**2d0, dim=1)) .le. this%hparam%tolerance) exit
        end do
        this % is_trained = t_
        this % cluster_centers(:,:) = new_cluster_centers(:,:)
    end subroutine fit_kmeans

    !> A subroutine to predict cluster index of new data
    !! \param x data to be predicted
    function predict_kmeans(this, x)
        class(kmeans) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), allocatable :: predict_kmeans(:)

        type(error)               :: err
        integer(kind=8)           :: x_shape(2), n_samples, n_columns, c
        real(kind=8), allocatable :: distance(:,:), x_sq_sum_row(:)

        x_shape = shape(x)
        n_samples = x_shape(1)
        n_columns = x_shape(2)

        if (n_samples .eq. 0_8) stop "Number of samples of input matrix is less than 0, must be positive."
        call err%check_estimator_is_fitted(this%is_trained, this%algo_name)
        call err%check_number_of_features_mismatch(this%n_columns, n_columns, this%algo_name)

        allocate(distance(n_samples, this%hparam%n_clusters))
        do c=1, this%hparam%n_clusters, 1
            call calculate_distance_from_center_slow(x, this%cluster_centers(:,c), distance(:,c), & 
                n_samples, n_columns)
        end do
        predict_kmeans = minloc(distance, dim=2)
    end function predict_kmeans

    !> A subroutine to select initial cluster centroids by 'kmeans++'
    !! \param x data to be fitted
    subroutine select_initial_clusters_slow(this, x)
        class(kmeans) :: this
        real(kind=8), intent(in) :: x(:,:)

        integer(kind=8)              :: rand_idx, idx
        real(kind=8), allocatable    :: distance(:), dist_mat(:,:)
        real(kind=8), allocatable    :: nearest_distance(:), probas(:)
        integer(kind=8), allocatable :: cluster_idx(:)
        integer(kind=8)              :: c, k, uniq_num_idx
        
        allocate(distance(this%n_samples), nearest_distance(this%n_samples), probas(this%n_samples))
        allocate(this%cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(cluster_idx(0))

        distance(:) = 1d0/dble(this%n_samples)
        nearest_distance(:) = 1d0/dble(this%n_samples)
        do c=1, this%hparam%n_clusters, 1
            call get_nearest_distance(nearest_distance, distance, this%n_samples)
            probas(:) = nearest_distance(:)
            call vector2sum1(probas, this%n_samples)
            idx = roulette_selection(probas, this%n_samples, f_)
            cluster_idx = [cluster_idx, idx]
            this%cluster_centers(:,c) = x(idx,:)
            call calculate_distance_from_center_slow(x, this%cluster_centers(:,c), distance, this%n_samples, this%n_columns)
        end do

        allocate(dist_mat(this%hparam%n_clusters, this%hparam%n_clusters))
        dist_mat = huge(0d0)
        do c=1, this%hparam%n_clusters, 1
            do k=c+1, this%hparam%n_clusters, 1
                dist_mat(c,k) = sum( (this%cluster_centers(:,c)-this%cluster_centers(:,k))**2d0 )
            end do
        end do
        ! print distance matirx for debug.
        ! do c=1, this%hparam%n_clusters, 1
        !     print*, dist_mat(c,:)
        ! end do
        if (minval(dist_mat) .eq. 0d0) stop "Number of unique rows is lower than 'n_clusters'."
    end subroutine select_initial_clusters_slow

    subroutine select_initial_clusters(this, x, x_sq_sum_row, n_samples, n_columns)
        class(kmeans) :: this
        real(kind=8), intent(in) :: x(n_samples, n_columns)
        real(kind=8), intent(in) :: x_sq_sum_row(n_samples)
        integer(kind=8), intent(in) :: n_samples, n_columns

        integer(kind=8)              :: rand_idx, idx
        real(kind=8), allocatable    :: distance(:), dist_mat(:,:)
        real(kind=8), allocatable    :: nearest_distance(:), probas(:)
        integer(kind=8), allocatable :: cluster_idx(:)
        integer(kind=8)              :: c, k, uniq_num_idx
        
        allocate(distance(this%n_samples), nearest_distance(this%n_samples), probas(this%n_samples))
        allocate(this%cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(cluster_idx(0))

        distance(:) = 1d0/dble(this%n_samples)
        nearest_distance(:) = 1d0/dble(this%n_samples)
        do c=1, this%hparam%n_clusters, 1
            call get_nearest_distance(nearest_distance, distance, this%n_samples)

            probas(:) = nearest_distance(:)
            call relu(probas)
            call vector2sum1(probas, this%n_samples)

            cluster_idx = [cluster_idx, idx]
            idx = roulette_selection(probas, this%n_samples, f_)
            this%cluster_centers(:,c) = x(idx,:)
            call calculate_distance_from_center(x, x_sq_sum_row, this%cluster_centers(:,c), distance, &
                    this%n_samples, this%n_columns)
        end do

        allocate(dist_mat(this%hparam%n_clusters, this%hparam%n_clusters))
        dist_mat = huge(0d0)
        do c=1, this%hparam%n_clusters, 1
            do k=c+1, this%hparam%n_clusters, 1
                dist_mat(c,k) = sum( (this%cluster_centers(:,c)-this%cluster_centers(:,k))**2d0 )
            end do
        end do
        ! print distance matirx for debug.
        ! do c=1, this%hparam%n_clusters, 1
        !     print*, dist_mat(c,:)
        ! end do
        if (minval(dist_mat) .eq. 0d0) stop "Number of unique rows is lower than 'n_clusters'."
    end subroutine select_initial_clusters

    !> A subroutine to calculate the distance of 'x' from the centroid
    !! \param x data points (#samples, #columns)
    !! \param center coordinates of centroid (#columns)
    !! \param distance distance from the centroid (#samples)
    !! \param n_samples number of samples
    !! \param n_columns number of columns
    subroutine calculate_distance_from_center_slow(x, center, distance, n_samples, n_columns)
        real(kind=8), intent(in)    :: x(n_samples, n_columns)
        real(kind=8), intent(in)    :: center(n_columns)
        real(kind=8), intent(inout) :: distance(n_samples)
        integer(kind=8), intent(in) :: n_samples, n_columns

        integer(kind=8) :: i, j

        distance = 0d0
        do j=1, n_columns, 1
            do i=1, n_samples, 1
                distance(i) = distance(i) + (x(i,j)-center(j))**2d0
            end do
        end do
    end subroutine calculate_distance_from_center_slow

    subroutine calculate_distance_from_center(x, x_sq_sum_row, center, distance, n_samples, n_columns)
        real(kind=8), intent(in)    :: x(n_samples, n_columns)
        real(kind=8), intent(in)    :: x_sq_sum_row(n_samples)
        real(kind=8), intent(in)    :: center(n_columns)
        real(kind=8), intent(inout) :: distance(n_samples)
        integer(kind=8), intent(in) :: n_samples, n_columns

        real(kind=8) :: center_sq_sum
        integer(kind=8) :: i, j

        center_sq_sum = sum(center**2d0)
        distance(:) = 0d0
        call multi_mat_vec_r8(x, center, distance, n_samples, n_columns, parallel=f_)
        distance = sqrt(maxval((/-2d0*distance + x_sq_sum_row + center_sq_sum, 0d0/)))
    end subroutine calculate_distance_from_center

    !> A subroutine to get nearest distance. 
    !> Compare two distances('nearest_distance', 'distance') and take out the smallest value.
    !! \param nearest_distance current nearest distance values. It will be replaced if the value is greater than 'distance'.
    !! \param distance new distance values, might contain values smaller than 'nearest_distance'.
    subroutine get_nearest_distance(nearest_distance, distance, n)
        implicit none
        real(kind=8), intent(inout) :: nearest_distance(n)
        real(kind=8), intent(in)    :: distance(n)
        integer(kind=8), intent(in) :: n
        integer(kind=8) :: i
        do i=1, n, 1
            nearest_distance(i) = minval((/nearest_distance(i), distance(i)/))
        end do
    end subroutine get_nearest_distance

    subroutine calculate_inter_cluster_distance(inter_cluster_distance, centers, n_clusters, n_columns)
        real(kind=8), intent(inout) :: inter_cluster_distance(n_clusters, n_clusters)
        real(kind=8), intent(in)    :: centers(n_columns, n_clusters)
        integer(kind=8), intent(in) :: n_clusters, n_columns

        integer(kind=8) :: i, j

        inter_cluster_distance(:,:) = huge(0d0)
        do i=1, n_clusters, 1
            do j=1, n_clusters, 1
                if (i .eq. j) cycle
                inter_cluster_distance(i,j) = sum( (centers(:,i)-centers(:,j))**2d0 )
            end do
        end do
        inter_cluster_distance(:,:) = inter_cluster_distance(:,:)
    end subroutine calculate_inter_cluster_distance



end module mod_kmeans
