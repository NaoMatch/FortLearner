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
        procedure :: fit => fit_kmeans
        procedure :: fit_faster => fit_faster_kmeans
        procedure :: predict => predict_kmeans
        procedure :: dump => dump_kmeans
        procedure :: load => load_kmeans
        procedure :: select_initial_clusters
        procedure :: select_initial_clusters_faster
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
    subroutine fit_kmeans(this, x, iter0, iter_max)
        class(kmeans) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: iter0, iter_max

        integer(kind=8)              :: x_shape(2), iter, c, i, idx
        real(kind=8), allocatable    :: distance_from_cluster_center(:,:)
        real(kind=8), allocatable    :: new_cluster_centers(:,:), old_cluster_centers(:,:)
        real(kind=8), allocatable    :: diff_update(:,:)
        integer(kind=8), allocatable :: nearest_cluster_index(:), counter(:)

        integer(kind=8) :: date_value1(8), date_value2(8)
        integer(kind=8), save :: time_select_initial_clusters=0
        integer(kind=8), save :: time_calculate_distance_from_center=0
        integer(kind=8), save :: time_update_center1=0
        integer(kind=8), save :: time_update_center2=0

        if (iter0 .eq. 1_8) then
            time_select_initial_clusters=0
            time_calculate_distance_from_center=0
            time_update_center1=0
            time_update_center2=0
        end if

        call ifdealloc(this%cluster_centers)

        x_shape = shape(x)
        this % n_samples = x_shape(1)
        this % n_columns = x_shape(2)

        call date_and_time(values=date_value1)
        call this%select_initial_clusters(x, iter0, iter_max)
        call date_and_time(values=date_value2)
        time_select_initial_clusters = time_select_initial_clusters + time_diff(date_value1, date_value2)

        allocate(distance_from_cluster_center(this%n_samples, this%hparam%n_clusters))
        allocate(old_cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(new_cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(diff_update(this%n_columns, this%hparam%n_clusters))
        allocate(nearest_cluster_index(this%n_samples))
        allocate(counter(this%hparam%n_clusters))

        old_cluster_centers(:,:) = this%cluster_centers(:,:)
        do iter=1, this%hparam%max_iter, 1
            ! Calculate Distance From Cluster Centers, and Assign Cluster Index
            distance_from_cluster_center(:,:) = 0d0
            call date_and_time(values=date_value1)
            do c=1, this%hparam%n_clusters, 1
                call calculate_distance_from_center(x, old_cluster_centers(:,c), distance_from_cluster_center(:,c), & 
                    this%n_samples, this%n_columns)
            end do
            call date_and_time(values=date_value2)
            time_calculate_distance_from_center = time_calculate_distance_from_center + time_diff(date_value1, date_value2)
            nearest_cluster_index(:) = minloc(distance_from_cluster_center(:,:), dim=2)

            ! Update Cluster Center Coordinates
            call date_and_time(values=date_value1)
            counter(:) = 0_8
            new_cluster_centers(:,:) = 0d0
            do i=1, this%n_samples, 1
                idx = nearest_cluster_index(i)
                new_cluster_centers(:,idx) = new_cluster_centers(:,idx) + x(i,:)
                counter(idx) = counter(idx) + 1_8
            end do
            call date_and_time(values=date_value2)
            time_update_center1 = time_update_center1 + time_diff(date_value1, date_value2)
            
            call date_and_time(values=date_value1)
            do c=1, this%hparam%n_clusters, 1
                new_cluster_centers(:,c) = new_cluster_centers(:,c)/dble(counter(c))
                diff_update(:,c) = abs(new_cluster_centers(:,c)-old_cluster_centers(:,c))
            end do
            old_cluster_centers(:,:) = new_cluster_centers(:,:)
            call date_and_time(values=date_value2)
            time_update_center2 = time_update_center2 + time_diff(date_value1, date_value2)

            if (maxval(sum(diff_update**2d0, dim=1)) .le. this%hparam%tolerance) exit
        end do
        this % is_trained = t_
        this % cluster_centers(:,:) = new_cluster_centers(:,:)

        if (iter0 .eq. iter_max) then
            print*, '============================================================='
            print*, " time_select_initial_clusters        : ", time_select_initial_clusters
            print*, " time_calculate_distance_from_center : ", time_calculate_distance_from_center
            print*, " time_update_center1                 : ", time_update_center1
            print*, " time_update_center2                 : ", time_update_center2
        end if
    end subroutine fit_kmeans

    subroutine fit_faster_kmeans(this, x, iter0, iter_max)
        class(kmeans) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: iter0, iter_max

        integer(kind=8)              :: x_shape(2), iter, c, i, idx
        real(kind=8), allocatable    :: distance_from_cluster_center(:,:)
        real(kind=8), allocatable    :: new_cluster_centers(:,:), old_cluster_centers(:,:)
        real(kind=8), allocatable    :: diff_update(:,:)
        real(kind=8), allocatable    :: x_sq_sum_row(:)
        integer(kind=8), allocatable :: nearest_cluster_index(:), counter(:)

        integer(kind=8) :: date_value1(8), date_value2(8)
        integer(kind=8), save :: time_sq_sum_row=0
        integer(kind=8), save :: time_select_initial_clusters=0
        integer(kind=8), save :: time_calculate_distance_from_center=0
        integer(kind=8), save :: time_update_center1=0
        integer(kind=8), save :: time_update_center2=0

        if (iter0 .eq. 1_8) then
            time_sq_sum_row=0
            time_select_initial_clusters=0
            time_calculate_distance_from_center=0
            time_update_center1=0
            time_update_center2=0
        end if

        call ifdealloc(this%cluster_centers)

        x_shape = shape(x)
        this % n_samples = x_shape(1)
        this % n_columns = x_shape(2)

        call date_and_time(values=date_value1)
        allocate(x_sq_sum_row(this%n_samples))
        ! x_sq_sum_row = sum(x**2d0, dim=2)
        call matrix_sqsum_row_01x16_F_r8(x, x_sq_sum_row, this % n_samples, this % n_columns)
        call date_and_time(values=date_value2)
        time_sq_sum_row = time_sq_sum_row + time_diff(date_value1, date_value2)

        call date_and_time(values=date_value1)
        call this%select_initial_clusters_faster(x, x_sq_sum_row, this%n_samples, this%n_columns, iter0, iter_max)
        call date_and_time(values=date_value2)
        time_select_initial_clusters = time_select_initial_clusters + time_diff(date_value1, date_value2)

        allocate(distance_from_cluster_center(this%n_samples, this%hparam%n_clusters))
        allocate(old_cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(new_cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(diff_update(this%n_columns, this%hparam%n_clusters))
        allocate(nearest_cluster_index(this%n_samples))
        allocate(counter(this%hparam%n_clusters))

        old_cluster_centers(:,:) = this%cluster_centers(:,:)
        do iter=1, this%hparam%max_iter, 1
            ! Calculate Distance From Cluster Centers, and Assign Cluster Index
            distance_from_cluster_center(:,:) = 0d0
            call date_and_time(values=date_value1)
            do c=1, this%hparam%n_clusters, 1
                call calculate_distance_from_center_faster(x, x_sq_sum_row, old_cluster_centers(:,c), & 
                    distance_from_cluster_center(:,c), this%n_samples, this%n_columns)
            end do
            call date_and_time(values=date_value2)
            time_calculate_distance_from_center = time_calculate_distance_from_center + time_diff(date_value1, date_value2)
            nearest_cluster_index(:) = minloc(distance_from_cluster_center(:,:), dim=2)

            ! Update Cluster Center Coordinates
            call date_and_time(values=date_value1)
            counter(:) = 0_8
            new_cluster_centers(:,:) = 0d0
            do i=1, this%n_samples, 1
                idx = nearest_cluster_index(i)
                new_cluster_centers(:,idx) = new_cluster_centers(:,idx) + x(i,:)
                counter(idx) = counter(idx) + 1_8
            end do
            call date_and_time(values=date_value2)
            time_update_center1 = time_update_center1 + time_diff(date_value1, date_value2)
            
            call date_and_time(values=date_value1)
            do c=1, this%hparam%n_clusters, 1
                new_cluster_centers(:,c) = new_cluster_centers(:,c)/dble(counter(c))
                diff_update(:,c) = abs(new_cluster_centers(:,c)-old_cluster_centers(:,c))
            end do
            old_cluster_centers(:,:) = new_cluster_centers(:,:)
            call date_and_time(values=date_value2)
            time_update_center2 = time_update_center2 + time_diff(date_value1, date_value2)

            if (maxval(sum(diff_update**2d0, dim=1)) .le. this%hparam%tolerance) exit
        end do
        this % is_trained = t_
        this % cluster_centers(:,:) = new_cluster_centers(:,:)

        if (iter0 .eq. iter_max) then
            print*, '============================================================='
            print*, " time_sq_sum_row                     : ", time_sq_sum_row
            print*, " time_select_initial_clusters        : ", time_select_initial_clusters
            print*, " time_calculate_distance_from_center : ", time_calculate_distance_from_center
            print*, " time_update_center1                 : ", time_update_center1
            print*, " time_update_center2                 : ", time_update_center2
        end if
    end subroutine fit_faster_kmeans

    !> A subroutine to predict cluster index of new data
    !! \param x data to be predicted
    function predict_kmeans(this, x)
        class(kmeans) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), allocatable :: predict_kmeans(:)

        type(error)               :: err
        integer(kind=8)           :: x_shape(2), n_samples, n_columns, c
        real(kind=8), allocatable :: distance(:,:)

        x_shape = shape(x)
        n_samples = x_shape(1)
        n_columns = x_shape(2)

        if (n_samples .eq. 0_8) stop "Number of samples of input matrix is less than 0, must be positive."
        call err%check_estimator_is_fitted(this%is_trained, this%algo_name)
        call err%check_number_of_features_mismatch(this%n_columns, n_columns, this%algo_name)

        allocate(distance(n_samples, this%hparam%n_clusters))
        do c=1, this%hparam%n_clusters, 1
            call calculate_distance_from_center(x, this%cluster_centers(:,c), distance(:,c), & 
                n_samples, n_columns)
        end do

        predict_kmeans = minloc(distance, dim=2)
    end function predict_kmeans

    !> A subroutine to select initial cluster centroids by 'kmeans++'
    !! \param x data to be fitted
    subroutine select_initial_clusters(this, x, iter0, iter_max)
        class(kmeans) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: iter0, iter_max

        integer(kind=8)              :: rand_idx, idx
        real(kind=8), allocatable    :: distance(:), dist_mat(:,:)
        real(kind=8), allocatable    :: nearest_distance(:), probas(:)
        integer(kind=8), allocatable :: cluster_idx(:)
        integer(kind=8)              :: c, k, uniq_num_idx

        integer(kind=8) :: date_value1(8), date_value2(8)
        integer(kind=8), save :: time_get_nearest_distance=0
        integer(kind=8), save :: time_vector2sum1=0
        integer(kind=8), save :: time_calculate_distance_from_center=0
        integer(kind=8), save :: time_validation=0
        
        allocate(distance(this%n_samples), nearest_distance(this%n_samples), probas(this%n_samples))
        allocate(this%cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(cluster_idx(0))
        this%cluster_centers = -2d0
        if (iter0 .eq. 1_8) then
            time_get_nearest_distance=0
            time_vector2sum1=0
            time_calculate_distance_from_center=0
            time_validation=0
        end if

        distance(:) = 1d0
        nearest_distance(:) = 1d0
        do c=1, this%hparam%n_clusters, 1
            call date_and_time(values=date_value1)
            call get_nearest_distance(nearest_distance, distance, this%n_samples)
            call date_and_time(values=date_value2)
            time_get_nearest_distance = time_get_nearest_distance + time_diff(date_value1, date_value2)

            probas(:) = nearest_distance(:)
            call date_and_time(values=date_value1)
            call vector2sum1(probas, this%n_samples)
            call date_and_time(values=date_value2)
            time_vector2sum1 = time_vector2sum1 + time_diff(date_value1, date_value2)

            cluster_idx = [cluster_idx, idx]
            idx = roulette_selection(probas, this%n_samples, f_)
            this%cluster_centers(:,c) = x(idx,:)
            call date_and_time(values=date_value1)
            call calculate_distance_from_center(x, this%cluster_centers(:,c), distance, this%n_samples, this%n_columns)
            call date_and_time(values=date_value2)
            time_calculate_distance_from_center = time_calculate_distance_from_center + time_diff(date_value1, date_value2)
        end do

        call date_and_time(values=date_value1)
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
        call date_and_time(values=date_value2)
        time_validation = time_validation + time_diff(date_value1, date_value2)

        if (iter0 .eq. iter_max) then
            print*, "        time_get_nearest_distance          : ", time_get_nearest_distance
            print*, "        time_vector2sum1                   : ", time_vector2sum1
            print*, "        time_calculate_distance_from_center: ", time_calculate_distance_from_center
            print*, "        time_validation                    : ", time_validation
        end if
    end subroutine select_initial_clusters

    subroutine select_initial_clusters_faster(this, x, x_sq_sum_row, n_samples, n_columns, iter0, iter_max)
        class(kmeans) :: this
        real(kind=8), intent(in) :: x(n_samples, n_columns)
        real(kind=8), intent(in) :: x_sq_sum_row(n_samples)
        integer(kind=8), intent(in) :: n_samples, n_columns
        integer(kind=8), intent(in) :: iter0, iter_max

        integer(kind=8)              :: rand_idx, idx
        real(kind=8), allocatable    :: distance(:), dist_mat(:,:)
        real(kind=8), allocatable    :: nearest_distance(:), probas(:)
        integer(kind=8), allocatable :: cluster_idx(:)
        integer(kind=8)              :: c, k, uniq_num_idx

        integer(kind=8) :: date_value1(8), date_value2(8)
        integer(kind=8), save :: time_get_nearest_distance=0
        integer(kind=8), save :: time_vector2sum1=0
        integer(kind=8), save :: time_calculate_distance_from_center=0
        integer(kind=8), save :: time_validation=0
        
        allocate(distance(this%n_samples), nearest_distance(this%n_samples), probas(this%n_samples))
        allocate(this%cluster_centers(this%n_columns, this%hparam%n_clusters))
        allocate(cluster_idx(0))
        this%cluster_centers = -2d0
        if (iter0 .eq. 1_8) then
            time_get_nearest_distance=0
            time_vector2sum1=0
            time_calculate_distance_from_center=0
            time_validation=0
        end if

        distance(:) = 1d0
        nearest_distance(:) = 1d0
        do c=1, this%hparam%n_clusters, 1
            call date_and_time(values=date_value1)
            call get_nearest_distance(nearest_distance, distance, this%n_samples)
            call date_and_time(values=date_value2)
            time_get_nearest_distance = time_get_nearest_distance + time_diff(date_value1, date_value2)

            probas(:) = nearest_distance(:)
            call date_and_time(values=date_value1)
            call vector2sum1(probas, this%n_samples)
            call date_and_time(values=date_value2)
            time_vector2sum1 = time_vector2sum1 + time_diff(date_value1, date_value2)

            cluster_idx = [cluster_idx, idx]
            idx = roulette_selection(probas, this%n_samples, f_)
            this%cluster_centers(:,c) = x(idx,:)
            call date_and_time(values=date_value1)
            call calculate_distance_from_center_faster(x, x_sq_sum_row, this%cluster_centers(:,c), distance, &
                    this%n_samples, this%n_columns)
            call date_and_time(values=date_value2)
            time_calculate_distance_from_center = time_calculate_distance_from_center + time_diff(date_value1, date_value2)
        end do

        call date_and_time(values=date_value1)
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
        call date_and_time(values=date_value2)
        time_validation = time_validation + time_diff(date_value1, date_value2)

        if (iter0 .eq. iter_max) then
            print*, "        time_get_nearest_distance          : ", time_get_nearest_distance
            print*, "        time_vector2sum1                   : ", time_vector2sum1
            print*, "        time_calculate_distance_from_center: ", time_calculate_distance_from_center
            print*, "        time_validation                    : ", time_validation
        end if
    end subroutine select_initial_clusters_faster

    !> A subroutine to calculate the distance of 'x' from the centroid
    !! \param x data points (#samples, #columns)
    !! \param center coordinates of centroid (#columns)
    !! \param distance distance from the centroid (#samples)
    !! \param n_samples number of samples
    !! \param n_columns number of columns
    subroutine calculate_distance_from_center(x, center, distance, n_samples, n_columns)
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
    end subroutine calculate_distance_from_center

    subroutine calculate_distance_from_center_faster(x, x_sq_sum_row, center, distance, n_samples, n_columns)
        real(kind=8), intent(in)    :: x(n_samples, n_columns)
        real(kind=8), intent(in)    :: x_sq_sum_row(n_samples)
        real(kind=8), intent(in)    :: center(n_columns)
        real(kind=8), intent(inout) :: distance(n_samples)
        integer(kind=8), intent(in) :: n_samples, n_columns

        real(kind=8) :: center_sq_sum
        integer(kind=8) :: i, j

        center_sq_sum = sum(center**2d0)
        call multi_mat_vec(x, center, distance, n_samples, n_columns)
        distance = distance + x_sq_sum_row + center_sq_sum
    end subroutine calculate_distance_from_center_faster

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

end module mod_kmeans
