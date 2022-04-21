module mod_base_kmeans
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

    type base_kmeans
        ! base
        character(len=256) :: algo_name !< algorithm name

        ! parameters
        type(hparam_kmeans) :: hparam !< type of hyperparameter for 'kmeans'

        ! train results
        integer(kind=8)              :: n_samples=-1               !< training results. #samples
        integer(kind=8)              :: n_columns=-1               !< training results. #columns
        logical(kind=4)              :: is_trained = f_            !< training results. already fitted or not
        real(kind=8), allocatable    :: cluster_centers(:,:)       !< training results. coordinates of cluster centroids. dimension=(#centroids, #columns)    
    contains
        procedure :: score    => score_base_kmeans
        procedure :: dump     => dump_base_kmeans
        procedure :: load     => load_base_kmeans
        procedure :: predict  => predict_base_kmeans
        procedure :: select_initial_clusters
        procedure :: select_initial_clusters_slow
    end type base_kmeans
    
contains


    !> A subroutine to select initial cluster centroids by 'kmeans++'
    !! \param x data to be fitted
    subroutine select_initial_clusters_slow(this, x)
        class(base_kmeans) :: this
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


    !> A subroutine to select initial cluster centroids by 'base_kmeans++', a little bit fast.
    !! \param x data to be fitted
    subroutine select_initial_clusters(this, x, x_sq_sum_row, n_samples, n_columns)
        class(base_kmeans) :: this
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
            probas = relu(probas)
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

    !> A subroutine to update cluster centers.
    !! \param new_cluster_centers new cluster center coordinates
    !! \param x explanatory data
    !! \param cluster_indices cluster center indices for each points of 'x'
    !! \param n_samples number of samples of 'x'
    !! \param n_samples number of clusters to be assigned
    !! \param n_samples number of columns of 'x'
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

    !> A subroutine to update cluster centers.
    !! \param new_cluster_centers new cluster center coordinates
    !! \param old_cluster_centers old cluster center coordinates
    !! \param new_counters number of samples in each clusters
    !! \param x explanatory data
    !! \param new_cluster_indices new cluster center indices for each points of 'x'
    !! \param old_cluster_indices old cluster center indices for each points of 'x'
    !! \param n_samples number of samples of 'x'
    !! \param n_samples number of clusters to be assigned
    !! \param n_samples number of columns of 'x'
    subroutine update_cluster_centers_diff_only(new_cluster_centers, old_cluster_centers, new_counters, x, & 
        new_cluster_indices, old_cluster_indices, n_samples, n_clusters, n_columns)
        real(kind=8), intent(inout) :: new_cluster_centers(n_columns, n_clusters)
        real(kind=8), intent(inout) :: old_cluster_centers(n_columns, n_clusters)
        integer(kind=8), intent(inout) :: new_counters(n_clusters)
        real(kind=8), intent(in)    :: x(n_samples, n_columns)
        integer(kind=8), intent(in) :: new_cluster_indices(n_samples)
        integer(kind=8), intent(in) :: old_cluster_indices(n_samples)
        integer(kind=8), intent(in) :: n_samples, n_clusters, n_columns

        integer(kind=8) :: i, c, c_idx, old_c_idx, new_c_idx

        if (sum(new_counters) .eq. 0_8) then
            do i=1, n_samples, 1
                c_idx = old_cluster_indices(i)
                new_cluster_centers(:,c_idx) = new_cluster_centers(:,c_idx) + x(i,:)
                new_counters(c_idx) = new_counters(c_idx) + 1_8
            end do

            do c_idx=1, n_clusters, 1
                new_cluster_centers(:,c_idx) = new_cluster_centers(:,c_idx) / dble(new_counters(c_idx))
            end do
        else
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
        end if
    end subroutine update_cluster_centers_diff_only

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

    !> A subroutine to calculate the distance of 'x' from the centroid.
    !> Expanding (x-y)^2 and calculating only x x y will speed up the process. 
    !> (Since x is a data point, x^2 only needs to be computed once, 
    !> and y is a cluster and is very few in number compared to the data points and the number of features, 
    !> the amount of computation is negligible.)
    !! \param x data points (#samples, #columns)
    !! \param center coordinates of centroid (#columns)
    !! \param distance distance from the centroid (#samples)
    !! \param n_samples number of samples
    !! \param n_columns number of columns
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
        call multi_mat_vec_r8(x, center, distance, n_samples, n_columns)
        ! call multi_mat_vec_parallel_r8(x, center, distance, n_samples, n_columns)
        do i=1, n_samples, 1
            distance(i) = maxval((/-2d0*distance(i) + x_sq_sum_row(i) + center_sq_sum, 0d0/))
        end do
    end subroutine calculate_distance_from_center

    !> A subroutine to calculate the distance of 'x' from the centroid with dgemv.
    !> Expanding (x-y)^2 and calculating only x x y will speed up the process. 
    !> (Since x is a data point, x^2 only needs to be computed once, 
    !> and y is a cluster and is very few in number compared to the data points and the number of features, 
    !> the amount of computation is negligible.)
    !! \param x data points (#samples, #columns)
    !! \param center coordinates of centroid (#columns)
    !! \param distance distance from the centroid (#samples)
    !! \param n_samples number of samples
    !! \param n_columns number of columns
    subroutine calculate_distance_from_center_dgemv(x, x_sq_sum_row, center, distance, n_samples, n_columns)
        real(kind=8), intent(in)    :: x(n_samples, n_columns)
        real(kind=8), intent(in)    :: x_sq_sum_row(n_samples)
        real(kind=8), intent(in)    :: center(n_columns)
        real(kind=8), intent(inout) :: distance(n_samples)
        integer(kind=8), intent(in) :: n_samples, n_columns

        real(kind=8) :: center_sq_sum
        integer(kind=8) :: i, j

        center_sq_sum = sum(center**2d0)
        ! distance(:) = 0d0
        ! call multi_mat_vec_r8(x, center, distance, n_samples, n_columns)
        call dgemv("N", n_samples, n_columns, 1d0, x, n_samples, center, 1_8, 0d0, distance, 1_8)
        ! call multi_mat_vec_parallel_r8(x, center, distance, n_samples, n_columns)
        do i=1, n_samples, 1
            distance(i) = maxval((/-2d0*distance(i) + x_sq_sum_row(i) + center_sq_sum, 0d0/))
        end do
    end subroutine calculate_distance_from_center_dgemv

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

    !> A subroutine to calculate inter cluster distance for elkan's method. 
    !! \param inter_cluster_distance inter cluster distance.
    !! \param centers cluster center coordinates.
    !! \param n_clusters number of clusters
    !! \param n_columns number of columns
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

    !> Calculate Kmeans Score
    !> sum of squared distance from nearest centroid.
    !! \param x input data, 2-dim.
    function score_base_kmeans(this, x)
        implicit none
        class(base_kmeans)            :: this
        real(kind=8)             :: score_base_kmeans
        real(kind=8), intent(in) :: x(:,:)

        integer(kind=8), allocatable :: cluster_indices(:)
        integer(kind=8)              :: n_samples, n_columns, i, c_idx, step
        cluster_indices = this%predict(x)
        score_base_kmeans = 0d0
        n_samples = size(cluster_indices)
        do i=1, n_samples, 1
            c_idx = cluster_indices(i)
            score_base_kmeans = score_base_kmeans + sum( (x(i,:) - this%cluster_centers(:,c_idx))**2d0 )
        end do
    end function score_base_kmeans

    !> A subroutine to dump fitted 'kmeans' object
    !> If not fitted, cannot dump training results.
    !! \param file_name output file name
    subroutine dump_base_kmeans(this, file_name)
        class(base_kmeans) :: this
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
    end subroutine dump_base_kmeans


    !> A subroutine to load 'base_kmeans' object
    !> If not fitted, cannot load.
    !! \param file_name loading file name
    subroutine load_base_kmeans(this, file_name)
        implicit none
        class(base_kmeans)    :: this
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
    end subroutine load_base_kmeans


    !> A subroutine to predict cluster index of new data
    !! \param x data to be predicted
    function predict_base_kmeans(this, x)
        class(base_kmeans) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), allocatable :: predict_base_kmeans(:)

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
        predict_base_kmeans = minloc(distance, dim=2)
    end function predict_base_kmeans

end module mod_base_kmeans