module mod_base_kmeans
    use :: mod_kinds
    use :: mod_clip,               only: clip_both
    use :: mod_program_limits,     only: MAX_PARAM_LEN, ATOL_R64
    use :: mod_warn_in_range,         only: warn_in_range_i64
    use :: mod_warn_array_size_within_limit
    use :: mod_warn_non_zero_mean
    use :: mod_check
    use :: mod_is_close
    use :: mod_random_int,         only: random_int_single
    use :: mod_random_permutation, only: random_permutation_i64
    use :: mod_prefix_sum,          only: prefix_sum_r64
    use :: mod_prefix_sum_selection, only: prefix_sum_selection_r64
    use :: mod_euclidean_distance, only: euclidean_distance_mv
    use :: mod_base_estimator,     only: base_estimator
    use :: mod_dump_load
    use :: mod_timer
    implicit none
    
    type, abstract, extends(base_estimator) :: base_kmeans
        ! preset
        integer(i64)                 :: n_clusters   = 6
        integer(i64)                 :: max_iter     = 100
        integer(i64)                 :: n_init       = 1
        real(r64)                    :: tol          = 1.0e-5_r64
        character(len=MAX_PARAM_LEN) :: init         = "kmeans++"
        integer(i64)                 :: random_state = -1
        character(len=MAX_PARAM_LEN) :: allowed_init(2) = [ character(len=MAX_PARAM_LEN) :: "random", "kmeans++" ]
        integer(i64)                 :: num_threads
        integer(i64)                 :: chunk_size = -1

        ! post
        integer(i64) :: n_cols_           = -1
        integer(i64) :: n_iter_           = -1
        integer(i64) :: n_init_           = -1
        real(r64)    :: tol_              = -1
        integer(i64) :: init_code_        = -1
        integer(i64) :: random_state_     = -1
        logical      :: set_random_state_ = .false.
        integer(i64) :: num_threads_      = -1
        integer(i64) :: chunk_size_       = -1
        real(r64)    :: best_score        = huge(0_r64)

        ! Cluster centers
        real(r64), allocatable :: centroids(:,:)
        real(r64), allocatable :: C_row_sq_norm(:)
    contains
        ! Initialization
        procedure :: initialize_clusters
        procedure :: initialize_clusters_random
        procedure :: initialize_clusters_kmeanspp

        procedure :: check_X_train
        procedure :: check_X_predict

        procedure :: dump_base_kmeans
        procedure :: load_base_kmeans
    end type base_kmeans

contains

    subroutine initialize_clusters(this, centroids, n_clusters, X, n_rows, n_cols)
        implicit none
        class(base_kmeans)       :: this
        real(r64), intent(inout) :: centroids(n_clusters, n_cols)
        integer(i64), intent(in) :: n_clusters
        real(r64), intent(in)    :: X(n_rows, n_cols)
        integer(i64), intent(in) :: n_rows, n_cols

        if (this%init_code_ == 1_i64) then
            call this%initialize_clusters_random(centroids, n_clusters, X, n_rows, n_cols)
        else if (this%init_code_ == 2_i64) then
            call this%initialize_clusters_kmeanspp(centroids, n_clusters, X, n_rows, n_cols)
        end if
    end subroutine initialize_clusters

    subroutine initialize_clusters_random(this, centroids, n_clusters, X, n_rows, n_cols)
        implicit none
        class(base_kmeans)    :: this
        real(r64), intent(inout) :: centroids(n_clusters, n_cols)
        integer(i64), intent(in) :: n_clusters
        real(r64), intent(in)    :: X(n_rows, n_cols)
        integer(i64), intent(in) :: n_rows, n_cols

        integer(i64) :: i              ! loop counter

        integer(i64), allocatable :: indices(:) ! data indices for random selection
        integer(i64), allocatable :: cluster_idxs(:)      ! selected cluster indices


        ! Random permutation
        allocate(indices(n_rows))
        do i=1, n_rows, 1
            indices(i) = i
        end do
        call random_permutation_i64(indices, n_rows)

        ! Get cluster indices
        allocate(cluster_idxs(this%n_clusters))
        cluster_idxs(:) = indices(1:this%n_clusters)
        centroids(:,:) = X(cluster_idxs,:)

        ! release
        deallocate(cluster_idxs)
    end subroutine initialize_clusters_random

    subroutine initialize_clusters_kmeanspp(this, centroids, n_clusters, X, n_rows, n_cols)
        implicit none
        class(base_kmeans)    :: this
        real(r64), intent(inout) :: centroids(n_clusters, n_cols)
        integer(i64), intent(in) :: n_clusters
        real(r64), intent(in)    :: X(n_rows, n_cols)
        integer(i64), intent(in) :: n_rows, n_cols

        integer(i64) :: c, c_idx       ! loop counter, cluster index
        real(r64)    :: max_dist       ! maximum value of `nearest_distances`
        real(r64)    :: c_sq_norm      ! cluster squared norm

        real(r64), allocatable    :: distances(:)         ! store distance
        real(r64), allocatable    :: nearest_distances(:) ! distance to nearest cluster
        real(r64), allocatable    :: weights(:)           ! store temporal cluster
        real(r64), allocatable    :: weight_prefix(:)
        real(r64), allocatable    :: X_row_sq_norm(:)     ! squared norm per row
        real(r64), allocatable    :: tmp_cluster(:)       ! store cluster temporaly
        integer(i64), allocatable :: cluster_idxs(:)      ! selected cluster indices

        integer(i64) :: date_value1(8), date_value2(8)
        integer(i64) :: time_weight, time_distance

        time_weight = 0
        time_distance = 0

        ! allocation
        allocate(distances(n_rows)); distances = huge(1.0_r64)
        allocate(nearest_distances, source=distances)
        allocate(weights, source=distances)
        allocate(weight_prefix, source=distances)
        allocate(X_row_sq_norm, source=distances)
        allocate(tmp_cluster(n_cols))
        allocate(cluster_idxs(0))

        ! input data norm
        X_row_sq_norm(:) = sum(X*X, dim=2)

        ! process
        do c=1, this%n_clusters, 1
            if (c == 1) then
                c_idx = random_int_single(lo=1_i64, hi=n_rows)
            else
                call date_and_time(values=date_value1)
                nearest_distances(:) = min(nearest_distances(:), distances(:))
                max_dist = maxval(nearest_distances, dim=1)
                if (is_zero(max_dist)) exit
                weights(:) = nearest_distances(:) / sum(nearest_distances)
                weights(:) = clip_both(weights(:), min_val=0.0_r64, max_val=1.0_r64)
                call prefix_sum_r64(weights, weight_prefix, n_rows)
                if (weight_prefix(n_rows) <= 0.0_r64) exit

                c_idx = prefix_sum_selection_r64(weight_prefix, n_rows)
                call date_and_time(values=date_value2)
                time_weight = time_weight + time_diff(date_value1, date_value2)
            end if
            cluster_idxs = [cluster_idxs, c_idx]

            tmp_cluster(:) = X(c_idx,:)
            c_sq_norm = sum(tmp_cluster*tmp_cluster)
            
            call date_and_time(values=date_value1)
            call euclidean_distance_mv(    & 
                    n_rows, n_cols,        &
                    distances,             &
                    X, X_row_sq_norm,      &
                    tmp_cluster, c_sq_norm &
                )
            call date_and_time(values=date_value2)
            time_distance = time_distance + time_diff(date_value1, date_value2)
        end do
        
        this%n_clusters = size(cluster_idxs)
        call warn_in_range_i64(               &
            a=this%n_clusters,               &
            lo=this%n_clusters,               &
            file=__FILE__,                    &
            class_name=this%get_class_name(), &
            value_name="n_clusters")

        centroids(:,:) = X(cluster_idxs,:)

        ! release
        deallocate(cluster_idxs)
        deallocate(weight_prefix)
    end subroutine initialize_clusters_kmeanspp

    subroutine check_X_train(this, X, file, class_name, value_name)
        implicit none
        class(base_kmeans), intent(inout) :: this
        real(r64),    intent(in)       :: X(:,:)
        character(*), intent(in)       :: file
        character(*), intent(in)       :: class_name
        character(*), intent(in)       :: value_name

        integer(i64) :: n_rows

        this%min_n_rows = 2_i64
        this%min_n_cols = 1_i64

        call this%check_X_train_common_r64(X, file, class_name, value_name)

        n_rows = size(X, dim=1)
        call check_ge_i64(n_rows, "size(X, dim=1)", this%n_clusters, "n_clusters", &
            file=file, class_name="kmeans")

        call warn_array_size_within_limit([n_rows, this%n_clusters], &
                            file=file,              &
                            class_name=class_name,       &
                            value_name="DistanceMatrix")

        call warn_non_zero_mean(X, &
                            file=file,              &
                            class_name=class_name,       &
                            value_name="train X")
    end subroutine check_X_train

    subroutine check_X_predict(this, X, file, class_name, value_name)
        implicit none
        class(base_kmeans), intent(inout) :: this
        real(r64),    intent(in)       :: X(:,:)
        character(*), intent(in)       :: file
        character(*), intent(in)       :: class_name
        character(*), intent(in)       :: value_name

        integer(i64) :: n_rows

        call check_fitted_model(this%is_fitted, file, class_name, func_name="predict")
        call this%check_X_predict_common_r64(X, file, class_name, value_name)

        n_rows = size(X, dim=1)
        call warn_array_size_within_limit([n_rows, this%n_clusters], &
                            file=file,              &
                            class_name=class_name,       &
                            value_name="DistanceMatrix")

        call warn_non_zero_mean(X, &
                            file=file,              &
                            class_name=class_name,       &
                            value_name="predict X")
    end subroutine check_X_predict

    subroutine dump_base_kmeans(this, unit)
        implicit none
        class(base_kmeans)    :: this
        integer(i64), intent(in) :: unit

        call this%dump_base_estimator(unit)
        call dump_i64(unit, this%n_clusters)
        call dump_i64(unit, this%max_iter)
        call dump_i64(unit, this%n_init)
        call dump_r64(unit, this%tol)
        call dump_char_fixed_scalar(unit, this%init)
        call dump_i64(unit, this%random_state)
        call dump_char_fixed_vec(unit, this%allowed_init)
        call dump_i64(unit, this%num_threads)
        call dump_i64(unit, this%chunk_size)

        ! post
        call dump_i64(unit, this%n_cols_)
        call dump_i64(unit, this%n_iter_)
        call dump_i64(unit, this%n_init_)
        call dump_r64(unit, this%tol_)
        call dump_i64(unit, this%init_code_)
        call dump_i64(unit, this%random_state_)
        call dump_logical(unit, this%set_random_state_)
        call dump_i64(unit, this%num_threads_)
        call dump_i64(unit, this%chunk_size_)
        call dump_r64(unit, this%best_score)

        ! Cluster centers
        call dump_mat(unit, this%centroids)
        call dump_vec(unit, this%C_row_sq_norm)
    end subroutine dump_base_kmeans

    subroutine load_base_kmeans(this, unit)
        implicit none
        class(base_kmeans)    :: this
        integer(i64), intent(in) :: unit

        call this%load_base_estimator(unit)
        call load_i64(unit, this%n_clusters)
        call load_i64(unit, this%max_iter)
        call load_i64(unit, this%n_init)
        call load_r64(unit, this%tol)
        call load_char_fixed_scalar(unit, this%init)
        call load_i64(unit, this%random_state)
        call load_char_fixed_vec(unit, this%allowed_init)
        call load_i64(unit, this%num_threads)
        call load_i64(unit, this%chunk_size)

        ! post
        call load_i64(unit, this%n_cols_)
        call load_i64(unit, this%n_iter_)
        call load_i64(unit, this%n_init_)
        call load_r64(unit, this%tol_)
        call load_i64(unit, this%init_code_)
        call load_i64(unit, this%random_state_)
        call load_logical(unit, this%set_random_state_)
        call load_i64(unit, this%num_threads_)
        call load_i64(unit, this%chunk_size_)
        call load_r64(unit, this%best_score)

        ! Cluster centers
        call load_mat(unit, this%centroids)
        call load_vec(unit, this%C_row_sq_norm)
    end subroutine load_base_kmeans

end module mod_base_kmeans
