module mod_kmeans
    use :: mod_program_limits,   only: MAX_NAME_LEN, HUGE_R64
    use :: mod_data_summary,     only: data_summary, get_data_summary
    use :: mod_kinds
    use :: mod_memory_helpers,   only: force_deallocate_mat_r64, force_deallocate_vec_r64
    use :: mod_random_seed,      only: fix_random_seed, release_random_seed

    use :: mod_check

    use :: mod_euclidean_distance, only: euclidean_distance_mm, euclidean_distance_mm_bnorm

    use :: mod_base_kmeans,      only: base_kmeans
    use :: mod_kmeans_fit_single_iter_dense
    use :: omp_lib          ! omp_get_num_procs
    implicit none
    
    type, extends(base_kmeans) :: kmeans
    contains
        procedure :: fit                 => fit_kmeans_dense
        procedure :: predict             => predict_kmeans_dense
        procedure :: score               => score_kmeans_dense

        procedure :: dump => dump_kmeans
        procedure :: load => load_kmeans
    end type kmeans

contains

    function new_kmeans(n_clusters, max_iter, init, n_init, tol, &
        random_state, num_threads, print_log, chunk_size, fatal) result(obj)
        implicit none
        type(kmeans) :: obj
        integer(i64), intent(in), optional      :: n_clusters
        integer(i64), intent(in), optional      :: max_iter
        integer(i64), intent(in), optional      :: n_init
        real(r64),    intent(in), optional      :: tol
        character(len=*),  intent(in), optional :: init
        integer(i64), intent(in), optional      :: random_state
        integer(i64), intent(in), optional      :: num_threads
        logical, intent(in), optional           :: print_log
        integer(i64), intent(in), optional      :: chunk_size
        logical, intent(in), optional           :: fatal

        logical :: do_fatal

        obj%name = "kmeans"
        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal
        obj%fatal = do_fatal

        ! Check Hyperparameters
        if (present(n_clusters))        &
            call check_greater_equal(   &
                n_clusters,             & 
                lo=2_i64,               &
                file=__FILE__,          &
                class_name=obj%name,    &
                value_name="n_clusters", fatal=obj%fatal)

        if (present(max_iter))       &
            call check_positive(     &
                max_iter,            &
                file=__FILE__,       &
                class_name=obj%name, &
                value_name="max_iter", fatal=obj%fatal)

        if (present(n_init))         &
            call check_positive(     &
                n_init,              &
                file=__FILE__,       &
                class_name=obj%name, &
                value_name="n_init", fatal=obj%fatal)

        if (present(tol))            &
            call check_non_negative( &
                tol,                 &
                file=__FILE__,       &
                class_name=obj%name, &
                value_name="tol", fatal=obj%fatal)

        if (present(init))                        &
            call check_allowed_option(            &
                init,                             &
                allowed_options=obj%allowed_init, & 
                file=__FILE__,                    &
                class_name=obj%name,              &
                value_name="init", fatal=obj%fatal)

        if (present(num_threads))    &
            call check_positive_i64( &
                num_threads,         &
                file=__FILE__,       &
                class_name=obj%name, &
                value_name="num_threads", fatal=obj%fatal)

        if (present(chunk_size))     &
            call check_positive_i64( &
                chunk_size,          &
                file=__FILE__,       &
                class_name=obj%name, &
                value_name="chunk_size", fatal=obj%fatal)

        ! Set Hyperparameters
        if (present(n_clusters))   obj % n_clusters = n_clusters
        if (present(max_iter))     obj % max_iter = max_iter
        if (present(n_init))       obj % n_init = n_init
        if (present(tol))          obj % tol = tol
        if (present(init))         obj % init = init
        obj % set_random_state_ = present(random_state)
        if (present(random_state)) obj % random_state = random_state
        if (present(init)) obj % init = init
        obj % init_code_ = option_to_code(obj%init, obj%allowed_init)

        obj % num_threads = omp_get_num_procs()
        if (present(num_threads)) obj % num_threads = num_threads
        if (present(print_log)) obj % print_log = print_log
        if (present(num_threads)) obj % num_threads = num_threads
        if (present(chunk_size)) obj % chunk_size = chunk_size
    end function new_kmeans

    subroutine fit_kmeans_dense(this, X, init_C)
        implicit none
        class(kmeans)         :: this
        real(r64), intent(in) :: X(:,:)
        real(r64), optional   :: init_C(:,:)

        integer(i64) :: n_rows, n_cols
        integer(i64) :: ini, itr             ! loop counters, n_init and max_iter
        integer(i64) :: i, c                 ! loop counters, n_rows and n_clusters
        integer(i64) :: label                ! cluster label
        integer(i64) :: label_old, label_new ! cluster label
        integer(i64) :: count_diff           ! #diff centroid update
        real(r64)    :: best_score, score    ! Sum of Squared Error

        integer(i64), allocatable :: labels_new(:), labels_old(:)                 ! cluster labels
        logical,      allocatable :: labels_diff(:)                               ! cluster labels diff
        integer(i64), allocatable :: labels_counter_new(:)                        ! cluster label counter
        real(r64), allocatable    :: centroids_new(:,:), centroids_old(:,:)       ! centroids
        real(r64), allocatable    :: centroids_best(:,:)                          ! best centroids
        real(r64), allocatable    :: C_row_sq_norm(:), C_row_sq_norm_best(:)      ! squared norm per row
        real(r64), allocatable    :: mat_dist_whole(:,:), vec_min_dist(:)             ! distance matrix
        real(r64), allocatable    :: mat_dist_chunk(:,:)                          ! chunked distance matrix
        real(r64) :: X_sqsum
        logical   :: relocation_happened, reach_tol
        integer(i64) :: i_star, best_iter, n_iter_, prev
        logical   :: set_init_C

        set_init_C = present(init_C)

        ! Initialization
        call this%check_X_train(X, file=__FILE__, class_name=this%name, value_name="X")
        call force_deallocate_mat_r64(this%centroids)
        call force_deallocate_vec_r64(this%C_row_sq_norm)
        this % is_fitted = .false.
        best_score = huge(0.0_r64)
        this%summary = get_data_summary(X)
        n_rows = this%summary%n_rows
        n_cols = this%summary%n_cols
        X_sqsum = sum(X**2.0_r64)
        if (set_init_C) then
            call check_exact_shape(init_C, this%n_clusters, this%summary%n_cols, &
                file=__FILE__, &
                class_name=this%name, &
                value_name="init_C")
        end if

        ! Allocation
        allocate(labels_new(n_rows), labels_old(n_rows))
        allocate(labels_diff(n_rows))
        allocate(labels_counter_new(this%n_clusters))

        allocate(centroids_new(this%n_clusters, n_cols), centroids_old(this%n_clusters, n_cols))
        allocate(centroids_best(this%n_clusters, n_cols))

        if (this%chunk_size>0_i64) then
            allocate(mat_dist_chunk(this%chunk_size, this%n_clusters))
        else
            allocate(mat_dist_whole(n_rows, this%n_clusters))
        end if
        allocate(vec_min_dist(n_rows))
        allocate(C_row_sq_norm(this%n_clusters), C_row_sq_norm_best(this%n_clusters))

        if (this%set_random_state_) call fix_random_seed(this%random_state)
        prev = omp_get_num_threads()
        call openblas_set_num_threads(this%num_threads)
        do ini=1, this%n_init, 1
            if (this%print_log) print*, "ini: ", ini, "*******************************************************************"
            if (set_init_C) then
                centroids_new(:,:) = init_C(:,:)
            else
                call this%initialize_clusters(centroids_new, this%n_clusters, X, n_rows, n_cols)
            end if
            C_row_sq_norm(:) = sum(centroids_new**2.0_r64, dim=2)
            labels_old(:) = -1_i64
            relocation_happened = .false.

            do itr=1, this%max_iter, 1
                if (this%chunk_size>0_i64) then
                    reach_tol = fit_single_iter_chunk_dense(                  &
                            n_rows, n_cols, this%n_clusters, this%chunk_size, &
                            this%print_log, this%tol, this%name, itr,         &
                            X, X_sqsum,                                       &
                            mat_dist_chunk, vec_min_dist,                     &
                            centroids_new, centroids_old, C_row_sq_norm,      &
                            labels_new, labels_old, labels_diff, labels_counter_new, n_iter_)
                else
                    reach_tol = fit_single_iter_whole_dense(             &
                            n_rows, n_cols, this%n_clusters,             &
                            this%print_log, this%tol, this%name, itr,    &
                            X, X_sqsum,                                  &
                            mat_dist_whole, vec_min_dist,                &
                            centroids_new, centroids_old, C_row_sq_norm, &
                            labels_new, labels_old, labels_diff, labels_counter_new, n_iter_)
                end if
                if (reach_tol) exit
            end do

            if (this%n_init == 1_i64) then
                centroids_best(:,:) = centroids_new(:,:)
                best_iter = n_iter_
            else
                ! b_j|^2  − 2 a_i·b_j
                call euclidean_distance_mm_bnorm(    &
                    n_rows, n_cols, this%n_clusters, &
                    mat_dist_whole,                        &
                    X,                               &
                    centroids_new, C_row_sq_norm)
                score = sum(minval(mat_dist_whole, dim=2)) + X_sqsum
                if (score <= best_score) then
                    best_score = score
                    centroids_best(:,:) = centroids_new(:,:)
                    best_iter = n_iter_
                end if
            end if
            C_row_sq_norm_best(:) = sum(centroids_best(:,:)*centroids_best(:,:), dim=2)
            if (set_init_C) exit
        end do
        call openblas_set_num_threads(prev)
        if (this%set_random_state_) call release_random_seed()
        allocate(this%centroids, source=centroids_best)
        allocate(this%C_row_sq_norm, source=C_row_sq_norm_best)

        ! Deallocation
        deallocate(vec_min_dist)
        deallocate(labels_new, labels_old, labels_diff)
        deallocate(centroids_new, centroids_old, centroids_best)
        deallocate(C_row_sq_norm, C_row_sq_norm_best)
        call force_deallocate_mat_r64(mat_dist_whole)
        call force_deallocate_mat_r64(mat_dist_chunk)
        this % n_iter_ = best_iter
        this % best_score = best_score

        this % is_fitted = .true.
    end subroutine fit_kmeans_dense


    function predict_kmeans_dense(this, X) result(cluster_labels)
        implicit none
        class(kmeans)         :: this
        real(r64), intent(in) :: X(:,:)
        integer(i64), allocatable :: cluster_labels(:,:)
        integer(i64) :: n_rows

        real(r64), allocatable    :: mat_dist(:,:)                                ! distance matrix

        call this%check_X_predict(X,         & 
            file=__FILE__,                   &
            class_name=this%name,            &
            value_name="X")

        n_rows = size(X, dim=1)
        allocate(cluster_labels(n_rows, 1))

        allocate(mat_dist(n_rows, this%n_clusters))
        call euclidean_distance_mm_bnorm(     &
            n_rows, this%summary%n_cols, this%n_clusters, &
            mat_dist,                         &
            X,                                &
            this%centroids, this%C_row_sq_norm)

        cluster_labels(:,1) = minloc(mat_dist, dim=2)
        deallocate(mat_dist)
    end function predict_kmeans_dense

    function score_kmeans_dense(this, X) result(score)
        implicit none
        class(kmeans)         :: this
        real(r64), intent(in) :: X(:,:)
        real(r64)             :: score
        integer(i64) :: n_rows

        real(r64), allocatable    :: mat_dist(:,:)                                ! distance matrix
        real(r64) :: X_sqsum

        call this%check_X_predict(X,         & 
            file=__FILE__,                   &
            class_name=this%name,            &
            value_name="X")

        n_rows = size(X, dim=1)
        X_sqsum = sum(X**2.0_r64)

        allocate(mat_dist(n_rows, this%n_clusters))
        call euclidean_distance_mm_bnorm(     &
            n_rows, this%summary%n_cols, this%n_clusters, &
            mat_dist,                         &
            X,                                &
            this%centroids, this%C_row_sq_norm)
        score = sum(minval(mat_dist, dim=2)) + X_sqsum
        deallocate(mat_dist)
    end function score_kmeans_dense

    subroutine dump_kmeans(this, filename)
        class(kmeans), intent(in) :: this
        character(*),  intent(in) :: filename
        integer(i64) :: u

        open(newunit=u, file=filename, access='stream', form='unformatted', &
             action='write', status='replace', convert='little_endian')
        call this%dump_base_kmeans(u)   ! ← 実体はこれだけ
        close(u)
    end subroutine

    subroutine load_kmeans(this, filename)
        class(kmeans), intent(inout) :: this
        character(*),  intent(in)    :: filename
        integer(i64) :: u

        call check_file_exists(   &
            filename,             & 
            file=__FILE__,        &
            class_name=this%name, &
            func_name="load", fatal=this%fatal)

        open(newunit=u, file=filename, access='stream', form='unformatted', &
             action='read', convert='little_endian')
        call this%load_base_kmeans(u)   ! ← 実体はこれだけ
        close(u)
    end subroutine
end module mod_kmeans