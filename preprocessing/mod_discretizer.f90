module mod_discretizer
    use mod_const
    use mod_common
    use mod_timer
    use mod_sort
    use mod_stats
    use mod_hyperparameter
    use mod_linalg
    implicit none


    type breaks
        real(kind=8), allocatable :: break(:)
    end type breaks
    

    type column_discretizer
        integer(kind=8)              :: n_bins_
        real(kind=8), allocatable    :: thresholds_(:) ! break point values
        type(hparam_discretizer)     :: hparam

        real(kind=8), allocatable    :: uniq_array(:)   ! unique values
        integer(kind=8), allocatable :: uniq_counter(:) ! unique value counters

        integer(kind=8), allocatable :: prefix_counter(:) ! prefix count
        real(kind=8), allocatable    :: prefix_sum(:)     ! prefix sum
        real(kind=8), allocatable    :: prefix_sq_sum(:)  ! prefix squared sum
    contains
        procedure :: psum
        procedure :: fit => fit_column_discretizer
        procedure :: transform => transform_column_discretizer
        procedure :: fit_uniform_discretizer
        procedure :: fit_quantile_discretizer
        procedure :: fit_kmeans_discretizer
        procedure :: fit_greedy_discretizer
        procedure :: fit_modified_greedy_discretizer
        procedure :: fit_dpoptimal_equal_area_discretizer
        ! procedure :: fit_kmeans_discretizer_old
    end type column_discretizer


    interface column_discretizer
        procedure :: new_column_discretizer
    end interface column_discretizer


    type discretizer
        type(hparam_discretizer)              :: hparam
        type(column_discretizer), allocatable :: column_discretizers(:)
    contains
        procedure :: fit => fit_descretizer
        procedure :: transform => transform_descretizer
    end type discretizer


    interface discretizer
        procedure :: new_discretizer
    end interface discretizer

contains

    function transform_descretizer(this, x)
        implicit none
        class(discretizer)            :: this
        real(kind=8), intent(in)     :: x(:,:)
        integer(kind=4), allocatable :: transform_descretizer(:,:)

        integer(kind=8) :: x_shape(2), n_samples, n_columns, c, i
        real(kind=8), allocatable :: vector(:)

        x_shape = shape(x)
        n_samples = x_shape(1)
        n_columns = x_shape(2)
        allocate(transform_descretizer(n_samples, n_columns))
        allocate(vector(n_samples))

        do c=1, n_columns, 1
            do i=1, n_samples, 1
                vector(i) = x(i, c)
            end do
            transform_descretizer(:,c) = this%column_discretizers(c)%transform(vector)
        end do
    end function transform_descretizer


    function new_discretizer(max_bins, strategy)
        implicit none
        type(discretizer)   :: new_discretizer
        type(discretizer)   :: tmp
        integer(kind=8), optional  :: max_bins
        character(len=*), optional :: strategy
        character(len=256) :: preset_strategies(6)

        if ( present(max_bins) ) tmp%hparam%max_bins = max_bins
        if ( present(strategy) ) tmp%hparam%strategy = strategy

        preset_strategies(1) = "uniform"
        preset_strategies(2) = "quantile"
        preset_strategies(3) = "kmeans"
        preset_strategies(4) = "greedy"
        preset_strategies(5) = "modified_greedy"
        preset_strategies(6) = "dpoptimal"

        call tmp%hparam%validate_int_range("max_bins", tmp%hparam%max_bins, 2_8, huge(1_8))
        call tmp%hparam%validate_char_list("strategy", tmp%hparam%strategy, preset_strategies)
        tmp%hparam%strategy_int = tmp%hparam%convert_char_to_int(tmp%hparam%strategy, preset_strategies)

        new_discretizer = tmp
    end function new_discretizer


    function new_column_discretizer(max_bins, strategy)
        implicit none
        type(column_discretizer)   :: new_column_discretizer
        type(column_discretizer)   :: tmp
        integer(kind=8), optional  :: max_bins
        character(len=*), optional :: strategy

        character(len=256) :: preset_strategies(6)
        preset_strategies(1) = "uniform"
        preset_strategies(2) = "quantile"
        preset_strategies(3) = "kmeans"
        preset_strategies(4) = "greedy"
        preset_strategies(5) = "modified_greedy"
        preset_strategies(6) = "dpoptimal"

        if ( present(max_bins) ) tmp%hparam%max_bins = max_bins
        if ( present(strategy) ) tmp%hparam%strategy = strategy

        call tmp%hparam%validate_int_range("max_bins", tmp%hparam%max_bins, 2_8, huge(1_8))
        call tmp%hparam%validate_char_list("strategy", tmp%hparam%strategy, preset_strategies)
        tmp%hparam%strategy_int = tmp%hparam%convert_char_to_int(tmp%hparam%strategy, preset_strategies)

        new_column_discretizer = tmp
    end function new_column_discretizer


    subroutine fit_descretizer(this, x)
        class(discretizer)       :: this
        real(kind=8), intent(in) :: x(:,:)

        integer(kind=8) :: x_shape(2), n_samples, n_columns, col, n
        real(kind=8), allocatable :: vector(:)

        x_shape = shape(x)
        n_samples = x_shape(1)
        n_columns = x_shape(2)
        allocate(this%column_discretizers(n_columns))

        allocate(vector(n_samples))
        do col=1, n_columns, 1
            do n=1, n_samples
                vector(n) = x(n,col)
            end do
            this%column_discretizers(col) &
                = column_discretizer(max_bins=this%hparam%max_bins, strategy=this%hparam%strategy)
            call this%column_discretizers(col)%fit(vector)
        end do
    end subroutine fit_descretizer


    !> A function to calculate prefix sum.
    function psum(this, i,j) 
        implicit none
        class(column_discretizer) :: this
        real(kind=8) :: psum
        integer(kind=8) :: i, j
        if ( i .ge. j ) then
            psum = 0d0
            return
        end if
        psum = this%prefix_sum(j) - this%prefix_sum(i-1)
    end function psum


    function transform_column_discretizer(this, vector)
        implicit none
        class(column_discretizer)    :: this
        real(kind=8), intent(in)     :: vector(:)
        integer(kind=8), allocatable :: transform_column_discretizer(:)

        real(kind=8)    :: tmp_val
        integer(kind=8) :: n_samples, n, idx
        integer(kind=8) :: n_thresholds

        n_samples = size(vector)
        n_thresholds = size(this%thresholds_)

        if (allocated(transform_column_discretizer)) deallocate(transform_column_discretizer)
        allocate(transform_column_discretizer(n_samples))

        do n=1, n_samples, 1
            tmp_val = vector(n)
            idx = linear_search(this%thresholds_, n_thresholds, tmp_val)
            transform_column_discretizer(n) = idx
        end do
    end function transform_column_discretizer


    subroutine fit_column_discretizer(this, vector)
        implicit none
        class(column_discretizer) :: this
        real(kind=8)       :: vector(:)

        integer(kind=8)    :: n_samples

        n_samples = size(vector)
        select case (this%hparam%strategy_int)
            case (1) ! uniform
                call this%fit_uniform_discretizer(vector, n_samples)
            case (2) ! quantile
                call this%fit_quantile_discretizer(vector, n_samples)
            case (3) ! kmeans
                call this%fit_kmeans_discretizer(vector, n_samples)
            case (4) ! greedy
                call this%fit_greedy_discretizer(vector, n_samples)
            case (5) ! modified_greedy
                call this%fit_modified_greedy_discretizer(vector, n_samples)
            case (6) ! dynamic programming optimal equal area break
                call this%fit_dpoptimal_equal_area_discretizer(vector, n_samples)
            case (7:)
                stop "Not Implemented"
        end select

        this%n_bins_ = size(this%thresholds_)
    end subroutine fit_column_discretizer


    subroutine fit_dpoptimal_equal_area_discretizer(this, vector, n_samples)
        implicit none
        class(column_discretizer)          :: this
        real(kind=8), intent(in)    :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples

        integer(kind=8)              :: n_unique_v, n, m, null_break(0)
        real(kind=8)                 :: avg, sum_total
        real(kind=8), allocatable    :: vector_copy(:), unique_v(:), sum_v(:)
        real(kind=8), allocatable    :: best_error(:,:)
        type(breaks), allocatable    :: best_breaks(:,:)


        print*, "GROUPBY"
        allocate(vector_copy(n_samples))
        do n=1, n_samples, 1
            vector_copy(n) = vector(n)
        end do
        call groupby_sum(unique_v, sum_v, vector_copy, n_samples)
        n_unique_v = size(unique_v)

        print*, "PREFIX SUM"
        allocate(this%prefix_sum(0:n_unique_v))
        sum_total = 0d0
        this%prefix_sum(0) = 0
        do n=1, n_unique_v, 1
            sum_total = sum_total + sum_v(n)
            this%prefix_sum(n) = sum_total
        end do
        avg = sum_total / dble(this%hparam%max_bins)

        print*, "ALLOCATE ERROR MATRIX"
        allocate(best_error(1:n_unique_v+1,  1:this%hparam%max_bins))
        allocate(best_breaks(1:n_unique_v+1, 1:this%hparam%max_bins))

        print*, "INITIALIZE ERROR MATRIX"
        do m=1, n_unique_v+1, 1
            call progress_bar(m, n_unique_v+1, int((n_unique_v+1)/dble(100d0), kind=8))
            best_error(m,1) = abs(this%psum(1_8, m) - avg)
            allocate(best_breaks(m,1)%break(0))
        end do
    end subroutine fit_dpoptimal_equal_area_discretizer


    subroutine fit_modified_greedy_discretizer(this, vector, n_samples)
        implicit none
        class(column_discretizer)          :: this
        real(kind=8), intent(in)    :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples

        real(kind=8)                 :: sum_total, avg_chunk, tmp_sum_chunk
        integer(kind=8)              :: n, u, n_unique_v, c, n_chunks
        real(kind=8), allocatable    :: unique_v(:), sum_v(:), vector_copy(:)
        integer(kind=8), allocatable :: count_v(:), chunk_edges(:)

        allocate(vector_copy(n_samples))
        do n=1, n_samples, 1
            vector_copy(n) = vector(n)
        end do
        call groupby_sum(unique_v, sum_v, vector_copy, n_samples)
        call groupby_count(unique_v, count_v, vector_copy, n_samples)
        n_unique_v = size(unique_v)

        sum_total = 0d0
        do u=1, n_unique_v, 1
            sum_total = sum_total + sum_v(u)
        end do
        avg_chunk = sum_total / dble(this%hparam%max_bins)

        n_chunks = 1
        tmp_sum_chunk = 0d0
        allocate(chunk_edges(0))
        do c=1, n_unique_v-1
            tmp_sum_chunk = tmp_sum_chunk + sum_v(c)
            if ( tmp_sum_chunk .ge. avg_chunk * n_chunks ) then
                chunk_edges = [chunk_edges, c]
                n_chunks = n_chunks + 1
            end if
        end do

        if (allocated(this%thresholds_)) deallocate(this%thresholds_)
        allocate(this%thresholds_(0))
        do c=1, size(chunk_edges), 1
            this%thresholds_ & 
                = [this%thresholds_, & 
                    (unique_v(chunk_edges(c)) + unique_v(chunk_edges(c)+1))*.5d0]
        end do
        this%thresholds_ = [this%thresholds_, huge(0d0)]
    end subroutine fit_modified_greedy_discretizer


    subroutine fit_greedy_discretizer(this, vector, n_samples)
        implicit none
        class(column_discretizer)          :: this
        real(kind=8), intent(in)    :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples

        real(kind=8)                 :: sum_total, avg_chunk, tmp_sum_chunk
        integer(kind=8)              :: n, u, n_unique_v, c
        real(kind=8), allocatable    :: unique_v(:), sum_v(:), vector_copy(:)
        integer(kind=8), allocatable :: count_v(:), chunk_edges(:)

        allocate(vector_copy(n_samples))
        do n=1, n_samples, 1
            vector_copy(n) = vector(n)
        end do
        call groupby_sum(unique_v, sum_v, vector_copy, n_samples)
        call groupby_count(unique_v, count_v, vector_copy, n_samples)
        n_unique_v = size(unique_v)

        sum_total = 0d0
        do u=1, n_unique_v, 1
            sum_total = sum_total + sum_v(u)
        end do
        avg_chunk = sum_total / dble(this%hparam%max_bins)

        tmp_sum_chunk = 0d0
        allocate(chunk_edges(0))
        do c=1, n_unique_v-1
            tmp_sum_chunk = tmp_sum_chunk + sum_v(c)
            if ( tmp_sum_chunk .ge. avg_chunk ) then
                chunk_edges = [chunk_edges, c]
                tmp_sum_chunk = 0d0
            end if
        end do

        if (allocated(this%thresholds_)) deallocate(this%thresholds_)
        allocate(this%thresholds_(0))
        do c=1, size(chunk_edges), 1
            this%thresholds_ & 
                = [this%thresholds_, & 
                    (unique_v(chunk_edges(c)) + unique_v(chunk_edges(c)+1))*.5d0]
        end do
        this%thresholds_ = [this%thresholds_, huge(0d0)]
    end subroutine fit_greedy_discretizer


    subroutine fit_kmeans_discretizer(this, vector, n_samples)
        implicit none
        integer(kind=8) :: date_value1(8), date_value2(8)
        class(column_discretizer)          :: this
        real(kind=8), intent(in)    :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples

        real(kind=8), allocatable    :: unique_v(:), vector_copy(:)
        integer(kind=8), allocatable :: count_v(:)
        integer(kind=8) :: n, n_unique_v, idx, m, factor, i, mm, cnt, c
        integer(kind=8) :: n_unique_v_unroll, n_selected
        real(kind=8) :: rand_val, dc, dn, tmp_dist_nearest(63), tmp_centroid, sum_dist
        real(kind=8), allocatable :: centroids(:), centroids_new(:), dist_current(:), dist_nearest(:), dist_nearest_norm(:)
        real(kind=8), allocatable :: cluster_sum(:), centroids_tmp(:)
        real(kind=8) :: centroid_old, centroid_new, dist_old, dist_new, min_centroid, max_centroid, med_centroid, tmp_point
        integer(kind=8), allocatable :: cluster_counter(:), cluster_idx(:), centroid_indices(:), uniq_indices(:)
        integer(kind=8) :: centroid_idx_old, centroid_idx_new, start_idx, end_idx, iter, nearest_idx
        logical(kind=4) :: is_reverse
        integer(kind=8) :: min_index, med_index, max_index

        if (allocated(this%thresholds_)) deallocate(this%thresholds_)
        if (allocated(centroids)) deallocate(centroids)
        if (allocated(dist_nearest)) deallocate(dist_nearest)
        if (allocated(dist_current)) deallocate(dist_current)

        ! print*, "Copy & Sort" ! -------------------------------------------------------------------------
        allocate(vector_copy(n_samples))
        do n=1, n_samples, 1
            vector_copy(n) = vector(n)
        end do
        call quick_sort(vector_copy, n_samples)
        
        ! print*, "GROUPBY" ! -------------------------------------------------------------------------
        call groupby_count(unique_v, count_v, vector_copy, n_samples)
        n_unique_v = size(unique_v)
        if (n_unique_v .le. this%hparam%max_bins) goto 999

        ! print*, "Select Centroids" ! -------------------------------------------------------------------------
        allocate(centroids(0))
        allocate(centroid_indices(0))
        allocate(dist_nearest(n_unique_v))
        allocate(dist_nearest_norm(n_unique_v))

        dist_nearest = 1d0/dble(n_unique_v)
        idx = roulette_selection(dist_nearest, n_unique_v, reverse=f_)
        centroids = [centroids, unique_v(idx)]
        tmp_centroid = centroids(1)
        do n=1, n_unique_v, 1
            dist_nearest(n) = abs(tmp_centroid-unique_v(n))
        end do
        centroid_old = unique_v(idx)
        centroid_indices = [centroid_indices, 1_8, idx, n_unique_v]
        call quick_sort(centroid_indices, size(centroid_indices)+0_8)

        do while (size(centroids) .lt. this%hparam%max_bins)
            is_reverse = f_
            if (mod(size(centroids),2) .eq. 0) is_reverse = t_

            do n=1, n_unique_v, 1
                dist_nearest_norm(n) = dist_nearest(n)
            end do
            call vector2sum1(dist_nearest_norm, n_unique_v)

            idx = roulette_selection(dist_nearest_norm, n_unique_v, reverse=is_reverse)
            centroids = [centroids, unique_v(idx)]
            centroid_new = unique_v(idx)
            centroid_idx_new = idx
            centroid_indices = [centroid_indices, centroid_idx_new]
            call quick_sort(centroids, size(centroids)+0_8)
            call quick_sort(centroid_indices, size(centroid_indices)+0_8)

            do i=2, size(centroid_indices)-1, 1
                if ( centroid_idx_new .eq. centroid_indices(i) ) then
                    start_idx = centroid_indices(i-1)
                    end_idx   = centroid_indices(i+1)
                    exit
                end if
            end do

            min_centroid = unique_v(start_idx)
            med_centroid = centroid_new
            max_centroid = unique_v(end_idx)

            do n=start_idx, end_idx, 1
                tmp_point = unique_v(n)
                dist_nearest(n) = minval((/ & 
                    abs(tmp_point-min_centroid) ,& 
                    abs(tmp_point-med_centroid) , & 
                    abs(tmp_point-max_centroid) /))
            end do
        end do


        ! print*, "Update Centroids until convergence" ! -------------------------------------------------------------------------
        allocate(centroids_tmp(0))
        allocate(cluster_idx(n_unique_v))
        allocate(cluster_sum(this%hparam%max_bins))
        allocate(cluster_counter(this%hparam%max_bins))
        allocate(dist_current(n_unique_v))

        call quick_sort(centroids, size(centroids)+0_8)
        ! print*, "START: ", centroids

        dist_nearest = abs(unique_v-centroids(1))
        cluster_idx = 1_8
        do n=2, size(centroids), 1
            dist_current = abs(unique_v-centroids(n))
            do i=1, n_unique_v
                dn = dist_nearest(i)
                dc = dist_current(i)
                factor = dc .lt. dn
                cluster_idx(i) = cluster_idx(i) + factor
                dist_nearest(i) = minval((/dn, dc/))
            end do
        end do

        do iter=1, 10000, 1
            centroids_tmp = centroids

            ! Left most Centroid
            min_centroid = centroids_tmp(1)
            min_index = binary_search_left(unique_v, n_unique_v, min_centroid)
            ! print*, 1, min_index
            do idx=1, min_index
                cluster_idx(idx) = 1_8
            end do

            ! Intermediate Centroids
            do c=2, this%hparam%max_bins
                max_centroid = centroids_tmp(c)
                max_index = binary_search_left(unique_v, n_unique_v, max_centroid)
                ! print*, min_index, max_index 

                do idx=min_index, max_index, 1
                    tmp_point = unique_v(idx)
                    factor = abs(tmp_point-min_centroid) .lt. abs(tmp_point-max_centroid)
                    cluster_idx(idx) = c-factor
                end do

                min_centroid = max_centroid
                min_index = max_index + 1
            end do

            ! Right most Centroid
            max_centroid = centroids_tmp(this%hparam%max_bins)
            max_index = binary_search_left(unique_v, n_unique_v, max_centroid)
            ! print*, max_index, n_unique_v
            do idx=max_index, n_unique_v
                cluster_idx(idx) = this%hparam%max_bins
            end do

            ! print*, "Update Centroid Coordinates"
            cluster_sum = 0d0
            cluster_counter = 0_8
            do i=1, n_unique_v, 1
                idx = cluster_idx(i)
                cluster_sum(idx) = cluster_sum(idx) + unique_v(i) * count_v(i)
                cluster_counter(idx) = cluster_counter(idx) + count_v(i)
            end do
            centroids_new = cluster_sum / dble(cluster_counter)

            ! print*, iter, sum(abs(centroids-centroids_new))
            if ( maxval(abs(centroids-centroids_new)) .le. 10d-18 ) exit
            centroids = centroids_new
            ! call sleep(1)
        end do

        if (allocated(this%thresholds_)) deallocate(this%thresholds_)
        allocate(this%thresholds_(0))
        do i=1, size(centroids_new)-1, 1
            this%thresholds_ = [this%thresholds_, (centroids_new(i)+centroids_new(i+1))*.5d0]
        end do
        this%thresholds_ = [this%thresholds_, huge(0d0)]

        return
        999 continue
        if (n_unique_v .eq. 1) then
            allocate(this%thresholds_(1))
            this%thresholds_ = huge(0d0)
        elseif ( n_unique_v .le. this%hparam%max_bins ) then
            allocate(this%thresholds_(0))
            do n=2, n_unique_v, 1
                this%thresholds_ = [this%thresholds_, (unique_v(n-1)+unique_v(n)) * .5d0]
            end do
            this%thresholds_ = [this%thresholds_, huge(0d0)]
        end if        
    end subroutine fit_kmeans_discretizer


    subroutine fit_kmeans_discretizer_old(this, vector, n_samples)
        implicit none
        integer(kind=8) :: date_value1(8), date_value2(8)
        class(column_discretizer)          :: this
        real(kind=8), intent(in)    :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples

        real(kind=8), allocatable    :: unique_v(:), vector_copy(:)
        integer(kind=8), allocatable :: count_v(:)
        integer(kind=8) :: n, n_unique_v, idx, m, factor, i, mm, cnt
        integer(kind=8) :: n_unique_v_unroll, n_selected
        real(kind=8) :: rand_val, dc, dn, tmp_dist_nearest(63), tmp_centroid, sum_dist
        real(kind=8), allocatable :: centroids(:), centroids_new(:), dist_current(:), dist_nearest(:), dist_nearest_norm(:)
        real(kind=8), allocatable :: cluster_sum(:)
        real(kind=8) :: centroid_old, centroid_new, dist_old, dist_new, min_centroid, max_centroid, med_centroid, tmp_point
        integer(kind=8), allocatable :: cluster_counter(:), cluster_idx(:), centroid_indices(:)
        integer(kind=8) :: centroid_idx_old, centroid_idx_new, start_idx, end_idx
        logical(kind=4) :: is_reverse

        if (allocated(this%thresholds_)) deallocate(this%thresholds_)
        if (allocated(centroids)) deallocate(centroids)
        if (allocated(dist_nearest)) deallocate(dist_nearest)
        if (allocated(dist_current)) deallocate(dist_current)
        call date_and_time(values=date_value1)
        allocate(vector_copy(n_samples))
        do n=1, n_samples, 1
            vector_copy(n) = vector(n)
        end do
        call quick_sort(vector_copy, n_samples)
        call date_and_time(values=date_value2)
        print*, "Copy & Sort Time: ", time_diff(date_value1, date_value2)
        

        print*, "GROUPBY"
        call date_and_time(values=date_value1)
        call groupby_count(unique_v, count_v, vector_copy, n_samples)
        n_unique_v = size(unique_v)
        print*, n_unique_v, this%hparam%max_bins
        if (n_unique_v .le. this%hparam%max_bins) goto 999
        call date_and_time(values=date_value2)
        print*, "Groupby Time: ", time_diff(date_value1, date_value2)


        print*, "Select Centroids"
        n_selected = 1
        call date_and_time(values=date_value1)
        allocate(centroids(this%hparam%max_bins))
        centroids = 0d0
        call random_number(rand_val)
        idx = n_unique_v * rand_val + 1
        centroids(1) = unique_v(idx)

        ! select 2th- centroids
        allocate(dist_nearest(n_unique_v))
        allocate(dist_current(n_unique_v))
        n_unique_v_unroll = n_unique_v - mod(n_unique_v, 63)
        do while (n_selected .lt. this%hparam%max_bins)
            dist_nearest = abs(unique_v-centroids(1))
            do n=2, n_selected, 1
                dist_current = abs(unique_v-centroids(n))
                do m=1, n_unique_v_unroll, 63
                    do mm=0, 63-1
                        tmp_dist_nearest(mm+1) = minval((/dist_nearest(m+mm), dist_current(m+mm)/))
                    end do
                    do mm=0, 63-1
                        dist_nearest(m+mm) = tmp_dist_nearest(mm+1)
                    end do
                end do
                do m=n_unique_v_unroll+1, n_unique_v
                    dist_nearest(m) = minval((/dist_nearest(m), dist_current(m)/))
                end do
            end do

            if (sum(dist_nearest) .eq. 0d0) exit
            dist_nearest = dist_nearest * dist_nearest
            dist_nearest = dist_nearest / sum(dist_nearest)

            idx = roulette_selection(dist_nearest, n_unique_v, reverse=f_)
            n_selected = n_selected + 1
            centroids(n_selected) = unique_v(idx)

            ! print*, "Current Centroids: ", centroids
            ! print*, "New Centroids:     ", unique_v(idx)
            ! call sleep(5)
        end do
        call quick_sort(centroids, size(centroids)+0_8)
        allocate(centroids_new(size(centroids)))
        call date_and_time(values=date_value2)
        print*, "Selection Time: ", time_diff(date_value1, date_value2)

        print*, "Update Centroids until convergence"
        ! print*, "START: ", centroids
        cnt = 1
        call date_and_time(values=date_value1)
        allocate(cluster_idx(n_unique_v))
        allocate(cluster_sum(this%hparam%max_bins))
        allocate(cluster_counter(this%hparam%max_bins))
        ! print*, "----------------------------------------------------------------------------------"
        ! print*, centroids
        do while (t_)
            dist_nearest = abs(unique_v-centroids(1))
            cluster_idx = 1_8
            do n=2, size(centroids), 1
                dist_current = abs(unique_v-centroids(n))
                do i=1, n_unique_v
                    dn = dist_nearest(i)
                    dc = dist_current(i)
                    factor = dc .lt. dn
                    cluster_idx(i) = cluster_idx(i) + factor
                    dist_nearest(i) = minval((/dn, dc/))
                end do
            end do

            cluster_sum = 0d0
            cluster_counter = 0_8
            do i=1, n_unique_v, 1
                idx = cluster_idx(i)
                cluster_sum(idx) = cluster_sum(idx) + unique_v(i) * count_v(i)
                cluster_counter(idx) = cluster_counter(idx) + count_v(i)
            end do
            centroids_new = cluster_sum / dble(cluster_counter)

            if ( maxval(abs(centroids-centroids_new)) .le. 10d-18 ) exit
            centroids = centroids_new
        end do

        allocate(this%thresholds_(0))
        do i=1, size(centroids_new)-1, 1
            this%thresholds_ = [this%thresholds_, (centroids_new(i)+centroids_new(i+1))*.5d0]
        end do
        this%thresholds_ = [this%thresholds_, huge(0d0)]
        call date_and_time(values=date_value2)
        print*, "----------------------------------------------------------------------------------"
        print*, "Convergence Time: ", time_diff(date_value1, date_value2)
        ! print*, "STOP: ", real(centroids_new, kind=4)


        return
        999 continue
        if (n_unique_v .eq. 1) then
            allocate(this%thresholds_(1))
            this%thresholds_ = huge(0d0)
        elseif ( n_unique_v .le. this%hparam%max_bins ) then
            allocate(this%thresholds_(0))
            do n=2, n_unique_v, 1
                this%thresholds_ = [this%thresholds_, (unique_v(n-1)+unique_v(n)) * .5d0]
            end do
            this%thresholds_ = [this%thresholds_, huge(0d0)]
        end if        
    end subroutine fit_kmeans_discretizer_old


    subroutine fit_quantile_discretizer(this, vector, n_samples)
        implicit none
        class(column_discretizer)          :: this
        real(kind=8), intent(in)    :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples

        real(kind=8), allocatable :: vector_copy(:)
        real(kind=8), allocatable :: tmp(:)
        real(kind=8) :: step, qr, r, qth
        integer(kind=8) :: q, i

        if (allocated(this%thresholds_)) deallocate(this%thresholds_)

        allocate(vector_copy(n_samples))
        do i=1, n_samples, 1
            vector_copy(i) = vector(i)
        end do
        call quick_sort(vector_copy, n_samples)
        if (vector_copy(1) .eq. vector_copy(n_samples)) then
            allocate(this%thresholds_(1))
            this%thresholds_ = huge(0d0)
        end if

        allocate(tmp(0))
        step = (n_samples-1_8) * 1d0/dble(this%hparam%max_bins)
        do i=1, this%hparam%max_bins-1, 1
            qth = step * i
            qr = qth + 1_8
            q = int(qr)
            r = qr-q
            tmp = [tmp, vector_copy(q-1) + (vector_copy(q)-vector_copy(q-1)) * r]
        end do

        allocate(this%thresholds_(0))
        this%thresholds_ = [this%thresholds_, tmp(1)]
        do i=2, size(tmp)
            if (tmp(i-1) .ne. tmp(i) ) then
                this%thresholds_ = [this%thresholds_, tmp(i)]
            end if
        end do
        this%thresholds_ = [this%thresholds_, huge(0d0)]
    end subroutine fit_quantile_discretizer


    subroutine fit_uniform_discretizer(this, vector, n_samples)
        implicit none
        class(column_discretizer)          :: this
        real(kind=8), intent(in)    :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples

        integer(kind=8)  :: i
        real(kind=8)     :: min_val, max_val, step, tmp

        call get_minmax(min_val, max_val, vector, n_samples)

        if (min_val .eq. max_val) then
            allocate(this%thresholds_(1))
            this%thresholds_ = huge(0d0)
        else
            step = (max_val - min_val) / dble(this%hparam%max_bins)
            allocate(this%thresholds_(this%hparam%max_bins))
            tmp = min_val
            do i=1, this%hparam%max_bins-1, 1
                tmp = tmp + step
                this%thresholds_(i) = tmp
            end do
            this%thresholds_(this%hparam%max_bins) = huge(0d0)
        end if
        this%n_bins_ = size(this%thresholds_)
    end subroutine fit_uniform_discretizer


end module mod_discretizer
