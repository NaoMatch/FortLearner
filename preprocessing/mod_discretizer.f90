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
        procedure :: fit_kmeans_naive_discretizer
        procedure :: fit_greedy_discretizer
        procedure :: fit_modified_greedy_discretizer
        procedure :: fit_dpoptimal_equal_area_discretizer
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
        character(len=256) :: preset_strategies(5)

        if ( present(max_bins) ) tmp%hparam%max_bins = max_bins
        if ( present(strategy) ) tmp%hparam%strategy = strategy

        tmp%hparam%algo_name = "dicretizer"
        preset_strategies(1) = "uniform"
        preset_strategies(2) = "quantile"
        preset_strategies(3) = "kmeans_naive"
        preset_strategies(4) = "greedy"
        preset_strategies(5) = "modified_greedy"
        ! preset_strategies(6) = "dpoptimal"

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

        character(len=256) :: preset_strategies(5)
        tmp%hparam%algo_name = "dicretizer"
        preset_strategies(1) = "uniform"
        preset_strategies(2) = "quantile"
        preset_strategies(3) = "kmeans_naive"
        preset_strategies(4) = "greedy"
        preset_strategies(5) = "modified_greedy"
        ! preset_strategies(6) = "dpoptimal"

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
            case (3) ! kmeans_naive
                call this%fit_kmeans_naive_discretizer(vector, n_samples)
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


    subroutine fit_kmeans_naive_discretizer(this, vector, n_samples)
        implicit none
        integer(kind=8) :: date_value1(8), date_value2(8)
        class(column_discretizer)          :: this
        real(kind=8), intent(in)    :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples

        real(kind=8), allocatable    :: unique_values(:), vector_copy(:)
        integer(kind=8), allocatable :: counts(:)
        integer(kind=8) :: n, n_unique_values, idx, m, factor, i, mm, cnt, c, selected_idx, min_idx, mid_idx, max_idx
        integer(kind=8) :: n_selected, n_current_centroids
        real(kind=8) :: rand_val, dc, dn, tmp_dist_nearest(63), tmp_centroid, sum_dist
        real(kind=8) :: n_unique_values_inv, centroid
        real(kind=8), allocatable :: centroid_coordinates(:), centroid_coordinates_new(:), centroid_coordinates_old(:)
        real(kind=8), allocatable :: dist_current(:), distance_nearest(:), distance_nearest_norm(:), sum_distances(:)
        real(kind=8), allocatable :: cluster_sum(:), centroid_coordinates_tmp(:)
        real(kind=8) :: centroid_new, dist_old, dist_new, min_centroid, max_centroid, mid_centroid, tmp_point
        real(kind=8) :: uniq_val, centroid_left, centroid_right, midpoint, coordinate
        integer(kind=8), allocatable :: cluster_counter(:), centroid_indices(:), uniq_indices(:), bin_counter(:)
        integer(kind=8) :: centroid_idx_old, centroid_idx_new, start_idx, end_idx, iter, nearest_idx, centroid_idx, cluster_idx
        integer(kind=8) :: midpoint_idx, left_idx, right_idx, counter, sum_check
        logical(kind=4) :: is_reverse
        integer(kind=8) :: min_index, med_index, max_index, n_centroid_coordinates, n_centroid_indices

        if (allocated(this%thresholds_)) deallocate(this%thresholds_)
        if (allocated(centroid_coordinates)) deallocate(centroid_coordinates)
        if (allocated(distance_nearest)) deallocate(distance_nearest)
        if (allocated(dist_current)) deallocate(dist_current)

! call date_and_time(values=date_value1)
        ! print*, '============================================================='
        ! print*, "Copy & Sort"
        allocate(vector_copy(n_samples))
        do n=1, n_samples, 1
            vector_copy(n) = vector(n)
        end do
        call quick_sort(vector_copy, n_samples)
! call date_and_time(values=date_value2)
! print*, "Copy & Sort", time_diff(date_value1, date_value2)

! call date_and_time(values=date_value1)
        ! print*, '============================================================='
        ! print*, "Get Unique Values & Counts"
        call groupby_count(unique_values, counts, vector_copy, n_samples)
        n_unique_values = size(unique_values)
        DEALLOCATE(vector_copy)
        if (n_unique_values .le. this%hparam%max_bins) goto 999
! call date_and_time(values=date_value2)
! print*, "Get Unique Values & Counts", time_diff(date_value1, date_value2)


        ! print*, '============================================================='
        ! print*, "Select Initial Centroids"
        allocate(centroid_coordinates(0))
        allocate(centroid_indices(0))
        allocate(distance_nearest(n_unique_values))
        allocate(distance_nearest_norm(n_unique_values))

! call date_and_time(values=date_value1)
        is_reverse = f_
        n_unique_values_inv = 1d0 / dble(n_unique_values)
        do n=1, n_unique_values, 1
            distance_nearest(n) = n_unique_values_inv
        end do
        centroid_indices = [centroid_indices, 1_8, n_unique_values]

        do while ( size(centroid_coordinates) .lt. this%hparam%max_bins )
            ! Select New Centroid
            is_reverse = .not. is_reverse
            do n=1, n_unique_values, 1
                distance_nearest_norm(n) = distance_nearest(n)
            end do
            call vector2sum1(distance_nearest_norm, n_unique_values)
            selected_idx = roulette_selection(distance_nearest_norm, n_unique_values, reverse=is_reverse)
            centroid_new = unique_values(selected_idx)
            centroid_coordinates = [centroid_coordinates, centroid_new]
            centroid_indices = [centroid_indices, selected_idx]

            ! Update Nearest Centroid Distance
            n_centroid_coordinates = size(centroid_coordinates)
            n_centroid_indices     = size(centroid_indices)
            call quick_sort(centroid_coordinates, n_centroid_coordinates)
            call quick_sort(centroid_indices,     n_centroid_indices)
            mid_idx = linear_search(centroid_indices, n_centroid_indices, selected_idx)
            min_idx = centroid_indices(mid_idx-1)
            max_idx = centroid_indices(mid_idx+1)
            mid_centroid = centroid_new

            if     ( min_idx .eq. 1_8 .and. max_idx .eq. n_unique_values ) then
                ! First
                do n=1, n_unique_values, 1
                    distance_nearest(n) = abs(unique_values(n)-mid_centroid)
                end do
            elseif ( min_idx .eq. 1_8 .and. max_idx .ne. n_unique_values ) then
                ! New Selected Centroid is most left position.
                max_centroid = unique_values(max_idx)
                do n=1, mid_idx, 1
                    distance_nearest(n) = abs(unique_values(n)-mid_centroid)
                end do
                do n=mid_idx+1, max_idx, 1
                    distance_nearest(n) = minval((/ & 
                        abs(unique_values(n)-mid_centroid),  &
                        abs(unique_values(n)-max_centroid)   &
                    /))
                end do
            elseif ( min_idx .ne. 1_8 .and. max_idx .eq. n_unique_values ) then
                ! New Selected Centroid is most right position.
                min_centroid = unique_values(min_idx)
                do n=min_idx, mid_idx, 1
                    distance_nearest(n) = minval((/ & 
                        abs(unique_values(n)-mid_centroid),  &
                        abs(unique_values(n)-min_centroid)   &
                    /))
                end do
                do n=mid_idx+1, n_unique_values, 1
                    distance_nearest(n) = abs(unique_values(n)-mid_centroid)
                end do
            else
                ! Others
                min_centroid = unique_values(min_idx)
                max_centroid = unique_values(max_idx)
                do n=min_idx, mid_idx, 1
                    distance_nearest(n) = minval((/ & 
                        abs(unique_values(n)-mid_centroid),  &
                        abs(unique_values(n)-min_centroid)   &
                    /))
                end do
                do n=mid_idx+1, max_idx, 1
                    distance_nearest(n) = minval((/ & 
                        abs(unique_values(n)-mid_centroid),  &
                        abs(unique_values(n)-max_centroid)   &
                    /))
                end do
            end if
        end do
! call date_and_time(values=date_value2)
! print*, "Select Initial Centroids", time_diff(date_value1, date_value2)

! call date_and_time(values=date_value1)
        ! print*, '============================================================='
        ! print*, "Update Centroids coordinates until convergence"
        call ifdealloc(centroid_indices)
        allocate(sum_distances(this%hparam%max_bins))
        allocate(bin_counter(this%hparam%max_bins))

        allocate(centroid_indices(n_unique_values))
        allocate(centroid_coordinates_old(this%hparam%max_bins))
        allocate(centroid_coordinates_new(this%hparam%max_bins))
        do i=1, size(centroid_coordinates), 1
            centroid_coordinates_old(i) = centroid_coordinates(i)
        end do
        ! print*, "INITIAL: ", centroid_coordinates_old

        do iter=1, this%hparam%max_iteration, 1
            ! Most Left Centroid 
            cluster_idx = 1_8
            centroid = centroid_coordinates_old(cluster_idx)
            max_idx = find_le(unique_values, n_unique_values, centroid)
            do i=1, max_idx, 1
                centroid_indices(i) = cluster_idx
            end do

            ! Most Right Centroid 
            cluster_idx = this%hparam%max_bins
            centroid = centroid_coordinates_old(cluster_idx)
            min_idx = find_ge(unique_values, n_unique_values, centroid)
            do i=min_idx, n_unique_values, 1
                centroid_indices(i) = cluster_idx
            end do

            ! print*, 1, max_idx
            ! Intermediate Centroids
            ! print*, '============================================================='
            do n=1, this%hparam%max_bins-1, 1
                centroid_left  = centroid_coordinates_old(n)
                centroid_right = centroid_coordinates_old(n+1)
                midpoint = (centroid_left + centroid_right) * .5d0

                left_idx     = find_gt(unique_values, n_unique_values, centroid_left)
                midpoint_idx = find_le(unique_values, n_unique_values, midpoint)
                right_idx    = find_le(unique_values, n_unique_values, centroid_right)

                ! print*, "---------------------------------------------------------------"
                ! print*, "     -> ", left_idx, centroid_left
                ! print*, "     -> ", midpoint_idx, midpoint
                ! print*, "     -> ", right_idx, centroid_right
                do i=left_idx, midpoint_idx, 1
                    centroid_indices(i) = n
                end do
                do i=midpoint_idx+1, right_idx, 1
                    centroid_indices(i) = n+1
                end do
            end do
            ! print*, min_idx, n_unique_values

            bin_counter   = 0_8
            sum_distances = 0d0
            do n=1, n_unique_values, 1
                idx        = centroid_indices(n)
                counter    = counts(n)
                coordinate = unique_values(n)
                bin_counter(idx)   = bin_counter(idx)   + counter
                sum_distances(idx) = sum_distances(idx) + counter * coordinate
            end do
            ! print*, '============================================================='
            ! print*, '============================================================='
            ! print*, "bin_counter:   ", bin_counter, sum(bin_counter), sum(counts), sum(bin_counter)-sum(counts), sum_check
            ! print*, "sum_distances: ", sum_distances

            do i=1, this%hparam%max_bins, 1
                centroid_coordinates_new(i) = sum_distances(i) / dble(bin_counter(i))
            end do
            print*, sum(abs(centroid_coordinates_new-centroid_coordinates_old)), & 
                maxval(abs(centroid_coordinates_new-centroid_coordinates_old))
            if (sum(abs(centroid_coordinates_new-centroid_coordinates_old)) .le. this%hparam%tolerance) exit
            centroid_coordinates_old = centroid_coordinates_new
            ! print*, centroid_coordinates_new, " : ", sum(abs(centroid_coordinates_new-centroid_coordinates_old))
            ! stop 
        end do
            ! print*, max_idx
            ! print*, centroid, unique_values(max_idx)
            ! print*, unique_values(max_idx-1:max_idx+1)

            allocate(this%thresholds_(0))
            do i=1, this%hparam%max_bins-1, 1
                this%thresholds_ = [this%thresholds_, (centroid_coordinates_old(i)+centroid_coordinates_old(i+1))*.5d0]
                print*, i, (centroid_coordinates_old(i)+centroid_coordinates_old(i+1))*.5d0
            end do
! call date_and_time(values=date_value2)
! print*, "Update Centroids coordinates until convergence", time_diff(date_value1, date_value2)

        return
        999 continue
        if (n_unique_values .eq. 1_8) then
            allocate(this%thresholds_(1))
            this%thresholds_ = huge(0d0)
        elseif ( n_unique_values .le. this%hparam%max_bins ) then
            allocate(this%thresholds_(0))
            do n=2, n_unique_values, 1
                this%thresholds_ = [this%thresholds_, (unique_values(n-1)+unique_values(n)) * .5d0]
            end do
            this%thresholds_ = [this%thresholds_, huge(0d0)]
        end if                
    end subroutine fit_kmeans_naive_discretizer


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
