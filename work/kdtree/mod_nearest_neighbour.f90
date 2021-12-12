module mod_nearest_neighbour
    !$ use omp_lib
    use mod_const
    use mod_common
    use mod_error
    use mod_timer
    use mod_sort
    use mod_linalg
    implicit none

    type node_type
        integer(kind=8) :: idx
        logical(kind=4) :: is_root=f_
        logical(kind=4) :: is_leaf=f_
        logical(kind=4) :: is_left=f_
        integer(kind=8) :: depth
        integer(kind=8) :: n_samples
        integer(kind=8) :: n_columns
        integer(kind=8) :: min_samples_in_leaf

        real(kind=8)    :: split_val = huge(0d0)
        integer(kind=8) :: split_fid = -1_8

        integer(kind=8)              :: data_index
        integer(kind=8), ALLOCATABLE :: data_indices(:)
        integer(kind=8), ALLOCATABLE :: indices(:)
        real(kind=8), allocatable    :: x(:,:), x_sq(:)

        type(node_type), pointer :: node_p_ptr => null()
        type(node_type), pointer :: node_l_ptr => null()
        type(node_type), pointer :: node_r_ptr => null()
    contains
        procedure :: info
        procedure :: init
        procedure :: adopting_twins_in_kdtree
    end type node_type

    type node_type_ptr
        type(node_type), pointer :: ptr
    end type node_type_ptr

    type kdtree
        type(node_type) :: root_node_
        type(node_type), pointer :: root_node_ptr_
        integer(kind=8) :: min_samples_in_leaf=5_8

        integer(kind=8) :: n_samples, n_columns
        real(kind=8), ALLOCATABLE :: x(:,:)
        real(kind=8), ALLOCATABLE :: x_t(:,:)
        real(kind=8), ALLOCATABLE :: x_sq_sum_row(:)
        real(kind=8), ALLOCATABLE :: q_sq_sum_row(:)
    contains
        procedure :: build
        procedure :: build_rec

        procedure :: query

        procedure :: query_nearest
        procedure :: query_nearest_rec_get_leaf_ptr
        procedure :: query_nearest_search_subtree

        procedure :: query_nearest_n_points
        procedure :: query_nearest_n_points_rec_get_leaf_ptr

    end type kdtree

    interface kdtree
        module procedure :: new_kdtree
    end interface kdtree

    type index
        integer(kind=8)              :: n
        integer(kind=8), allocatable :: idx(:)
    end type index

    type distance
        real(kind=8)              :: n
        real(kind=8)              :: max_dist
        real(kind=8), allocatable :: dst(:)
    end type distance

    type kdtree_results
        type(index), ALLOCATABLE :: indices(:)
        type(distance), ALLOCATABLE :: distances(:)
    end type kdtree_results

contains

    !> Construct New kdtree
    function new_kdtree(min_samples_in_leaf)
        implicit none
        type(kdtree) :: new_kdtree
        integer(kind=8), optional :: min_samples_in_leaf

        if ( present(min_samples_in_leaf) ) new_kdtree%min_samples_in_leaf = min_samples_in_leaf
    end function new_kdtree

    !> Initialize kdtree node
    subroutine init(this, is_root, is_leaf, is_left, depth, n_samples, n_columns, min_samples_in_leaf, &
        parent_node)
        implicit none
        class(node_type), intent(inout), target :: this
        logical(kind=4), intent(in) :: is_root, is_leaf, is_left
        integer(kind=8), intent(in) :: depth, n_samples, n_columns, min_samples_in_leaf
        type(node_type), intent(in), optional, target :: parent_node

        this%is_root = is_root
        this%is_leaf = is_leaf
        this%is_left = is_left
        this%depth = depth
        this%n_samples = n_samples
        this%n_columns = n_columns
        this%min_samples_in_leaf = min_samples_in_leaf

        this%split_val = huge(0d0)
        this%split_fid = -2_8

        this%data_index = -2_8

        call ifdealloc(this%data_indices)
        call ifdealloc(this%indices)

        if (ASSOCIATED(this%node_p_ptr)) nullify(this%node_p_ptr)
        if (ASSOCIATED(this%node_l_ptr)) nullify(this%node_l_ptr)
        if (ASSOCIATED(this%node_r_ptr)) nullify(this%node_r_ptr)

        if (present(parent_node)) then
            if (ASSOCIATED(this%node_p_ptr)) deallocate(this%node_p_ptr)
            allocate(this%node_p_ptr)
            this%node_p_ptr => parent_node
        end if
    end subroutine init

    !> Create Child Node
    subroutine adopting_twins_in_kdtree(this, median_index)
        implicit none
        class(node_type), intent(inout) :: this
        integer(kind=8), intent(in) :: median_index

        integer(kind=8) :: n_samples_l, n_samples_r, i

        n_samples_l = median_index-1
        n_samples_r = this%n_samples-median_index
        this%data_index = this%indices(median_index)

        allocate(this%node_l_ptr)
        call this%node_l_ptr%init(f_, f_, t_, this%depth+1, &
            n_samples_l, this%n_columns, this%min_samples_in_leaf, this)

        allocate(this%node_r_ptr)
        call this%node_r_ptr%init(f_, f_, f_, this%depth+1, &
            n_samples_r, this%n_columns, this%min_samples_in_leaf, this)

        allocate(this%node_l_ptr%indices(n_samples_l))
        do i=1, median_index-1, 1
            this%node_l_ptr%indices(i) = this%indices(i)
        end do

        allocate(this%node_r_ptr%indices(n_samples_r))
        do i=median_index+1, this%n_samples, 1
            this%node_r_ptr%indices(i-median_index) = this%indices(i)
        end do
    end subroutine adopting_twins_in_kdtree

    !> Print Node information
    subroutine info(this)
        implicit none
        class(node_type) :: this
        integer(kind=8) :: fin

        print*, '============================================================='
        print*, '============================================================='
        print*, "node%is_root                 : ", this%is_root
        print*, "node%is_leaf                 : ", this%is_leaf
        print*, "node%is_left                 : ", this%is_left
        print*, "node%idx                     : ", this%idx
        print*, "node%depth                   : ", this%depth
        print*, "node%n_samples               : ", this%n_samples
        print*, "node%n_columns               : ", this%n_columns
        print*, "node%min_samples_in_leaf     : ", this%min_samples_in_leaf

        print*, "node%split_val               : ", this%split_val
        print*, "node%split_fid               : ", this%split_fid

        print*, "node%data_index              : ", this%data_index
        print*, "allocated(node%data_indices) : ", allocated(this%data_indices)
        if (allocated(this%data_indices)) then
            fin = minval((/5, size(this%data_indices)/))
            print*, "this%data_indices            : ", this%data_indices(1:fin)
        end if
        print*, "allocated(node%indices)      : ", allocated(this%indices)
        if (allocated(this%data_indices)) then
            fin = minval((/5, size(this%indices)/))
            print*, "this%indices                 : ", this%indices(1:fin)
        end if
        ! print*, "allocated(node%node_p)       : ", allocated(this%node_p)
        print*, "associated(node%node_p)      : ", associated(this%node_p_ptr)
        print*, "associated(node%node_l)      : ", associated(this%node_l_ptr)
        print*, "associated(node%node_r)      : ", associated(this%node_r_ptr)
    end subroutine info

    !> Building kdtree, wrapping 'build_rec'
    subroutine build(this, x)
        implicit none
        class(kdtree) :: this
        real(kind=8), target  :: x(:,:)
        real(kind=8), pointer  :: x_ptr(:,:)

        type(node_type), TARGET  :: root_node
        type(node_type), POINTER :: root_node_ptr, hoge
        integer(kind=8) :: n_samples, n_columns, i, depth, counter, node_idx
        integer(kind=8), ALLOCATABLE :: indices(:)
        type(node_type), pointer :: node_tmp

        x_ptr => x
        n_samples = size(x(:,1))
        n_columns = size(x(1,:))
        this%n_columns = n_columns

        if (ASSOCIATED(this%root_node_ptr_)) nullify(this%root_node_ptr_)
        allocate(this%root_node_ptr_)
        call this%root_node_ptr_%init(t_, f_, f_, 0_8, n_samples, n_columns, this%min_samples_in_leaf)
        this%root_node_ptr_%idx = 0
        allocate(this%root_node_ptr_%indices(this%root_node_ptr_%n_samples))
        do i=1, this%root_node_ptr_%n_samples, 1
            this%root_node_ptr_%indices(i) = i
        end do

        node_idx = 1_8
        call this%build_rec(this%root_node_ptr_, x_ptr, node_idx)

        this%root_node_ = root_node
        this%n_samples = n_samples
        this%n_columns = n_columns
        call ifdealloc(this%x)
        allocate(this%x(this%n_samples, this%n_columns))
        this%x(:,:) = x(:,:)

        call ifdealloc(this%x_t)
        allocate(this%x_t(this%n_columns, this%n_samples))
        this%x_t(:,:) = transpose(x(:,:))

        call ifdealloc(this%x_sq_sum_row)
        allocate(this%x_sq_sum_row(this%n_samples))
        call matrix_sqsum_row(x, this%x_sq_sum_row, n_samples, n_columns, parallel=f_)
    end subroutine build

    !> Building kdtree recursively. Best-First Method.
    recursive subroutine build_rec(this, node, x_ptr, node_idx)
        implicit none
        class(kdtree) :: this
        type(node_type), pointer, intent(inout) :: node
        real(kind=8), pointer  :: x_ptr(:,:)
        integer(kind=8), intent(inout) :: node_idx

        integer(kind=8) :: i, idx, split_fid, med_idx
        integer(kind=8) :: n_samples_l, n_samples_r
        real(kind=8)    :: split_val
        type(node_type), target  :: node_l,     node_r

        real(kind=8), ALLOCATABLE :: f_tmp(:)
        integer(kind=8), ALLOCATABLE :: indices_l(:), indices_r(:)

        node%idx = node_idx

        if (this%min_samples_in_leaf > node%n_samples) then
            node%is_leaf = t_
            allocate(node%x(node%n_samples,this%n_columns))
            node%x(:,:) = x_ptr(node%indices,:)

            allocate(node%x_sq(node%n_samples))
            node%x_sq(:) = sum(node%x(:,:)*node%x(:,:), dim=2)
            return
        end if

        allocate(f_tmp(node%n_samples))

        ! Select Feature
        node%split_fid = mod(node%depth, node%n_columns)+1

        ! Extract Data
        do i=1, node%n_samples, 1
            idx = node%indices(i)
            f_tmp(i) = x_ptr(idx,node%split_fid)
        end do

        ! Calculate Median Index
        med_idx = node%n_samples / 2_8

        ! argsort
        call quick_argselect(f_tmp, node%indices, node%n_samples, med_idx)
        node%split_val = f_tmp(med_idx)

        call node%adopting_twins_in_kdtree(med_idx)

        node_idx = node_idx + 1
        call this%build_rec(node%node_l_ptr, x_ptr, node_idx)

        node_idx = node_idx + 1
        call this%build_rec(node%node_r_ptr, x_ptr, node_idx)
    end subroutine build_rec


    !> Get Neighbors
    function query(this, q, iii, iii_max, n_neighbors, radius)
        implicit none
        class(kdtree)                         :: this
        type(kdtree_results)                  :: query
        real(kind=8), target, intent(in)      :: q(:,:)
        integer(kind=8), optional, intent(in) :: n_neighbors
        real(kind=8), optional, intent(in)    :: radius
        integer(kind=8), intent(in)           :: iii, iii_max

        real(kind=8), pointer      :: q_ptr(:,:)

        if (present(n_neighbors) .and. present(radius)) then
            stop "Only one of 'n_neighbors' or 'radius' can be specified."
        end if

        q_ptr => q

        if (present(n_neighbors)) then
            if (n_neighbors < 1_8) then
                stop "'n_neighbors' must be greater than 1."
            elseif (n_neighbors .eq. 1_8) then
                query = this%query_nearest(q_ptr, iii, iii_max)
            else
                query = this%query_nearest_n_points(q_ptr, n_neighbors, iii, iii_max)
            end if
        end if

        if (present(radius)) then
            if (radius < 0d0) then
                stop "'radius' must be greater than 0."
            else
                print*, "Not-Implemented for 'radius'."
            end if
        end if
    end function query


    function query_nearest_n_points(this, q_ptr, n_neighbors, iii, iii_max)
        implicit none
        type(kdtree_results)              :: query_nearest_n_points
        class(kdtree)                     :: this
        real(kind=8), pointer, intent(in) :: q_ptr(:,:)
        integer(kind=8), intent(in)       :: n_neighbors
        integer(kind=8), intent(in)       :: iii, iii_max

        integer(kind=8)       :: n_samples, n_columns, q_shape(2)

        real(kind=8)                 :: val, nearest_dist, q_sq
        integer(kind=8)              :: i, nearest_idx, fid, q_idx
        integer(kind=8), allocatable :: indices(:), nearest_idxs(:)
        real(kind=8), allocatable :: nearest_dists(:), q_vals(:), q_sq_sum_row(:)
        type(node_type_ptr), ALLOCATABLE :: leaf_ptrs(:)
        type(node_type), target :: root_node
        type(node_type), pointer :: root_node_ptr, node_ptr, leaf_node_ptr

        integer(kind=8) :: date_value1(8), date_value2(8)
        integer(kind=8), save :: t_leaf, t_get_leaf, t_internal, t_search_best

        if (iii .eq. 1_8) then
            t_leaf = 0_8
            t_get_leaf = 0_8
            t_internal = 0_8
            t_search_best = 0_8
        end if

        ! print*, 1
        q_shape = shape(q_ptr)
        n_samples = q_shape(1)
        n_columns = q_shape(2)
        call ifdealloc(this%q_sq_sum_row)
        allocate(this%q_sq_sum_row(n_samples))
        call matrix_sqsum_row(q_ptr, this%q_sq_sum_row, n_samples, n_columns, parallel=f_)

        ! print*, 2
        if ( allocated(query_nearest_n_points%indices) ) deallocate(query_nearest_n_points%indices)
        if ( allocated(query_nearest_n_points%distances) ) deallocate(query_nearest_n_points%distances)
        allocate(query_nearest_n_points%indices(n_samples))
        allocate(query_nearest_n_points%distances(n_samples))
        do i=1, n_samples, 1
            allocate(query_nearest_n_points%indices(i)%idx(n_neighbors))
            allocate(query_nearest_n_points%distances(i)%dst(n_neighbors))

            query_nearest_n_points%indices(i)%n=0_8
            query_nearest_n_points%indices(i)%idx(:) = -2_8

            query_nearest_n_points%distances(i)%n=0_8
            query_nearest_n_points%distances(i)%max_dist = huge(0d0)
            query_nearest_n_points%distances(i)%dst(:) = huge(0d0)
        end do
        allocate(q_vals(n_columns))

        ! print*, 5
        !$omp parallel num_threads(6)
        !$omp do private(leaf_node_ptr, nearest_dist, nearest_idx, i, node_ptr, fid, val, q_vals, q_idx, q_sq) &
        !$omp private(nearest_dists, nearest_idxs) &
        !$omp reduction(+:t_get_leaf) reduction(+:t_search_best)
        do i=1, n_samples, 1
            q_idx = i
            q_vals = q_ptr(q_idx,:)
            q_sq = this%q_sq_sum_row(q_idx)
            nearest_dist = huge(nearest_dist)
            nearest_idx = -2
            nearest_dists(:) = huge(0d0)
            nearest_idxs(:) = -2
            call date_and_time(values=date_value1)
            call this%query_nearest_n_points_rec_get_leaf_ptr(this%root_node_ptr_, n_neighbors, q_vals, q_sq, & 
                leaf_node_ptr, nearest_dists, nearest_idxs, n_columns)
            call date_and_time(values=date_value2)
            t_get_leaf = t_get_leaf + time_diff(date_value1, date_value2)

            cycle

            node_ptr => leaf_node_ptr

            call date_and_time(values=date_value1)
            do while (t_)
                fid = node_ptr%node_p_ptr%split_fid
                val = node_ptr%node_p_ptr%split_val

                if ( (q_vals(fid)-val)**2d0 < nearest_dist ) then
                    if (node_ptr%is_left) then
                        call this%query_nearest_search_subtree(node_ptr%node_p_ptr%node_r_ptr, q_vals, q_sq, & 
                            nearest_idx, nearest_dist, n_columns)
                    else
                        call this%query_nearest_search_subtree(node_ptr%node_p_ptr%node_l_ptr, q_vals, q_sq, & 
                            nearest_idx, nearest_dist, n_columns)
                    end if
                end if
                node_ptr => node_ptr%node_p_ptr
                if (.not. ASSOCIATED(node_ptr%node_p_ptr)) exit
            end do
            call date_and_time(values=date_value2)
            t_search_best = t_search_best + time_diff(date_value1, date_value2)
            query_nearest_n_points%indices(i)%idx(1) = nearest_idx
            query_nearest_n_points%distances(i)%dst(1) = sqrt(nearest_dist)
        end do
        !$omp end do
        !$omp end parallel 

        if (iii .eq. iii_max) then
            print*, "t_get_leaf:       ", t_get_leaf / dble(iii_max)
            print*, "t_leaf:           ", t_leaf / dble(iii_max)
            print*, "t_internal:       ", t_internal / dble(iii_max)
            print*, "t_search_best:    ", t_search_best / dble(iii_max)
        end if
    end function query_nearest_n_points

    !> Get Leaf Node pointer of Query point
    recursive subroutine query_nearest_n_points_rec_get_leaf_ptr(this, root_node_ptr, n_neighbors, q, q_sq, leaf_ptr, &
        nearest_dsts, nearest_idxs, n_columns)
        implicit none
        class(kdtree)            :: this
        type(node_type), pointer :: root_node_ptr
        integer(kind=8), intent(in) :: n_neighbors
        real(kind=8), intent(in) :: q(n_columns)
        real(kind=8), intent(in) :: q_sq
        type(node_type), pointer :: leaf_ptr
        real(kind=8), intent(inout) :: nearest_dsts(n_neighbors)
        integer(kind=8), intent(inout) :: nearest_idxs(n_neighbors)
        integer(kind=8)          :: n_columns

        real(kind=8) :: f_val, dist, dist_new, dist_old, idx_new, idx_old, dist_q, dist_from_q_to_x
        integer(kind=8) :: i, j, idx, x_idx, f_idx, n_samples_l, n_samples_r, idx_l, idx_r, n
        integer(kind=8), allocatable :: indices_l(:), indices_r(:)
        logical(kind=4), allocatable :: tmp_l(:)
        logical(kind=4) :: goto_left

        ! if (root_node_ptr%is_leaf) then
        !     ! Search Closest Point from Leaf Node
        !     dist_old = nearest_dist
        !     idx_old  = nearest_idx
        !     leaf_ptr => root_node_ptr
        !     do j=1, leaf_ptr%n_samples, 1
        !         x_idx = leaf_ptr%indices(j)
        !         dist_new = q_sq + this%x_sq_sum_row(x_idx) - 2d0*dot_product( q(:),this%x(x_idx,:) )
        !         if ( dist_new < dist_old ) then
        !             idx_old = x_idx
        !             dist_old = dist_new
        !         end if
        !     end do
        !     nearest_dist = dist_old
        !     nearest_idx = idx_old
        !     return
        ! end if

        ! x_idx = root_node_ptr%data_index
        ! dist_from_q_to_x = q_sq + this%x_sq_sum_row(x_idx) - 2d0*dot_product( q(:),this%x(x_idx,:) )
        ! if (nearest_dist > dist_from_q_to_x) then
        !     nearest_dist = dist_from_q_to_x 
        !     nearest_idx = x_idx
        ! end if

        ! f_idx = root_node_ptr%split_fid
        ! f_val = root_node_ptr%split_val

        ! goto_left = q(f_idx) <= f_val

        ! if (goto_left) then
        !     call query_nearest_n_points_rec_get_leaf_ptr(this, root_node_ptr%node_l_ptr, n_neighbors, q, q_sq, leaf_ptr, &
        !         nearest_dist, nearest_idx, n_columns)
        ! else
        !     call query_nearest_n_points_rec_get_leaf_ptr(this, root_node_ptr%node_r_ptr, n_neighbors, q, q_sq, leaf_ptr, &
        !         nearest_dist, nearest_idx, n_columns)
        ! end if
    end subroutine query_nearest_n_points_rec_get_leaf_ptr








    !> Get Nearest Neighbor
    function query_nearest(this, q_ptr, iii, iii_max)
        implicit none
        type(kdtree_results)     :: query_nearest
        class(kdtree)                    :: this
        real(kind=8), pointer, intent(in) :: q_ptr(:,:)
        integer(kind=8), intent(in) :: iii, iii_max

        integer(kind=8)       :: n_samples, n_columns, q_shape(2)

        real(kind=8)                 :: val, nearest_dist, q_sq
        integer(kind=8)              :: i, nearest_idx, fid, q_idx
        integer(kind=8), allocatable :: indices(:), nearest_idxs(:)
        real(kind=8), allocatable :: nearest_dists(:), q_vals(:), q_sq_sum_row(:)
        type(node_type_ptr), ALLOCATABLE :: leaf_ptrs(:)
        type(node_type), target :: root_node
        type(node_type), pointer :: root_node_ptr, node_ptr, leaf_node_ptr

        integer(kind=8) :: date_value1(8), date_value2(8)
        integer(kind=8), save :: time_leaf, t_get_leaf, time_internal, t_search_best

        if (iii .eq. 1_8) then
            time_leaf = 0_8
            t_get_leaf = 0_8
            time_internal = 0_8
            t_search_best = 0_8
        end if

        ! print*, 1
        q_shape = shape(q_ptr)
        n_samples = q_shape(1)
        n_columns = q_shape(2)
        call ifdealloc(this%q_sq_sum_row)
        allocate(this%q_sq_sum_row(n_samples))
        call matrix_sqsum_row(q_ptr, this%q_sq_sum_row, n_samples, n_columns, parallel=f_)

        ! print*, 2
        if ( allocated(query_nearest%indices) ) deallocate(query_nearest%indices)
        if ( allocated(query_nearest%distances) ) deallocate(query_nearest%distances)
        allocate(query_nearest%indices(n_samples))
        allocate(query_nearest%distances(n_samples))
        do i=1, n_samples, 1
            allocate(query_nearest%indices(i)%idx(1))
            allocate(query_nearest%distances(i)%dst(1))
        end do
        allocate(q_vals(n_columns))

        ! print*, 5
        !$omp parallel num_threads(6)
        !$omp do private(leaf_node_ptr, nearest_dist, nearest_idx, i, node_ptr, fid, val, q_vals, q_idx, q_sq) &
        !$omp reduction(+:t_get_leaf) reduction(+:t_search_best) reduction(+:time_leaf) reduction(+:time_internal)
        do i=1, n_samples, 1
            q_idx = i
            q_vals = q_ptr(q_idx,:)
            q_sq = this%q_sq_sum_row(q_idx)
            nearest_dist = huge(nearest_dist)
            nearest_idx = -2
            call date_and_time(values=date_value1)
            call this%query_nearest_rec_get_leaf_ptr(this%root_node_ptr_, q_vals, q_sq, & 
                leaf_node_ptr, nearest_dist, nearest_idx, n_columns)
            call date_and_time(values=date_value2)
            t_get_leaf = t_get_leaf + time_diff(date_value1, date_value2)

            node_ptr => leaf_node_ptr

            call date_and_time(values=date_value1)
            do while (t_)
                fid = node_ptr%node_p_ptr%split_fid
                val = node_ptr%node_p_ptr%split_val

                if ( (q_vals(fid)-val)**2d0 < nearest_dist ) then
                    if (node_ptr%is_left) then
                        call this%query_nearest_search_subtree(node_ptr%node_p_ptr%node_r_ptr, q_vals, q_sq, & 
                            nearest_idx, nearest_dist, n_columns)
                    else
                        call this%query_nearest_search_subtree(node_ptr%node_p_ptr%node_l_ptr, q_vals, q_sq, & 
                            nearest_idx, nearest_dist, n_columns)
                    end if
                end if
                node_ptr => node_ptr%node_p_ptr
                if (.not. ASSOCIATED(node_ptr%node_p_ptr)) exit
            end do
            call date_and_time(values=date_value2)
            t_search_best = t_search_best + time_diff(date_value1, date_value2)
            query_nearest%indices(i)%idx(1) = nearest_idx
            query_nearest%distances(i)%dst(1) = sqrt(nearest_dist)
        end do
        !$omp end do
        !$omp end parallel 

        if (iii .eq. iii_max) then
            print*, "t_get_leaf:       ", t_get_leaf / dble(iii_max)
            print*, "t_search_best:    ", t_search_best / dble(iii_max)
            print*, "    t_leaf:       ", time_leaf / dble(iii_max)
            print*, "    t_internal:   ", time_internal / dble(iii_max)
            print*, "t_total:          ", (t_get_leaf+t_search_best) / dble(iii_max)
        end if

    end function query_nearest

    !> Get Leaf Node pointer of Query point
    recursive subroutine query_nearest_rec_get_leaf_ptr(this, root_node_ptr, q, q_sq, leaf_ptr, &
        nearest_dist, nearest_idx, n_columns)
        implicit none
        class(kdtree)            :: this
        type(node_type), pointer :: root_node_ptr
        real(kind=8), intent(in) :: q(n_columns)
        real(kind=8), intent(in) :: q_sq
        type(node_type), pointer :: leaf_ptr
        real(kind=8), intent(inout) :: nearest_dist
        integer(kind=8), intent(inout) :: nearest_idx
        integer(kind=8), intent(in)    :: n_columns

        real(kind=8) :: f_val, dist, dist_new, dist_old, idx_new, idx_old, dist_q, dist_from_q_to_x
        integer(kind=8) :: i, j, idx, x_idx, f_idx, n_samples_l, n_samples_r, idx_l, idx_r
        integer(kind=8), allocatable :: indices_l(:), indices_r(:)
        logical(kind=4), allocatable :: tmp_l(:)
        logical(kind=4) :: goto_left
        real(kind=8), allocatable :: XxQ(:)

        if (root_node_ptr%is_leaf) then
            ! Search Closest Point from Leaf Node
            allocate(XxQ(root_node_ptr%n_samples))
            leaf_ptr => root_node_ptr
            call multi_mat_vec(root_node_ptr%x, q, XxQ, &
                root_node_ptr%n_samples, n_columns, f_)
            XxQ(:) = q_sq + root_node_ptr%x_sq(:) - 2d0*XxQ(:)
            dist_new = minval(XxQ)
            x_idx  = minloc(XxQ, dim=1)
            if (dist_new < nearest_dist) then
                nearest_dist = dist_new
                nearest_idx = root_node_ptr%indices(x_idx)
            end if

            ! dist_old = nearest_dist
            ! idx_old  = nearest_idx
            ! do j=1, leaf_ptr%n_samples, 1
            !     x_idx = leaf_ptr%indices(j)
            !     dist_new = q_sq + this%x_sq_sum_row(x_idx) - 2d0*dot_product( q(:),this%x(x_idx,:) )
            !     if ( dist_new < dist_old ) then
            !         idx_old = x_idx
            !         dist_old = dist_new
            !     end if
            ! end do
            ! nearest_dist = dist_old
            ! nearest_idx = idx_old
            return
        end if

        x_idx = root_node_ptr%data_index
        dist_from_q_to_x = q_sq + this%x_sq_sum_row(x_idx) - 2d0*dot_product( q(:),this%x(x_idx,:) )
        if (nearest_dist > dist_from_q_to_x) then
            nearest_dist = dist_from_q_to_x 
            nearest_idx = x_idx
        end if

        f_idx = root_node_ptr%split_fid
        f_val = root_node_ptr%split_val

        goto_left = q(f_idx) <= f_val

        if (goto_left) then
            call query_nearest_rec_get_leaf_ptr(this, root_node_ptr%node_l_ptr, q, q_sq, leaf_ptr, &
                nearest_dist, nearest_idx, n_columns)
        else
            call query_nearest_rec_get_leaf_ptr(this, root_node_ptr%node_r_ptr, q, q_sq, leaf_ptr, &
                nearest_dist, nearest_idx, n_columns)
        end if
    end subroutine query_nearest_rec_get_leaf_ptr

    !> Get nearest data point of Query point from subtree
    recursive subroutine query_nearest_search_subtree(this, subtree_root_node_ptr, q, q_sq, &
        nearest_idx, nearest_dst, n_columns)
        implicit none
        class(kdtree) :: this
        type(node_type), pointer       :: subtree_root_node_ptr
        real(kind=8), intent(in)       :: q(n_columns), q_sq
        integer(kind=8), intent(inout) :: nearest_idx
        real(kind=8), intent(inout)    :: nearest_dst
        integer(kind=8), intent(in)    :: n_columns

        integer(kind=8) :: i, j, idx_old, idx_new, x_idx, fid
        real(kind=8) :: dist_old, dist_new, dist, dist_from_q_to_plane, val_x, val_q, dist_from_q_to_x
        real(kind=8), allocatable :: XxQ(:)

        if (subtree_root_node_ptr%is_leaf) then
            ! Search Closest Point from Leaf Node
            allocate(XxQ(subtree_root_node_ptr%n_samples))
            call multi_mat_vec(subtree_root_node_ptr%x, q, XxQ, &
                subtree_root_node_ptr%n_samples, n_columns, f_)
            XxQ(:) = q_sq + subtree_root_node_ptr%x_sq(:) - 2d0*XxQ(:)
            dist_new = minval(XxQ)
            x_idx  = minloc(XxQ, dim=1)
            if (dist_new < nearest_dst) then
                nearest_dst = dist_new
                nearest_idx = subtree_root_node_ptr%indices(x_idx)
            end if

            ! dist_old = nearest_dst
            ! idx_old = nearest_idx
            ! do j=1, subtree_root_node_ptr%n_samples, 1
            !     x_idx = subtree_root_node_ptr%indices(j)
            !     dist_new = q_sq + this%x_sq_sum_row(x_idx) - 2d0*sum( q(:)*this%x_t(:,x_idx) )
            !     if ( dist_new < dist_old ) then
            !         idx_old = x_idx
            !         dist_old = dist_new
            !     end if
            ! end do
            ! nearest_dst = dist_old
            ! nearest_idx = idx_old
            return
        else
            ! Compute Distance From Q to Split Plane
            fid = subtree_root_node_ptr%split_fid
            val_x = subtree_root_node_ptr%split_val
            val_q = q(fid)
            dist_from_q_to_plane = (val_q-val_x)**2d0

            if (nearest_dst < dist_from_q_to_plane) then
                if (val_q <= val_x) then
                    call this%query_nearest_search_subtree(subtree_root_node_ptr%node_l_ptr, &
                        q, q_sq, nearest_idx, nearest_dst, n_columns)
                else
                    call this%query_nearest_search_subtree(subtree_root_node_ptr%node_r_ptr, &
                        q, q_sq, nearest_idx, nearest_dst, n_columns)
                end if
            else
                x_idx = subtree_root_node_ptr%data_index
                dist_from_q_to_x = q_sq + this%x_sq_sum_row(x_idx) - 2d0*dot_product( q(:),this%x_t(:,x_idx) )
                if (nearest_dst > dist_from_q_to_x) then
                    nearest_dst = dist_from_q_to_x 
                    nearest_idx = x_idx
                end if

                call this%query_nearest_search_subtree(subtree_root_node_ptr%node_l_ptr, &
                    q, q_sq, nearest_idx, nearest_dst, n_columns)
                call this%query_nearest_search_subtree(subtree_root_node_ptr%node_r_ptr, &
                    q, q_sq, nearest_idx, nearest_dst, n_columns)
            end if
        end if
    end subroutine query_nearest_search_subtree

end module mod_nearest_neighbour
