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
        integer(kind=8) :: idx !< node index
        logical(kind=4) :: is_root=f_ !< is root node or not
        logical(kind=4) :: is_leaf=f_ !< is leaf node or not
        logical(kind=4) :: is_left=f_ !< is left child node or not
        integer(kind=8) :: depth !< node depth
        integer(kind=8) :: n_samples !< number of samples
        integer(kind=8) :: n_samples_ !< number of samples including split point indices
        integer(kind=8) :: n_columns !< number of samples
        integer(kind=8) :: min_samples_in_leaf !< minimum number of samples in leaf node

        real(kind=8)    :: split_val = huge(0d0) !< split threshold
        integer(kind=8) :: split_fid = -1_8 !< split feature index

        integer(kind=8)              :: data_index !< split data point index
        integer(kind=8), ALLOCATABLE :: indices(:) !< sample point indices
        integer(kind=8), ALLOCATABLE :: indices_(:) !< include split point indices
        real(kind=8), allocatable    :: x_(:,:), x_sq_(:) !< explanatory and its sum squared sum by row with split points

        type(node_type), pointer :: node_p_ptr => null() !< pointer to parent node
        type(node_type), pointer :: node_l_ptr => null() !< pointer to child left node
        type(node_type), pointer :: node_r_ptr => null() !< pointer to child right node
    contains
        procedure :: info
        procedure :: init
        procedure :: adopting_twins_in_kdtree
    end type node_type

    type kdtree
        type(node_type), pointer :: root_node_ptr_ !< pointer ot root node
        integer(kind=8) :: min_samples_in_leaf = 128_8 !< minimum number of samples in leaf node

        integer(kind=8) :: n_samples !< number of samples
        integer(kind=8) :: n_columns !< number of columns
        real(kind=8), ALLOCATABLE :: x_sq_sum_row(:) !< sum squared value by row of explanatory data
        real(kind=8), ALLOCATABLE :: q_sq_sum_row(:) !< sum squared value by row of query data
    contains
        procedure :: build
        procedure :: build_rec

        procedure :: query

        procedure :: query_nearest_n_neighbors
        procedure :: query_nearest_n_neighbors_rec_get_leaf_ptr
        procedure :: query_nearest_n_neighbors_search_subtree

        procedure :: query_nearest_radius
        procedure :: query_nearest_radius_rec_get_leaf_ptr
        procedure :: query_nearest_radius_search_subtree
    end type kdtree

    interface kdtree
        module procedure :: new_kdtree
    end interface kdtree

    type index
        integer(kind=8), allocatable :: idx(:) !< indices from query to N-th nearest points
    end type index

    type distance
        real(kind=8), allocatable :: dst(:) !< distances from query to N-th nearest points
    end type distance

    type kdtree_results
        type(index),    ALLOCATABLE :: indices(:)
        type(distance), ALLOCATABLE :: distances(:)
    end type kdtree_results

contains

    !> Construct New 'kdtree' object.
    !! \param min_samples_in_leaf minimum number of samples in leaf node.
    function new_kdtree(min_samples_in_leaf)
        implicit none
        type(kdtree) :: new_kdtree
        integer(kind=8), optional :: min_samples_in_leaf

        if ( present(min_samples_in_leaf) ) new_kdtree%min_samples_in_leaf = min_samples_in_leaf
    end function new_kdtree

    !> Initialize kdtree node
    !! \param is_root root node or not
    !! \param is_leaf leaf node or not
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


    !> Create Child Node.
    !! \param median_index median point index
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
        print*, "allocated(node%indices)      : ", allocated(this%indices)
        if (allocated(this%indices)) then
            fin = minval((/5, size(this%indices)/))
            print*, "this%indices                 : ", this%indices(1:fin)
        end if
        print*, "associated(node%node_p)      : ", associated(this%node_p_ptr)
        print*, "associated(node%node_l)      : ", associated(this%node_l_ptr)
        print*, "associated(node%node_r)      : ", associated(this%node_r_ptr)
    end subroutine info


    !> Building kdtree, wrapping 'build_rec'
    !! \param x input explanatory variable
    subroutine build(this, x)
        implicit none
        class(kdtree) :: this
        real(kind=8), target  :: x(:,:)
        real(kind=8), pointer  :: x_ptr(:,:)

        type(node_type), TARGET  :: root_node
        type(node_type), POINTER :: root_node_ptr, hoge
        integer(kind=8) :: n_samples, n_columns, i, depth, counter, node_idx
        integer(kind=8), ALLOCATABLE :: indices(:), split_indices(:)
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
        allocate(split_indices(0))
        call this%build_rec(this%root_node_ptr_, x_ptr, node_idx, split_indices)

        this%n_samples = n_samples
        this%n_columns = n_columns
    end subroutine build


    !> Building kdtree recursively. Best-First Method.
    !! \param node_ptr pointer to node
    !! \param x_ptr pointer to input explanatory variable
    !! \param node_idx index of node
    !! \param split_indices indices of split points from root to parent node.
    recursive subroutine build_rec(this, node_ptr, x_ptr, node_idx, split_indices)
        implicit none
        class(kdtree) :: this
        type(node_type), pointer, intent(inout) :: node_ptr
        real(kind=8), pointer  :: x_ptr(:,:)
        integer(kind=8), intent(inout) :: node_idx
        integer(kind=8), allocatable, intent(inout) :: split_indices(:)

        integer(kind=8) :: i, idx, split_fid, med_idx
        integer(kind=8) :: n_samples_l, n_samples_r
        real(kind=8)    :: split_val
        type(node_type), target  :: node_l,     node_r

        real(kind=8), ALLOCATABLE :: f_tmp(:)
        integer(kind=8), ALLOCATABLE :: indices_l(:), indices_r(:)
        integer(kind=8), ALLOCATABLE :: split_indices_l(:), split_indices_r(:)

        node_ptr%idx = node_idx

        if (this%min_samples_in_leaf > node_ptr%n_samples) then
            node_ptr%is_leaf = t_

            node_ptr%indices_ = [node_ptr%indices, split_indices]
            node_ptr%n_samples_ = size(node_ptr%indices_)
            allocate(node_ptr%x_(node_ptr%n_samples_,this%n_columns))
            node_ptr%x_(:,:) = x_ptr(node_ptr%indices_,:)

            allocate(node_ptr%x_sq_(node_ptr%n_samples_))
            node_ptr%x_sq_(:) = sum(node_ptr%x_(:,:)*node_ptr%x_(:,:), dim=2)
            return
        end if

        allocate(f_tmp(node_ptr%n_samples))

        ! Select Feature
        node_ptr%split_fid = mod(node_ptr%depth, node_ptr%n_columns)+1

        ! Extract Data
        do i=1, node_ptr%n_samples, 1
            idx = node_ptr%indices(i)
            f_tmp(i) = x_ptr(idx,node_ptr%split_fid)
        end do

        ! Calculate Median Index
        med_idx = node_ptr%n_samples / 2_8

        ! argsort
        call quick_argselect(f_tmp, node_ptr%indices, node_ptr%n_samples, med_idx)
        node_ptr%split_val = f_tmp(med_idx)

        call node_ptr%adopting_twins_in_kdtree(med_idx)

        node_idx = node_idx + 1
        allocate(split_indices_l(size(split_indices)))
        split_indices_l = [split_indices, node_ptr%indices(med_idx)]
        call this%build_rec(node_ptr%node_l_ptr, x_ptr, node_idx, split_indices_l)

        node_idx = node_idx + 1
        allocate(split_indices_r(size(split_indices)))
        split_indices_r = split_indices
        call this%build_rec(node_ptr%node_r_ptr, x_ptr, node_idx, split_indices_r)
    end subroutine build_rec


    !> Get Neighbors . Euclid distance Only.
    !! \param q query points
    !! \param n_neighbors number of nearest neighbors.
    !! \param radius ball raius size
    function query(this, q, n_neighbors, radius)
        implicit none
        class(kdtree)                         :: this
        type(kdtree_results)                  :: query
        real(kind=8), target, intent(in)      :: q(:,:)
        integer(kind=8), optional, intent(in) :: n_neighbors
        real(kind=8), optional, intent(in)    :: radius

        real(kind=8), pointer      :: q_ptr(:,:)
        real(kind=8) :: radius_sq

        if (present(n_neighbors) .and. present(radius)) then
            stop "Only one of 'n_neighbors' or 'radius' can be specified."
        end if

        q_ptr => q

        if (present(n_neighbors)) then
            if (n_neighbors < 1_8) then
                stop "'n_neighbors' must be greater equal 1."
            else
                query = this%query_nearest_n_neighbors(q_ptr, n_neighbors)
            end if
        end if

        if (present(radius)) then
            if (radius < 0d0) then
                stop "'radius' must be greater equal 0."
            else
                radius_sq = radius**2d0
                query = this%query_nearest_radius(q_ptr, radius_sq)
            end if
        end if
    end function query


    !> Get the nearest neighbors in radius R
    !! \param q_ptr pointer to query points
    !! \param square of radius to search
    function query_nearest_radius(this, q_ptr, radius_sq)
        implicit none
        type(kdtree_results)     :: query_nearest_radius
        class(kdtree)            :: this
        real(kind=8), pointer, intent(in) :: q_ptr(:,:)
        real(kind=8), intent(in) :: radius_sq

        integer(kind=8)       :: n_samples, n_columns, q_shape(2)

        real(kind=8)                 :: val, max_radius, q_sq
        integer(kind=8)              :: i, nearest_idx, fid, q_idx
        integer(kind=8), allocatable :: indices(:), nearest_idxs(:)
        real(kind=8), allocatable :: nearest_dsts(:), q_vals(:)
        type(node_type), target :: root_node
        type(node_type), pointer :: root_node_ptr, node_ptr, leaf_node_ptr

        integer(kind=8) :: date_value1(8), date_value2(8), n_size
        integer(kind=8), save :: time_leaf, t_get_leaf, time_internal, t_search_best

        ! print*, 1
        q_shape = shape(q_ptr)
        n_samples = q_shape(1)
        n_columns = q_shape(2)
        call ifdealloc(this%q_sq_sum_row)
        allocate(this%q_sq_sum_row(n_samples))
        call matrix_sqsum_row(q_ptr, this%q_sq_sum_row, n_samples, n_columns, parallel=f_)

        ! print*, 2
        if ( allocated(query_nearest_radius%indices) ) &
            deallocate(query_nearest_radius%indices)
        if ( allocated(query_nearest_radius%distances) ) &
            deallocate(query_nearest_radius%distances)
        allocate(query_nearest_radius%indices(n_samples))
        allocate(query_nearest_radius%distances(n_samples))
        allocate(q_vals(n_columns))

        !$omp parallel num_threads(6)
        !$omp do private(leaf_node_ptr, nearest_dsts, nearest_idxs, i, node_ptr, fid, val, q_vals, q_idx, q_sq, max_radius) &
        !$omp reduction(+:t_get_leaf) reduction(+:t_search_best) reduction(+:time_leaf) reduction(+:time_internal)
        do i=1, n_samples, 1
            q_idx = i
            q_vals = q_ptr(q_idx,:)
            q_sq = this%q_sq_sum_row(q_idx)
            allocate(nearest_dsts(1))
            allocate(nearest_idxs(1))
            nearest_dsts = radius_sq
            nearest_idxs = -2
            call this%query_nearest_radius_rec_get_leaf_ptr(this%root_node_ptr_, q_vals, q_sq, & 
                leaf_node_ptr, nearest_dsts, nearest_idxs, n_columns, radius_sq)

            node_ptr => leaf_node_ptr

            do while (t_)
                fid = node_ptr%node_p_ptr%split_fid
                val = node_ptr%node_p_ptr%split_val
                max_radius = minval(nearest_dsts)

                if ( (q_vals(fid)-val)**2d0 < max_radius ) then
                    if (node_ptr%is_left) then
                        call this%query_nearest_radius_search_subtree(node_ptr%node_p_ptr%node_r_ptr, &
                            q_vals, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
                    else
                        call this%query_nearest_radius_search_subtree(node_ptr%node_p_ptr%node_l_ptr, &
                            q_vals, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
                    end if
                end if
                node_ptr => node_ptr%node_p_ptr
                if (.not. ASSOCIATED(node_ptr%node_p_ptr)) exit
            end do

            call quick_argsort(nearest_dsts, nearest_idxs, size(nearest_idxs)+0_8)
            n_size = size(nearest_idxs)-1
            allocate(query_nearest_radius%indices(i)%idx(n_size))
            allocate(query_nearest_radius%distances(i)%dst(n_size))
            query_nearest_radius%indices(i)%idx = nearest_idxs(1:n_size)
            query_nearest_radius%distances(i)%dst = sqrt(nearest_dsts(1:n_size))
            deallocate(nearest_dsts)
            deallocate(nearest_idxs)
        end do
        !$omp end do
        !$omp end parallel
    end function query_nearest_radius


    !> Get Leaf Node pointer of Query point
    !! \param root_node_ptr pointer to root node
    !! \param q a coordinate of query point
    !! \param q_sq a square norm of query point
    !! \param leaf_ptr pointer to leaf node of 'q' to return
    !! \param nearest_dsts nearest distances in radius
    !! \param nearest_idxs nearest point indices in radius
    !! \param n_columns number of columns of q
    !! \param radius_sq square of raius
    recursive subroutine query_nearest_radius_rec_get_leaf_ptr(this, root_node_ptr, q, q_sq, leaf_ptr, &
        nearest_dsts, nearest_idxs, n_columns, radius_sq)
        implicit none
        class(kdtree)            :: this
        type(node_type), pointer :: root_node_ptr
        real(kind=8), intent(in) :: q(n_columns)
        real(kind=8), intent(in) :: q_sq
        type(node_type), pointer :: leaf_ptr
        real(kind=8), allocatable, intent(inout) :: nearest_dsts(:)
        integer(kind=8), allocatable, intent(inout) :: nearest_idxs(:)
        integer(kind=8), intent(in)    :: n_columns
        real(kind=8), intent(in) :: radius_sq

        real(kind=8) :: f_val, dist, dist_new, dist_old, idx_new, idx_old, dist_q, dist_from_q_to_x
        integer(kind=8) :: i, j, idx, x_idx, f_idx, n_samples_l, n_samples_r, idx_l, idx_r
        integer(kind=8), allocatable :: indices_l(:), indices_r(:), indices(:), tmp_i(:)
        logical(kind=4), allocatable :: tmp_l(:)
        logical(kind=4) :: goto_left
        real(kind=8), allocatable :: XxQ(:), tmp_d(:)
        integer(kind=8) :: n

        if (root_node_ptr%is_leaf) then
            ! Search Closest Point from Leaf Node
            allocate(XxQ(root_node_ptr%n_samples_))
            allocate(indices(root_node_ptr%n_samples_))
            leaf_ptr => root_node_ptr
            call multi_mat_vec(root_node_ptr%x_, q, XxQ, &
                root_node_ptr%n_samples_, n_columns, f_)
            XxQ(:) = q_sq + root_node_ptr%x_sq_(:) - 2d0*XxQ(:)
            if ( radius_sq < minval(XxQ) ) then
                !  skip
            else
                do i=1, root_node_ptr%n_samples_
                    indices(i) = root_node_ptr%indices_(i)
                end do
                n = count(XxQ <= radius_sq)
                if ( n > 0) then
                    call quick_argselect(XxQ, indices, size(XxQ)+0_8, n)
                    nearest_dsts = [nearest_dsts, XxQ(1:n)]
                    nearest_idxs = [nearest_idxs, indices(1:n)]
                end if
            end if
            return
        end if

        f_idx = root_node_ptr%split_fid
        f_val = root_node_ptr%split_val

        goto_left = q(f_idx) <= f_val

        if (goto_left) then
            call query_nearest_radius_rec_get_leaf_ptr(this, root_node_ptr%node_l_ptr, q, q_sq, leaf_ptr, &
                nearest_dsts, nearest_idxs, n_columns, radius_sq)
        else
            call query_nearest_radius_rec_get_leaf_ptr(this, root_node_ptr%node_r_ptr, q, q_sq, leaf_ptr, &
                nearest_dsts, nearest_idxs, n_columns, radius_sq)
        end if
    end subroutine query_nearest_radius_rec_get_leaf_ptr


    !> Get nearest data point of Query point from subtree
    !! \param subtree_root_node_ptr pointer to root node of subtree to search
    !! \param q a coordinate of query point
    !! \param q_sq a square norm of query point
    !! \param nearest_dsts nearest distances in radius
    !! \param nearest_idxs nearest point indices in radius
    !! \param n_columns number of columns of q
    !! \param radius_sq square of raius
    recursive subroutine query_nearest_radius_search_subtree(this, subtree_root_node_ptr, q, q_sq, &
        nearest_dsts, nearest_idxs, n_columns, radius_sq)
        implicit none
        class(kdtree) :: this
        type(node_type), pointer       :: subtree_root_node_ptr
        real(kind=8), intent(in)       :: q(n_columns), q_sq
        integer(kind=8), allocatable, intent(inout) :: nearest_idxs(:)
        real(kind=8), allocatable, intent(inout)    :: nearest_dsts(:)
        integer(kind=8), intent(in)    :: n_columns
        real(kind=8), intent(in)    :: radius_sq

        integer(kind=8) :: i, j, idx_old, idx_new, x_idx, fid, n, n_tmp
        real(kind=8) :: dist_old, dist_new, dist, dist_from_q_to_plane, val_x, val_q, dist_from_q_to_x
        real(kind=8) :: max_radius
        real(kind=8), allocatable :: XxQ(:), tmp_d(:)
        integer(kind=8), allocatable :: tmp_i(:), indices(:)

        if (subtree_root_node_ptr%is_leaf) then
            allocate(XxQ(subtree_root_node_ptr%n_samples_))
            allocate(indices(subtree_root_node_ptr%n_samples_))
            call multi_mat_vec(subtree_root_node_ptr%x_, q, XxQ, &
                subtree_root_node_ptr%n_samples_, n_columns, f_)
            XxQ(:) = q_sq + subtree_root_node_ptr%x_sq_(:) - 2d0*XxQ(:)
            if ( radius_sq < minval(XxQ) ) then
                !  skip
            else
                do i=1, subtree_root_node_ptr%n_samples_
                    indices(i) = subtree_root_node_ptr%indices_(i)
                end do
                n = count(XxQ <= radius_sq)
                if ( n > 0) then
                    call quick_argselect(XxQ, indices, size(XxQ)+0_8, n)
                    nearest_dsts = [nearest_dsts, XxQ(1:n)]
                    nearest_idxs = [nearest_idxs, indices(1:n)]
                end if
            end if
            return
        else
            ! Compute Distance From Q to Split Plane
            fid = subtree_root_node_ptr%split_fid
            val_x = subtree_root_node_ptr%split_val
            val_q = q(fid)
            dist_from_q_to_plane = (val_q-val_x)**2d0
            max_radius = minval(nearest_dsts)

            if (max_radius < dist_from_q_to_plane) then
                if (val_q <= val_x) then
                    call this%query_nearest_radius_search_subtree(subtree_root_node_ptr%node_l_ptr, &
                        q, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
                else
                    call this%query_nearest_radius_search_subtree(subtree_root_node_ptr%node_r_ptr, &
                        q, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
                end if
            else

                call this%query_nearest_radius_search_subtree(subtree_root_node_ptr%node_l_ptr, &
                    q, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
                call this%query_nearest_radius_search_subtree(subtree_root_node_ptr%node_r_ptr, &
                    q, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
            end if
        end if
    end subroutine query_nearest_radius_search_subtree


    !> Get N nearest neighbors
    !! \param q_ptr pointer to query points
    !! \param n_neighbors number of neighbors to return
    function query_nearest_n_neighbors(this, q_ptr, n_neighbors)
        implicit none
        type(kdtree_results)     :: query_nearest_n_neighbors
        class(kdtree)            :: this
        real(kind=8), pointer, intent(in) :: q_ptr(:,:)
        integer(kind=8), intent(in) :: n_neighbors

        integer(kind=8)       :: n_samples, n_columns, q_shape(2)

        real(kind=8)                 :: val, max_radius, q_sq
        integer(kind=8)              :: i, nearest_idx, fid, q_idx
        integer(kind=8), allocatable :: indices(:), nearest_idxs(:)
        real(kind=8), allocatable :: nearest_dsts(:), q_vals(:), q_sq_sum_row(:)
        type(node_type), target :: root_node
        type(node_type), pointer :: root_node_ptr, node_ptr, leaf_node_ptr

        integer(kind=8) :: date_value1(8), date_value2(8)
        integer(kind=8), save :: time_leaf, t_get_leaf, time_internal, t_search_best

        ! print*, 1
        q_shape = shape(q_ptr)
        n_samples = q_shape(1)
        n_columns = q_shape(2)
        call ifdealloc(this%q_sq_sum_row)
        allocate(this%q_sq_sum_row(n_samples))
        call matrix_sqsum_row(q_ptr, this%q_sq_sum_row, n_samples, n_columns, parallel=f_)

        ! print*, 2
        if ( allocated(query_nearest_n_neighbors%indices) ) &
            deallocate(query_nearest_n_neighbors%indices)
        if ( allocated(query_nearest_n_neighbors%distances) ) &
            deallocate(query_nearest_n_neighbors%distances)
        allocate(query_nearest_n_neighbors%indices(n_samples))
        allocate(query_nearest_n_neighbors%distances(n_samples))
        do i=1, n_samples, 1
            allocate(query_nearest_n_neighbors%indices(i)%idx(n_neighbors))
            allocate(query_nearest_n_neighbors%distances(i)%dst(n_neighbors))
        end do
        allocate(q_vals(n_columns))
        allocate(nearest_dsts(n_neighbors))
        allocate(nearest_idxs(n_neighbors))

        !$omp parallel num_threads(6)
        !$omp do private(leaf_node_ptr, nearest_dsts, nearest_idxs, i, node_ptr, fid, val, q_vals, q_idx, q_sq, max_radius) &
        !$omp reduction(+:t_get_leaf) reduction(+:t_search_best) reduction(+:time_leaf) reduction(+:time_internal)
        do i=1, n_samples, 1
            q_idx = i
            q_vals = q_ptr(q_idx,:)
            q_sq = this%q_sq_sum_row(q_idx)
            nearest_dsts = huge(0d0)
            nearest_idxs = -2
            call this%query_nearest_n_neighbors_rec_get_leaf_ptr(this%root_node_ptr_, q_vals, q_sq, & 
                leaf_node_ptr, nearest_dsts, nearest_idxs, n_columns, n_neighbors)

            node_ptr => leaf_node_ptr
            do while (t_)
                fid = node_ptr%node_p_ptr%split_fid
                val = node_ptr%node_p_ptr%split_val
                max_radius = minval(nearest_dsts)

                if ( (q_vals(fid)-val)**2d0 < max_radius ) then
                    if (node_ptr%is_left) then
                        call this%query_nearest_n_neighbors_search_subtree(node_ptr%node_p_ptr%node_r_ptr, &
                            q_vals, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
                    else
                        call this%query_nearest_n_neighbors_search_subtree(node_ptr%node_p_ptr%node_l_ptr, &
                            q_vals, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
                    end if
                end if
                node_ptr => node_ptr%node_p_ptr
                if (.not. ASSOCIATED(node_ptr%node_p_ptr)) exit
            end do
            call quick_argsort(nearest_dsts, nearest_idxs, size(nearest_idxs)+0_8)
            query_nearest_n_neighbors%indices(i)%idx(1:n_neighbors)   = nearest_idxs
            query_nearest_n_neighbors%distances(i)%dst(1:n_neighbors) = sqrt(nearest_dsts)
        end do
        !$omp end do
        !$omp end parallel 
    end function query_nearest_n_neighbors


    !> Get Leaf Node pointer of Query point
    !! \param root_node_ptr pointer to root node
    !! \param q a coordinate of query point
    !! \param q_sq a square norm of query point
    !! \param leaf_ptr pointer to leaf node of 'q' to return
    !! \param nearest_dsts nearest distances in radius
    !! \param nearest_idxs nearest point indices in radius
    !! \param n_columns number of columns of q
    !! \param n_neighbors number of neighbors to return
    recursive subroutine query_nearest_n_neighbors_rec_get_leaf_ptr(this, root_node_ptr, q, q_sq, leaf_ptr, &
        nearest_dsts, nearest_idxs, n_columns, n_neighbors)
        implicit none
        class(kdtree)            :: this
        type(node_type), pointer :: root_node_ptr
        real(kind=8), intent(in) :: q(n_columns)
        real(kind=8), intent(in) :: q_sq
        type(node_type), pointer :: leaf_ptr
        real(kind=8), intent(inout) :: nearest_dsts(n_neighbors)
        integer(kind=8), intent(inout) :: nearest_idxs(n_neighbors)
        integer(kind=8), intent(in)    :: n_columns, n_neighbors

        real(kind=8) :: f_val, dist, dist_new, dist_old, idx_new, idx_old, dist_q, dist_from_q_to_x
        integer(kind=8) :: i, j, idx, x_idx, f_idx, n_samples_l, n_samples_r, idx_l, idx_r
        integer(kind=8), allocatable :: indices_l(:), indices_r(:), indices(:), tmp_i(:)
        logical(kind=4), allocatable :: tmp_l(:)
        logical(kind=4) :: goto_left
        real(kind=8), allocatable :: XxQ(:), tmp_d(:)
        integer(kind=8) :: n

        if (root_node_ptr%is_leaf) then
            ! Search Closest Point from Leaf Node
            allocate(XxQ(root_node_ptr%n_samples_))
            allocate(indices(root_node_ptr%n_samples_))
            leaf_ptr => root_node_ptr
            call multi_mat_vec(root_node_ptr%x_, q, XxQ, &
                root_node_ptr%n_samples_, n_columns, f_)
            XxQ(:) = q_sq + root_node_ptr%x_sq_(:) - 2d0*XxQ(:)
            if ( maxval(nearest_dsts) < minval(XxQ) ) then
                !  skip
            else
                do i=1, root_node_ptr%n_samples_
                    indices(i) = i
                end do
                tmp_d = [XxQ, nearest_dsts]
                tmp_i = [indices, nearest_idxs]
                n = minval((/size(tmp_d)+0_8, n_neighbors/))
                call quick_argselect(tmp_d, tmp_i, size(tmp_d)+0_8, n)
                nearest_dsts(1:n) = tmp_d(1:n)
                nearest_idxs(1:n) = tmp_i(1:n)
            end if
            return
        end if

        ! x_idx = root_node_ptr%data_index
        ! dist_from_q_to_x = q_sq + this%x_sq_sum_row(x_idx) - 2d0*dot_product( q(:),this%x(x_idx,:) )
        ! if (nearest_dist > dist_from_q_to_x) then
        !     nearest_dist = dist_from_q_to_x 
        !     nearest_idx = x_idx
        ! end if

        f_idx = root_node_ptr%split_fid
        f_val = root_node_ptr%split_val

        goto_left = q(f_idx) <= f_val

        if (goto_left) then
            call query_nearest_n_neighbors_rec_get_leaf_ptr(this, root_node_ptr%node_l_ptr, q, q_sq, leaf_ptr, &
                nearest_dsts, nearest_idxs, n_columns, n_neighbors)
        else
            call query_nearest_n_neighbors_rec_get_leaf_ptr(this, root_node_ptr%node_r_ptr, q, q_sq, leaf_ptr, &
                nearest_dsts, nearest_idxs, n_columns, n_neighbors)
        end if
    end subroutine query_nearest_n_neighbors_rec_get_leaf_ptr


    !> Get nearest data point of Query point from subtree
    !! \param subtree_root_node_ptr pointer to root node of subtree to search
    !! \param q a coordinate of query point
    !! \param q_sq a square norm of query point
    !! \param nearest_dsts nearest distances in radius
    !! \param nearest_idxs nearest point indices in radius
    !! \param n_columns number of columns of q
    !! \param n_neighbors number of neighbors to return
    recursive subroutine query_nearest_n_neighbors_search_subtree(this, subtree_root_node_ptr, q, q_sq, &
        nearest_dsts, nearest_idxs, n_columns, n_neighbors)
        implicit none
        class(kdtree) :: this
        type(node_type), pointer       :: subtree_root_node_ptr
        real(kind=8), intent(in)       :: q(n_columns), q_sq
        integer(kind=8), intent(inout) :: nearest_idxs(n_neighbors)
        real(kind=8), intent(inout)    :: nearest_dsts(n_neighbors)
        integer(kind=8), intent(in)    :: n_columns, n_neighbors

        integer(kind=8) :: i, j, idx_old, idx_new, x_idx, fid, n, n_tmp
        real(kind=8) :: dist_old, dist_new, dist, dist_from_q_to_plane, val_x, val_q, dist_from_q_to_x
        real(kind=8) :: max_radius
        real(kind=8), allocatable :: XxQ(:), tmp_d(:)
        integer(kind=8), allocatable :: tmp_i(:), indices(:)

        if (subtree_root_node_ptr%is_leaf) then
            ! Search Closest Point from Leaf Node
            allocate(XxQ(subtree_root_node_ptr%n_samples_))
            allocate(indices(subtree_root_node_ptr%n_samples_))
            call multi_mat_vec(subtree_root_node_ptr%x_, q, XxQ, &
                subtree_root_node_ptr%n_samples_, n_columns, f_)
            XxQ(:) = q_sq + subtree_root_node_ptr%x_sq_(:) - 2d0*XxQ(:)
            if ( maxval(nearest_dsts) < minval(XxQ) ) then
                ! skip
            else
                do i=1, subtree_root_node_ptr%n_samples_
                    indices(i) = i
                end do
                tmp_d = [XxQ, nearest_dsts]
                tmp_i = [indices, nearest_idxs]
                n = minval((/size(tmp_d)+0_8, n_neighbors/))
                call quick_argselect(tmp_d, tmp_i, size(tmp_d)+0_8, n)
                nearest_dsts(1:n) = tmp_d(1:n)
                nearest_idxs(1:n) = tmp_i(1:n)
            end if
            return
        else
            ! Compute Distance From Q to Split Plane
            fid = subtree_root_node_ptr%split_fid
            val_x = subtree_root_node_ptr%split_val
            val_q = q(fid)
            dist_from_q_to_plane = (val_q-val_x)**2d0
            max_radius = minval(nearest_dsts)

            if (max_radius < dist_from_q_to_plane) then
                if (val_q <= val_x) then
                    call this%query_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_l_ptr, &
                        q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
                else
                    call this%query_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_r_ptr, &
                        q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
                end if
            else

                call this%query_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_l_ptr, &
                    q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
                call this%query_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_r_ptr, &
                    q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
            end if
        end if
    end subroutine query_nearest_n_neighbors_search_subtree


end module mod_nearest_neighbour
