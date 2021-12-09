module mod_nearest_neighbour
    !$ use omp_lib
    use mod_const
    use mod_common
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

        type(node_type), pointer :: node_p_ptr => null()
        type(node_type), pointer :: node_l_ptr => null()
        type(node_type), pointer :: node_r_ptr => null()
    contains
        procedure :: info
        procedure :: init
        procedure :: init_ver02
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
        real(kind=8), ALLOCATABLE :: x_sq_sum_row(:)
    contains
        procedure :: build
        procedure :: build_rec

        procedure :: build_ver02
        procedure :: build_ver02_rec

        procedure :: query_nearest
        procedure :: query_nearest_rec_get_leaf_ptr
        procedure :: query_nearest_rec_get_leaf_ptrs ! return all samples's leaf node pointer
        procedure :: query_nearest_get_nearest_in_same_leaf_node
        procedure :: query_nearest_search_subtree
        ! procedure :: query_rec
    end type kdtree

    interface kdtree
        module procedure :: new_kdtree
    end interface kdtree

contains


    function new_kdtree(min_samples_in_leaf)
        implicit none
        type(kdtree) :: new_kdtree
        integer(kind=8), optional :: min_samples_in_leaf

        if ( present(min_samples_in_leaf) ) new_kdtree%min_samples_in_leaf = min_samples_in_leaf
    end function new_kdtree

    subroutine init(this, is_root, is_leaf, is_left, depth, n_samples, n_columns, min_samples_in_leaf, parent_node_ptr)
        implicit none
        class(node_type), intent(inout) :: this
        logical(kind=4), intent(in) :: is_root, is_leaf, is_left
        integer(kind=8), intent(in) :: depth, n_samples, n_columns, min_samples_in_leaf
        type(node_type), pointer, intent(in), optional :: parent_node_ptr

        this%is_root = is_root
        this%is_leaf = is_leaf
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

        if (present(parent_node_ptr)) then
            allocate(this%node_p_ptr)
            this%node_p_ptr => parent_node_ptr
        end if
    end subroutine init

    subroutine init_ver02(this, is_root, is_leaf, is_left, depth, n_samples, n_columns, min_samples_in_leaf, &
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
    end subroutine init_ver02

    subroutine adopting_twins_in_kdtree(this, median_index)
        implicit none
        class(node_type), intent(inout) :: this
        integer(kind=8), intent(in) :: median_index

        integer(kind=8) :: n_samples_l, n_samples_r, i

        n_samples_l = median_index-1
        n_samples_r = this%n_samples-median_index
        this%data_index = this%indices(median_index)

        allocate(this%node_l_ptr)
        call this%node_l_ptr%init_ver02(f_, f_, t_, this%depth+1, &
            n_samples_l, this%n_columns, this%min_samples_in_leaf, this)

        allocate(this%node_r_ptr)
        call this%node_r_ptr%init_ver02(f_, f_, f_, this%depth+1, &
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

        ! call root_node%init(t_, f_, f_, 0_8, n_samples, n_columns, this%min_samples_in_leaf)
        ! root_node%idx = 0
        ! allocate(root_node%indices(root_node%n_samples))
        ! do i=1, root_node%n_samples, 1
        !     root_node%indices(i) = i
        ! end do

        ! node_idx = 1_8
        ! root_node_ptr => root_node
        call this%build_rec(root_node_ptr, x_ptr, node_idx)

        this%root_node_ = root_node
        this%n_samples = n_samples
        this%n_columns = n_columns
        allocate(this%x(this%n_samples, this%n_columns))
        this%x(:,:) = x(:,:)
        ! allocate(this%x_sq_sum_row(this%n_samples))
        ! call matrix_sqsum_row(x, this%x_sq_sum_row, n_samples, n_columns, parallel=f_)
    end subroutine build

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

        n_samples_l = med_idx-1
        n_samples_r = node%n_samples-med_idx
        node%data_index = node%indices(med_idx)

        allocate(node%node_l_ptr)
        call node%node_l_ptr%init(f_, f_, t_, node%depth+1, &
            n_samples_l, node%n_columns, node%min_samples_in_leaf, node)

        allocate(node%node_r_ptr)
        call node%node_r_ptr%init(f_, f_, f_, node%depth+1, &
            n_samples_r, node%n_columns, node%min_samples_in_leaf, node)

        allocate(node%node_l_ptr%indices(n_samples_l))
        do i=1, med_idx-1, 1
            node%node_l_ptr%indices(i) = node%indices(i)
        end do

        allocate(node%node_r_ptr%indices(n_samples_r))
        do i=med_idx+1, node%n_samples, 1
            node%node_r_ptr%indices(i-med_idx) = node%indices(i)
        end do

        node_idx = node_idx + 1
        call this%build_rec(node%node_l_ptr, x_ptr, node_idx)

        node_idx = node_idx + 1
        call this%build_rec(node%node_r_ptr, x_ptr, node_idx)
    end subroutine build_rec

    subroutine build_ver02(this, x)
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

        allocate(this%root_node_ptr_)
        call this%root_node_ptr_%init(t_, f_, f_, 0_8, n_samples, n_columns, this%min_samples_in_leaf)
        this%root_node_ptr_%idx = 0
        allocate(this%root_node_ptr_%indices(this%root_node_ptr_%n_samples))
        do i=1, this%root_node_ptr_%n_samples, 1
            this%root_node_ptr_%indices(i) = i
        end do

        node_idx = 1_8
        call this%build_ver02_rec(this%root_node_ptr_, x_ptr, node_idx)

        this%root_node_ = root_node
        this%n_samples = n_samples
        this%n_columns = n_columns
        call ifdealloc(this%x)
        allocate(this%x(this%n_samples, this%n_columns))
        this%x(:,:) = x(:,:)

        call ifdealloc(this%x_sq_sum_row)
        allocate(this%x_sq_sum_row(this%n_samples))
        call matrix_sqsum_row(x, this%x_sq_sum_row, n_samples, n_columns, parallel=f_)
    end subroutine build_ver02

    recursive subroutine build_ver02_rec(this, node, x_ptr, node_idx)
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
        call this%build_ver02_rec(node%node_l_ptr, x_ptr, node_idx)

        node_idx = node_idx + 1
        call this%build_ver02_rec(node%node_r_ptr, x_ptr, node_idx)
    end subroutine build_ver02_rec

    function query_nearest(this, q)
        implicit none
        integer(kind=8), allocatable     :: query_nearest(:,:)
        class(kdtree)                    :: this
        real(kind=8), target, intent(in) :: q(:,:)

        integer(kind=8)       :: n_samples, n_columns, q_shape(2)
        real(kind=8), pointer :: q_ptr(:,:)

        real(kind=8)                 :: val, nearest_dist
        integer(kind=8)              :: i, nearest_idx, fid
        integer(kind=8), allocatable :: indices(:), nearest_idxs(:)
        real(kind=8), allocatable :: nearest_dists(:)
        type(node_type_ptr), ALLOCATABLE :: leaf_ptrs(:)
        type(node_type), target :: root_node
        type(node_type), pointer :: root_node_ptr, node_ptr, leaf_node_ptr

        ! print*, 1
        q_shape = shape(q)
        n_samples = q_shape(1)
        n_columns = q_shape(2)
        q_ptr => q

        ! print*, 2
        call ifdealloc(query_nearest)
        allocate(query_nearest(n_samples, 1_8))
        query_nearest(:,:) = -1_8

        ! print*, 3
        allocate(leaf_ptrs(n_samples))
        allocate(indices(n_samples))
        allocate(nearest_dists(n_samples))
        allocate(nearest_idxs(n_samples))
        do i=1, n_samples, 1
            indices(i) = i
            nearest_dists(i) = huge(0d0)
            nearest_idxs(i) = -2
            nullify(leaf_ptrs(i)%ptr)
        end do

        ! print*, 4
        root_node = this%root_node_
        root_node_ptr => root_node

        ! print*, 5

        !$omp parallel num_threads(4)
        !$omp do private(i, node_ptr, leaf_node_ptr, nearest_dist, nearest_idx, fid, val)
        do i=1, n_samples, 1
            call this%query_nearest_rec_get_leaf_ptr(this%root_node_ptr_, q_ptr(i,:), & 
                leaf_node_ptr, nearest_dist, nearest_idx, n_columns)

            call this%query_nearest_get_nearest_in_same_leaf_node(i, nearest_idx, nearest_dist, &
                q_ptr, leaf_node_ptr, n_columns)

            node_ptr => leaf_node_ptr

            do while (t_)
                ! call node_ptr%node_p_ptr%info()
                if (.not. ASSOCIATED(node_ptr%node_p_ptr)) exit

                fid = node_ptr%node_p_ptr%split_fid
                val = node_ptr%node_p_ptr%split_val

                ! print*, fid, val, nearest_idx, nearest_dist
                if ( (q_ptr(i,fid)-val)**2d0 < nearest_dist ) then
                    if (node_ptr%is_left) then
                        call this%query_nearest_search_subtree(node_ptr%node_p_ptr%node_r_ptr, q_ptr(i,:), & 
                            nearest_idx, nearest_dist, n_columns)
                    else
                        call this%query_nearest_search_subtree(node_ptr%node_p_ptr%node_l_ptr, q_ptr(i,:), & 
                            nearest_idx, nearest_dist, n_columns)
                    end if
                end if
                node_ptr => node_ptr%node_p_ptr
            end do
            query_nearest(i,1) = nearest_idx
        end do
        !$omp end do
        !$omp end parallel

    end function query_nearest

    recursive subroutine query_nearest_rec_get_leaf_ptrs(this, root_node_ptr, q_ptr, leaf_ptrs, indices, &
        nearest_dists, nearest_idxs, n_samples)
        implicit none
        class(kdtree)            :: this
        type(node_type), pointer :: root_node_ptr
        real(kind=8), pointer    :: q_ptr(:,:)
        type(node_type_ptr)      :: leaf_ptrs(:)
        real(kind=8), intent(inout) :: nearest_dists(:)
        integer(kind=8), intent(inout) :: nearest_idxs(:)
        integer(kind=8)          :: indices(n_samples), n_samples

        real(kind=8) :: f_val, dist, dist_new, dist_old, idx_new, idx_old
        integer(kind=8) :: i, j, idx, q_idx, x_idx, f_idx, n_samples_l, n_samples_r, idx_l, idx_r
        integer(kind=8), allocatable :: indices_l(:), indices_r(:)
        logical(kind=4), allocatable :: tmp_l(:)

        if (root_node_ptr%is_leaf) then
            ! Search Closest Point from Leaf Node
            do i=1, n_samples, 1
                q_idx = indices(i)
                dist_old = nearest_dists(q_idx)
                idx_old = nearest_idxs(q_idx)
                leaf_ptrs(q_idx)%ptr => root_node_ptr
                do j=1, root_node_ptr%n_samples, 1
                    x_idx = root_node_ptr%indices(j)
                    dist_new = sum( (q_ptr(q_idx,:)-this%x(x_idx,:))**2d0 )
                    if ( dist_new < dist_old ) then
                        idx_old = x_idx
                        dist_old = dist_new
                    end if
                end do
                nearest_dists(q_idx) = dist_old
                nearest_idxs(q_idx) = idx_old
            end do
            return
        else
            ! Compute Distance From Split Point
            x_idx = root_node_ptr%idx
            do q_idx=1, n_samples, 1
                dist = sum( (q_ptr(q_idx,:)-this%x(x_idx,:))**2d0 )
                if (nearest_dists(q_idx) > dist) then
                    nearest_dists(q_idx) = dist
                    nearest_idxs(q_idx)  = x_idx
                end if
            end do
        end if

        allocate(tmp_l(n_samples))
        f_idx = root_node_ptr%split_fid
        f_val = root_node_ptr%split_val
        do i=1, n_samples, 1
            idx = indices(i)
            tmp_l(i) = q_ptr(idx, f_idx) <= f_val
        end do

        n_samples_l = count(tmp_l)
        n_samples_r = n_samples - n_samples_l
        allocate(indices_l(n_samples_l))
        allocate(indices_r(n_samples_r))

        idx_l = 1
        idx_r = 1
        do i=1, n_samples, 1
            idx = indices(i)
            if (tmp_l(i)) then
                indices_l(idx_l) = idx
                idx_l = idx_l + 1
            else
                indices_r(idx_r) = idx
                idx_r = idx_r + 1
            end if
        end do

        call query_nearest_rec_get_leaf_ptrs(this, root_node_ptr%node_l_ptr, q_ptr, leaf_ptrs, &
            indices_l, nearest_dists, nearest_idxs, n_samples_l)
        call query_nearest_rec_get_leaf_ptrs(this, root_node_ptr%node_r_ptr, q_ptr, leaf_ptrs, &
            indices_r, nearest_dists, nearest_idxs, n_samples_r)
    end subroutine query_nearest_rec_get_leaf_ptrs

    recursive subroutine query_nearest_rec_get_leaf_ptr(this, root_node_ptr, q, leaf_ptr, &
        nearest_dist, nearest_idx, n_columns)
        implicit none
        class(kdtree)            :: this
        type(node_type), pointer :: root_node_ptr
        real(kind=8), intent(in) :: q(n_columns)
        type(node_type), pointer :: leaf_ptr
        real(kind=8), intent(inout) :: nearest_dist
        integer(kind=8), intent(inout) :: nearest_idx
        integer(kind=8)          :: n_columns

        real(kind=8) :: f_val, dist, dist_new, dist_old, idx_new, idx_old
        integer(kind=8) :: i, j, idx, q_idx, x_idx, f_idx, n_samples_l, n_samples_r, idx_l, idx_r
        integer(kind=8), allocatable :: indices_l(:), indices_r(:)
        logical(kind=4), allocatable :: tmp_l(:)
        logical(kind=4) :: goto_left

        if (root_node_ptr%is_leaf) then
            ! Search Closest Point from Leaf Node
            dist_old = nearest_dist
            idx_old  = nearest_idx
            leaf_ptr => root_node_ptr
            do j=1, leaf_ptr%n_samples, 1
                x_idx = leaf_ptr%indices(j)
                dist_new = sum( (q(:)-this%x(x_idx,:))**2d0 )
                if ( dist_new < dist_old ) then
                    idx_old = x_idx
                    dist_old = dist_new
                end if
            end do
            nearest_dist = dist_old
            nearest_idx = idx_old
            return
        else
            ! Compute Distance From Split Point
            x_idx = root_node_ptr%idx
            dist = sum( (q(:)-this%x(x_idx,:))**2d0 )
            if (nearest_dist > dist) then
                nearest_dist = dist
                nearest_idx  = x_idx
            end if
        end if

        f_idx = root_node_ptr%split_fid
        f_val = root_node_ptr%split_val

        goto_left = q(f_idx) <= f_val

        if (goto_left) then
            call query_nearest_rec_get_leaf_ptr(this, root_node_ptr%node_l_ptr, q, leaf_ptr, &
                nearest_dist, nearest_idx, n_columns)
        else
            call query_nearest_rec_get_leaf_ptr(this, root_node_ptr%node_r_ptr, q, leaf_ptr, &
                nearest_dist, nearest_idx, n_columns)
        end if
    end subroutine query_nearest_rec_get_leaf_ptr

    subroutine query_nearest_get_nearest_in_same_leaf_node(this, sample_idx, nearest_idx, nearest_dist, q_ptr, leaf_ptr, n_columns)
        implicit none
        class(kdtree)                   :: this
        integer(kind=8), intent(in)     :: sample_idx
        integer(kind=8), intent(inout)  :: nearest_idx
        real(kind=8), intent(inout)     :: nearest_dist
        real(kind=8), intent(in)        :: q_ptr(:,:)
        type(node_type), pointer, intent(in) :: leaf_ptr
        integer(kind=8), intent(in)     :: n_columns
        type(node_type), pointer        :: node_ptr

        integer(kind=8) :: i, idx
        real(kind=8) :: q_sq_sum
        real(kind=8), allocatable :: dists(:)

        q_sq_sum = sum(q_ptr(sample_idx,:)*q_ptr(sample_idx,:))
        allocate(dists(leaf_ptr%n_samples))
        dists(:) = q_sq_sum
        do i=1, leaf_ptr%n_samples, 1
            idx = leaf_ptr%indices(i)
            dists(i) = sum( (this%x(idx,:)-q_ptr(sample_idx,:))**2d0 )
        end do
        nearest_dist = minval(dists)
        nearest_idx = leaf_ptr%indices(minloc(dists, dim=1))
    end subroutine query_nearest_get_nearest_in_same_leaf_node

    recursive subroutine query_nearest_search_subtree(this, subtree_root_node_ptr, q, nearest_idx, nearest_dst, n_columns)
        implicit none
        class(kdtree) :: this
        type(node_type), pointer       :: subtree_root_node_ptr
        real(kind=8), intent(in)       :: q(n_columns)
        integer(kind=8), intent(inout) :: nearest_idx
        real(kind=8), intent(inout)    :: nearest_dst
        integer(kind=8), intent(in)    :: n_columns

        integer(kind=8) :: i, j, idx_old, idx_new, x_idx
        real(kind=8) :: dist_old, dist_new, dist

        if (subtree_root_node_ptr%is_leaf) then
            ! Search Closest Point from Leaf Node
            dist_old = nearest_dst
            idx_old = nearest_idx
            do j=1, subtree_root_node_ptr%n_samples, 1
                x_idx = subtree_root_node_ptr%indices(j)
                dist_new = sum( (q(:)-this%x(x_idx,:))**2d0 )
                if ( dist_new < dist_old ) then
                    idx_old = x_idx
                    dist_old = dist_new
                end if
            end do
            nearest_dst = dist_old
            nearest_idx = idx_old
            return
        else
            ! Compute Distance From Split Point
            x_idx = subtree_root_node_ptr%idx
            dist_new = sum( (q(:)-this%x(x_idx,:))**2d0 )
            if (nearest_dst > dist_new) then
                nearest_dst = dist_new
                nearest_idx = x_idx
            end if
        end if
        call this%query_nearest_search_subtree(subtree_root_node_ptr%node_l_ptr, q, nearest_idx, nearest_dst, n_columns)
        call this%query_nearest_search_subtree(subtree_root_node_ptr%node_r_ptr, q, nearest_idx, nearest_dst, n_columns)
    end subroutine query_nearest_search_subtree

    recursive subroutine bottom_to_top(node_ptr)
        implicit none
        type(node_type), pointer :: node_ptr
        call node_ptr%info()
        if (ASSOCIATED(node_ptr%node_p_ptr)) then
            call bottom_to_top(node_ptr%node_p_ptr)
        end if
    end subroutine bottom_to_top

end module mod_nearest_neighbour
