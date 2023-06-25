module mod_kdtree
    !$ use omp_lib
    use mod_const
    use mod_common
    use mod_common_type
    use mod_error
    use mod_timer
    use mod_sort
    use mod_linalg
    use mod_math, only: relu
    use mod_brute_force_search
    use mod_data_holder    
    use mod_nearest_neighbour, only: neighbor_results, base_node_for_nearest_neighbor
    implicit none
    
    !> KDtree node.
    type, extends(base_node_for_nearest_neighbor) :: node_type
        real(kind=8)    :: split_val = huge(0d0) !< split threshold
        integer(kind=8) :: split_fid = -1_8 !< split feature index
        integer(kind=8) :: split_val_i8 = huge(0_8) !< split threshold, for 'exact_dup_search'

        integer(kind=8)              :: data_index !< split data point index
        real(kind=8), allocatable    :: x(:,:),    x_sq(:) !< explanatory and its sum squared sum by row
        real(kind=8), allocatable    :: x_(:,:),   x_sq_(:) !< explanatory and its sum squared sum by row with split points
        real(kind=8), allocatable    :: xt_(:,:) !< explanatory and its sum squared sum by row with split points, TRANSPOSED
        integer(kind=8), allocatable :: x_i8(:,:), x_sq_i8(:) !< explanatory and its sum squared sum by row with split points, for 'exact_dup_search'

        type(node_type), pointer :: node_p_ptr => null() !< pointer to parent node
        type(node_type), pointer :: node_l_ptr => null() !< pointer to child left node
        type(node_type), pointer :: node_r_ptr => null() !< pointer to child right node

        real(kind=8), allocatable :: f_min(:), f_max(:) !< feature minimum and maximum range
        real(kind=8), allocatable :: center(:) !< feature minimum and maximum range

        real(kind=8) :: radius
    contains
        procedure :: info
        procedure :: init
        procedure :: adopting_twins_in_kdtree
        procedure :: get_feature_range

        procedure :: comp_dist_rect_to_query

        procedure :: comp_feature_vars

        procedure :: comp_distance_from_query_to_all_samples
    end type node_type


    type node_type_ptr
        type(node_type), pointer :: ptr
    end type node_type_ptr
    
    
    !> Type of kdtree
    type kdtree
        type(node_type), pointer :: root_node_ptr_ !< pointer ot root node
        integer(kind=8) :: min_samples_in_leaf = 128_8 !< minimum number of samples in leaf node

        integer(kind=8) :: n_samples !< number of samples
        integer(kind=8) :: n_columns !< number of columns
        real(kind=8), ALLOCATABLE :: x_sq_sum(:) !< sum squared value by row of explanatory data
        real(kind=8), ALLOCATABLE :: q_sq_sum(:) !< sum squared value by row of query data

        logical(kind=4), allocatable :: is_useless_feature(:)

        type(node_type_ptr), allocatable :: leaves(:)

        logical(kind=4) :: use_brute_force=f_
        type(brute_force_search) :: bfs

        real(kind=8), allocatable :: rec_diag_length(:) ! diagonal length of hyper rectangle
        real(kind=8), allocatable :: rec_center(:,:) ! center position of hyper rectangle
        real(kind=8), allocatable :: rec_center_sq(:) ! center position of hyper rectangle
    contains
        procedure, pass :: build_x
        procedure, pass :: build_dholder
        generic :: build => build_x, build_dholder
        procedure :: build_rec

        procedure, pass :: build_new_x
        procedure, pass :: build_new_dholder
        generic :: build_new => build_new_x, build_new_dholder
        procedure :: build_new_rec

        procedure :: query
        procedure :: query_nearest_n_neighbors
        procedure :: query_nearest_n_neighbors_rec_get_leaf_ptr
        procedure :: query_nearest_n_neighbors_search_subtree
        procedure :: query_nearest_radius
        procedure :: query_nearest_radius_rec_get_leaf_ptr
        procedure :: query_nearest_radius_search_subtree

        ! procedure :: query_new
        ! procedure :: query_new_nearest_n_neighbors
        ! procedure :: query_new_nearest_n_neighbors_rec_get_leaf_ptr
        ! procedure :: query_new_nearest_n_neighbors_search_subtree
        ! procedure :: query_new_nearest_radius
        ! procedure :: query_new_nearest_radius_rec_get_leaf_ptr
        ! procedure :: query_new_nearest_radius_search_subtree

        ! procedure :: query_new_T
        ! procedure :: query_new_T_nearest_n_neighbors
        ! procedure :: query_new_T_nearest_n_neighbors_rec_get_leaf_ptr
        ! procedure :: query_new_T_nearest_n_neighbors_search_subtree
        ! procedure :: query_new_T_nearest_radius
        ! procedure :: query_new_T_nearest_radius_rec_get_leaf_ptr
        ! procedure :: query_new_T_nearest_radius_search_subtree

        ! procedure :: query_red
        ! procedure :: query_red_nearest_n_neighbors
        ! procedure :: query_red_nearest_n_neighbors_rec_get_leaf_ptr
        ! procedure :: query_red_nearest_n_neighbors_search_subtree

        ! procedure :: query_rec
        ! procedure :: query_rec_nearest_n_neighbors

        procedure :: dump => dump_kdtree
        procedure :: dump_node
        procedure :: load => load_kdtree
        procedure :: load_node

    end type kdtree

    !> Construct New 'kdtree' object.
    interface kdtree
        module procedure :: new_kdtree
    end interface kdtree



contains

    function check_duplication(vector, n_samples)
        implicit none
        logical(kind=4) :: check_duplication
        integer(kind=8), intent(in) :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples
        integer(kind=8), allocatable :: vector_copy(:)
        integer(kind=8) :: i

        check_duplication = f_
        allocate(vector_copy(n_samples))
        vector_copy(:) = vector(:)
        call quick_sort(vector_copy, n_samples)
        do i=1, n_samples-1, 1
            if (vector_copy(i) == vector_copy(i+1)) then
                check_duplication = t_
                return
            end if
        end do
    end function check_duplication

    subroutine comp_distance_from_query_to_all_samples(this, q, n_columns, dists, n_samples)
        implicit none
        class(node_type) :: this
        real(kind=8), intent(in) :: q(n_columns)
        integer(kind=8), intent(in) :: n_columns
        real(kind=8), intent(inout) :: dists(n_samples)
        integer(kind=8), intent(in) :: n_samples

        real(kind=8) :: q_sq

        q_sq = sum(q**2d0)
        dists = q_sq + this%x_sq_(:)
        call openblas_set_num_threads(1)
        call dgemv("N", this%n_samples_, n_columns, &
            -2d0, this%x_, this%n_samples_, &
            q, 1_8, 1d0, dists, 1_8)
    end subroutine comp_distance_from_query_to_all_samples


    recursive subroutine extract_all_leaves(root_node_ptr, leaf_ptrs)
        implicit none
        type(node_type), pointer, intent(in) :: root_node_ptr
        type(node_type_ptr), allocatable, intent(inout) :: leaf_ptrs(:)

        type(node_type_ptr) :: leaf

        if ( associated(root_node_ptr%node_l_ptr) ) then
            call extract_all_leaves(root_node_ptr%node_l_ptr, leaf_ptrs)
            call extract_all_leaves(root_node_ptr%node_r_ptr, leaf_ptrs)
        else
            leaf%ptr => root_node_ptr
            leaf_ptrs = [leaf_ptrs, leaf]
        end if        
    end subroutine extract_all_leaves


    function comp_feature_vars(this, x_ptr) result(vars)
        implicit none
        class(node_type) :: this
        real(kind=8), pointer :: x_ptr(:,:)
        real(kind=8), allocatable :: vars(:)

        integer(kind=8) :: idx, i, f
        real(kind=8) :: sum_x, sq_sum_x, tmp_x

        allocate(vars(this%n_columns))

        do f=1, this%n_columns, 1
            sum_x = 0d0
            sq_sum_x = 0d0
            do i=1, this%n_samples, 1
                idx = this%indices(i)
                tmp_x = x_ptr(idx,f)
                sum_x = sum_x + tmp_x
                sq_sum_x = sq_sum_x + tmp_x**2d0
            end do
            vars(f) = sq_sum_x / this%n_samples - (sum_x / this%n_samples)**2d0
        end do
    end function 


    function comp_dist_rect_to_query(this, q, n_columns) result(dist)
        implicit none
        class(node_type) :: this
        real(kind=8) :: dist
        real(kind=8), intent(in) :: q(n_columns)
        integer(kind=8), intent(in) :: n_columns

        integer(kind=8) :: f
        real(kind=8) :: df

        dist = 0d0

        do f=1, n_columns, 1
            df = maxval([this%f_min(f)-q(f), 0d0, q(f)-this%f_max(f)])
            dist = dist + df**2d0
        end do
    end function 


    recursive subroutine print_n_samples(root_node_ptr)
        type(node_type), pointer, intent(in) :: root_node_ptr
        if ( associated(root_node_ptr%node_l_ptr) ) then
            call print_n_samples(root_node_ptr%node_l_ptr)
            call print_n_samples(root_node_ptr%node_r_ptr)
        else
            print*, root_node_ptr%n_samples
        end if
    end subroutine print_n_samples


    recursive subroutine get_max_depth(root_node_ptr, max_depth, is_root)
        implicit none
        type(node_type), pointer, intent(in) :: root_node_ptr
        integer(kind=8), intent(inout) :: max_depth
        logical(kind=4), intent(in) :: is_root

        if (is_root) max_depth=0_8

        if ( associated(root_node_ptr%node_l_ptr) ) then
            call get_max_depth(root_node_ptr%node_l_ptr, max_depth, is_root=f_)
            call get_max_depth(root_node_ptr%node_r_ptr, max_depth, is_root=f_)
        else
            max_depth = maxval([max_depth, root_node_ptr%depth])
            return
        end if
    end subroutine get_max_depth


    recursive subroutine count_leaves(root_node_ptr, n_leaves, is_root)
        implicit none
        type(node_type), pointer, intent(in) :: root_node_ptr
        integer(kind=8), intent(inout) :: n_leaves
        logical(kind=4), intent(in) :: is_root

        if (is_root) n_leaves=0_8

        if ( associated(root_node_ptr%node_l_ptr) ) then
            call count_leaves(root_node_ptr%node_l_ptr, n_leaves, is_root=f_)
            call count_leaves(root_node_ptr%node_r_ptr, n_leaves, is_root=f_)
        else
            n_leaves = n_leaves + 1
            return
        end if
    end subroutine count_leaves


    subroutine dump_kdtree(this, file_name)
        class(kdtree) :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8) :: newunit
        open(newunit=newunit, file=file_name, form='unformatted', status='replace')
        write(newunit) this%n_samples
        write(newunit) this%n_columns
        call this%dump_node(this%root_node_ptr_, newunit)
        write(newunit) this%min_samples_in_leaf
        write(newunit) this%x_sq_sum(:)
        close(newunit)
    end subroutine dump_kdtree


    recursive subroutine dump_node(this, node_ptr, newunit)
        implicit none
        class(kdtree)                 :: this
        type(node_type), pointer, intent(in) :: node_ptr
        integer(kind=8), intent(in)     :: newunit

        logical(kind=4) :: is_terminal
        integer(kind=8) :: x_shape(2)
        type(node_type), pointer :: node_l_ptr, node_r_ptr

        is_terminal = .not. associated(node_ptr%node_l_ptr)
        ! node Info
        write(newunit) is_terminal
        write(newunit) node_ptr%is_root
        write(newunit) node_ptr%is_leaf
        write(newunit) node_ptr%is_left
        write(newunit) node_ptr%depth
        write(newunit) node_ptr%n_samples
        write(newunit) node_ptr%n_samples_
        write(newunit) node_ptr%n_columns
        write(newunit) node_ptr%min_samples_in_leaf
        write(newunit) node_ptr%split_val
        write(newunit) node_ptr%split_fid
        write(newunit) node_ptr%indices(:)

        if (is_terminal) then
            ! Additional Info
            x_shape(:) = shape(node_ptr%x_)
            write(newunit) x_shape(:)
            write(newunit) node_ptr%x_(:,:)
            write(newunit) node_ptr%x_sq_(:)
            write(newunit) node_ptr%indices_(:)
        else
            call this%dump_node(node_ptr%node_l_ptr, newunit)
            call this%dump_node(node_ptr%node_r_ptr, newunit)
        end if

    end subroutine dump_node


    subroutine load_kdtree(this, file_name)
        class(kdtree) :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8) :: newunit
        open(newunit=newunit, file=file_name, form='unformatted')
        read(newunit) this%n_samples
        read(newunit) this%n_columns
        allocate(this%root_node_ptr_)
        call this%load_node(this%root_node_ptr_, newunit)
        read(newunit) this%min_samples_in_leaf
        read(newunit) this%x_sq_sum(:)
        close(newunit)
    end subroutine load_kdtree


    recursive subroutine load_node(this, node_ptr, newunit)
        implicit none
        class(kdtree)                 :: this
        type(node_type), pointer, intent(in) :: node_ptr
        integer(kind=8), intent(in)     :: newunit

        logical(kind=4) :: is_terminal
        integer(kind=8) :: x_shape(2)
        integer(kind=8), save :: i=0

        ! i=i+1
        ! print*, i

        is_terminal = .not. associated(node_ptr%node_l_ptr)
        ! node Info
        ! print*, "node Info"
        read(newunit) is_terminal
        read(newunit) node_ptr%is_root
        read(newunit) node_ptr%is_leaf
        read(newunit) node_ptr%is_left
        read(newunit) node_ptr%depth
        read(newunit) node_ptr%n_samples
        read(newunit) node_ptr%n_samples_
        read(newunit) node_ptr%n_columns
        read(newunit) node_ptr%min_samples_in_leaf
        read(newunit) node_ptr%split_val
        read(newunit) node_ptr%split_fid
        allocate(node_ptr%indices(node_ptr%n_samples))
        read(newunit) node_ptr%indices

        allocate(node_ptr%node_l_ptr)
        allocate(node_ptr%node_r_ptr)
        allocate(node_ptr%node_l_ptr%node_p_ptr)
        allocate(node_ptr%node_r_ptr%node_p_ptr)
        node_ptr%node_l_ptr%node_p_ptr => node_ptr
        node_ptr%node_r_ptr%node_p_ptr => node_ptr

        if (is_terminal) then
            nullify(node_ptr%node_l_ptr)
            nullify(node_ptr%node_r_ptr)
            ! Additional Info
            ! print*, "Additional Info"
            read(newunit) x_shape(:)
            allocate(node_ptr%x_(x_shape(1), x_shape(2)))
            read(newunit) node_ptr%x_(:,:)
            allocate(node_ptr%x_sq_(x_shape(1)))
            read(newunit) node_ptr%x_sq_(:)    
            allocate(node_ptr%indices_(x_shape(1)))
            read(newunit) node_ptr%indices_(:)    
        else
            call this%load_node(node_ptr%node_l_ptr, newunit)
            call this%load_node(node_ptr%node_r_ptr, newunit)
        end if

    end subroutine load_node


    subroutine get_feature_range(this, x_ptr)
        implicit none
        class(node_type) :: this
        real(kind=8), pointer, intent(in) :: x_ptr(:,:)

        integer(kind=8) :: n, idx, f
        real(kind=8), allocatable :: x_row(:)

        allocate(this%f_min(this%n_columns)); this%f_min =   huge(0d0)
        allocate(this%f_max(this%n_columns)); this%f_max = - huge(0d0)
        allocate(x_row(this%n_columns))

        do n=1, this%n_samples, 1
            idx = this%indices(n)
            x_row(:) = x_ptr(idx,:)
            do f=1, this%n_columns, 1
                this%f_min(f) = minval([this%f_min(f), x_row(f)])
                this%f_max(f) = maxval([this%f_max(f), x_row(f)])
            end do
        end do
    end subroutine get_feature_range


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
    subroutine build_x(this, x)
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
    end subroutine build_x

    
    subroutine build_dholder(this, dholder)
        implicit none
        class(kdtree) :: this
        type(data_holder), intent(in) :: dholder
        call this%build_x(dholder%x_ptr%x_r8_ptr)
    end subroutine build_dholder


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
        ! call quick_argselect(f_tmp, node_ptr%indices, node_ptr%n_samples, med_idx)
        call quick_argsort(f_tmp, node_ptr%indices, node_ptr%n_samples)
        node_ptr%split_val = f_tmp(med_idx)
        ! print*, "OLD: ", node_ptr%depth, node_ptr%split_fid, med_idx, f_tmp(med_idx)

        call node_ptr%adopting_twins_in_kdtree(med_idx)

        node_idx = node_idx + 1
        allocate(split_indices_l(size(split_indices)))
        split_indices_l = [split_indices, node_ptr%indices(med_idx)]
        call this%build_rec(node_ptr%node_l_ptr, x_ptr, node_idx, split_indices_l)

        node_idx = node_idx + 1
        allocate(split_indices_r(0))
        call this%build_rec(node_ptr%node_r_ptr, x_ptr, node_idx, split_indices_r)
    end subroutine build_rec


    !> Building kdtree, wrapping 'build_rec'
    !! \param x input explanatory variable
    subroutine build_new_x(this, x)
        implicit none
        class(kdtree) :: this
        real(kind=8), target  :: x(:,:)
        real(kind=8), pointer  :: x_ptr(:,:)

        type(node_type), TARGET  :: root_node
        type(node_type), POINTER :: root_node_ptr, hoge
        integer(kind=8) :: n_samples, n_columns, i, depth, counter, node_idx
        integer(kind=8), ALLOCATABLE :: indices(:), split_indices(:)
        type(node_type), pointer :: node_tmp

        real(kind=8), allocatable :: min_val(:), max_val(:)
        real(kind=8), allocatable :: dist(:)
        real(kind=8) :: c_sq
        integer(kind=8) :: l, f


        n_samples = size(x(:,1))
        n_columns = size(x(1,:))
        if (n_samples < this%min_samples_in_leaf) then
            this%use_brute_force = t_
            this%bfs = brute_force_search()
            call this%bfs%build(x)
        end if
        x_ptr => x
        this%n_columns = n_columns

        allocate(this%is_useless_feature(n_columns), min_val(n_columns), max_val(n_columns))
        min_val = minval(x, dim=1)
        max_val = maxval(x, dim=1)
        this%is_useless_feature = (min_val == max_val)

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
        call this%build_new_rec(this%root_node_ptr_, x_ptr, node_idx, split_indices)

        this%n_samples = n_samples
        this%n_columns = n_columns

        allocate(this%leaves(0))
        call extract_all_leaves(this%root_node_ptr_, this%leaves)
        do l=1, size(this%leaves), 1
            this%leaves(l)%ptr%center = (this%leaves(l)%ptr%f_max(:) - this%leaves(l)%ptr%f_min(:)) * 0.5d0
            c_sq = sum(this%leaves(l)%ptr%center**2d0)
            allocate(dist(this%leaves(l)%ptr%n_samples_))
            call multi_mat_vec(this%leaves(l)%ptr%x_, this%leaves(l)%ptr%center, dist, &
                this%leaves(l)%ptr%n_samples_, n_columns, f_)
            dist = c_sq + this%leaves(l)%ptr%x_sq_ - 2d0 * dist
            this%leaves(l)%ptr%radius = maxval(dist)
            deallocate(dist)
        end do
    end subroutine build_new_x

    
    subroutine build_new_dholder(this, dholder)
        implicit none
        class(kdtree) :: this
        type(data_holder), intent(in) :: dholder
        call this%build_new_x(dholder%x_ptr%x_r8_ptr)
    end subroutine build_new_dholder


    !> Building kdtree recursively. Best-First Method.    
    !> Building kdtre :: leaf
    !! \param node_ptr pointer to node
    !! \param x_ptr pointer to input explanatory variable
    !! \param node_idx index of node
    !! \param split_indices indices of split points from root to parent node.
    recursive subroutine build_new_rec(this, node_ptr, x_ptr, node_idx, split_indices)
        implicit none
        class(kdtree) :: this
        type(node_type), pointer, intent(inout) :: node_ptr
        real(kind=8), pointer  :: x_ptr(:,:)
        integer(kind=8), intent(inout) :: node_idx
        integer(kind=8), allocatable, intent(inout) :: split_indices(:)

        integer(kind=8) :: i, idx, split_fid, med_idx
        integer(kind=8) :: n_samples_l, n_samples_r
        integer(kind=8) :: d, u, tmp_depth
        real(kind=8)    :: split_val
        type(node_type), target  :: node_l,     node_r

        real(kind=8), ALLOCATABLE :: f_tmp(:), vars(:)
        integer(kind=8), ALLOCATABLE :: indices_l(:), indices_r(:)
        integer(kind=8), ALLOCATABLE :: split_indices_l(:), split_indices_r(:)

        node_ptr%idx = node_idx
        call node_ptr%get_feature_range(x_ptr)

        node_ptr%center = (node_ptr%f_max(:) - node_ptr%f_min(:)) * 0.5d0
        node_ptr%radius = sum((node_ptr%f_max(:) - node_ptr%f_min(:))**2d0) ** 0.5d0 / 2d0

        if (this%min_samples_in_leaf > node_ptr%n_samples) then
            node_ptr%is_leaf = t_

            node_ptr%indices_ = [node_ptr%indices, split_indices]
            node_ptr%n_samples_ = size(node_ptr%indices_)
            allocate(node_ptr%x_(node_ptr%n_samples_,this%n_columns))
            node_ptr%x_(:,:) = x_ptr(node_ptr%indices_,:)

            allocate(node_ptr%x_sq_(node_ptr%n_samples_))
            node_ptr%x_sq_(:) = sum(node_ptr%x_(:,:)*node_ptr%x_(:,:), dim=2)

            allocate(node_ptr%x(node_ptr%n_samples,this%n_columns))
            node_ptr%x(:,:) = x_ptr(node_ptr%indices,:)

            allocate(node_ptr%x_sq(node_ptr%n_samples))
            node_ptr%x_sq(:) = sum(node_ptr%x(:,:)*node_ptr%x(:,:), dim=2)

            allocate(node_ptr%xt_(this%n_columns, node_ptr%n_samples_))
            node_ptr%xt_(:,:) = transpose(x_ptr(node_ptr%indices_,:))
            return
        end if

        allocate(f_tmp(node_ptr%n_samples))


        ! Select Feature
        vars = node_ptr%comp_feature_vars(x_ptr)
        node_ptr%split_fid = maxloc(vars, dim=1)
        111 continue
        ! Extract Data
        do i=1, node_ptr%n_samples, 1
            idx = node_ptr%indices(i)
            f_tmp(i) = x_ptr(idx,node_ptr%split_fid)
        end do

        ! argsort
        call quick_argsort(f_tmp, node_ptr%indices, node_ptr%n_samples)
        med_idx = node_ptr%n_samples / 2_8
        node_ptr%split_val = f_tmp(med_idx)
        med_idx = count(f_tmp <= node_ptr%split_val)
        node_ptr%split_val = f_tmp(med_idx)

        if (med_idx==0_8 .and. med_idx==node_ptr%n_samples) then
            vars(node_ptr%split_fid) = -huge(0d0)
            node_ptr%split_fid = maxloc(vars, dim=1)
            goto 111
        end if
        call node_ptr%adopting_twins_in_kdtree(med_idx)

        node_idx = node_idx + 1
        allocate(split_indices_l(size(split_indices)))
        split_indices_l = [split_indices, node_ptr%indices(med_idx)]
        call this%build_new_rec(node_ptr%node_l_ptr, x_ptr, node_idx, split_indices_l)

        node_idx = node_idx + 1
        allocate(split_indices_r(0))
        call this%build_new_rec(node_ptr%node_r_ptr, x_ptr, node_idx, split_indices_r)
    end subroutine build_new_rec


    !> Get Neighbors . Euclid distance Only.
    !! \param q query points
    !! \param n_neighbors number of nearest neighbors.
    !! \param radius ball raius size
    function query(this, q, n_neighbors, radius)
        implicit none
        class(kdtree)                         :: this
        type(neighbor_results)                  :: query
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
                if (this%use_brute_force) then
                    query = this%bfs%query(q_ptr, n_neighbors=n_neighbors)
                else
                    query = this%query_nearest_n_neighbors(q_ptr, n_neighbors)
                end if
            end if
        end if

        if (present(radius)) then
            if (radius < 0d0) then
                stop "'radius' must be greater equal 0."
            else
                if (this%use_brute_force) then
                    query = this%bfs%query(q_ptr, radius=radius)
                else
                    radius_sq = radius**2d0
                    query = this%query_nearest_radius(q_ptr, radius_sq)
                end if
            end if
        end if
    end function query


    !> Get the nearest neighbors in radius R
    !! \param q_ptr pointer to query points
    !! \param square of radius to search
    function query_nearest_radius(this, q_ptr, radius_sq)
        implicit none
        type(neighbor_results)     :: query_nearest_radius
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
        call ifdealloc(this%q_sq_sum)
        allocate(this%q_sq_sum(n_samples))
        call matrix_sqsum_row(q_ptr, this%q_sq_sum, n_samples, n_columns, parallel=f_)

        ! print*, 2
        if ( allocated(query_nearest_radius%indices) ) &
            deallocate(query_nearest_radius%indices)
        if ( allocated(query_nearest_radius%distances) ) &
            deallocate(query_nearest_radius%distances)
        allocate(query_nearest_radius%indices(n_samples))
        allocate(query_nearest_radius%distances(n_samples))
        allocate(q_vals(n_columns))

        !$omp parallel num_threads(8)
        !$omp do private(leaf_node_ptr, nearest_dsts, nearest_idxs, i, node_ptr, fid, val, q_vals, q_idx, q_sq, max_radius) &
        !$omp reduction(+:t_get_leaf) reduction(+:t_search_best) reduction(+:time_leaf) reduction(+:time_internal)
        do i=1, n_samples, 1
            q_idx = i
            q_vals = q_ptr(q_idx,:)
            q_sq = this%q_sq_sum(q_idx)
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
                max_radius = maxval(nearest_dsts)

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
            query_nearest_radius%distances(i)%dst = sqrt(abs(nearest_dsts(1:n_size)))
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
                    ! call quick_argselect(XxQ, indices, size(XxQ)+0_8, n)
                    call quick_argsort(XxQ, indices, size(XxQ, kind=8))
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
            max_radius = maxval(nearest_dsts)

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
    function query_nearest_n_neighbors(this, q_ptr, n_neighbors) result(neighbors)
        implicit none
        type(neighbor_results)     :: neighbors
        class(kdtree)            :: this
        real(kind=8), pointer, intent(in) :: q_ptr(:,:)
        integer(kind=8), intent(in) :: n_neighbors

        integer(kind=8)       :: n_samples, n_columns, q_shape(2)

        real(kind=8)                 :: val, max_radius, q_sq
        integer(kind=8)              :: i, nearest_idx, fid, q_idx
        integer(kind=8), allocatable :: indices(:), nearest_idxs(:)
        real(kind=8), allocatable :: nearest_dsts(:), q_vals(:), q_sq_sum(:)
        type(node_type), target :: root_node
        type(node_type), pointer :: root_node_ptr, node_ptr, leaf_node_ptr

        integer(kind=8) :: date_value1(8), date_value2(8)
        integer(kind=8), save :: time_leaf, t_get_leaf, time_internal, t_search_best

        ! print*, 1
        q_shape = shape(q_ptr)
        n_samples = q_shape(1)
        n_columns = q_shape(2)
        call ifdealloc(this%q_sq_sum)
        allocate(this%q_sq_sum(n_samples))
        call matrix_sqsum_row(q_ptr, this%q_sq_sum, n_samples, n_columns, parallel=f_)

        ! print*, 2
        if ( allocated(neighbors%indices) ) &
            deallocate(neighbors%indices)
        if ( allocated(neighbors%distances) ) &
            deallocate(neighbors%distances)
        allocate(neighbors%indices(n_samples))
        allocate(neighbors%distances(n_samples))
        do i=1, n_samples, 1
            allocate(neighbors%indices(i)%idx(n_neighbors))
            allocate(neighbors%distances(i)%dst(n_neighbors))
        end do
        allocate(q_vals(n_columns))
        allocate(nearest_dsts(n_neighbors))
        allocate(nearest_idxs(n_neighbors))

        !$omp parallel num_threads(8)
        !$omp do private(leaf_node_ptr, nearest_dsts, nearest_idxs, i, node_ptr, fid, val, q_vals, q_idx, q_sq, max_radius)
        do q_idx=1, n_samples, 1
            q_vals = q_ptr(q_idx,:)
            q_sq = this%q_sq_sum(q_idx)
            nearest_dsts = huge(0d0)
            nearest_idxs = -2
            call this%query_nearest_n_neighbors_rec_get_leaf_ptr(this%root_node_ptr_, q_vals, q_sq, & 
                leaf_node_ptr, nearest_dsts, nearest_idxs, n_columns, n_neighbors)

            node_ptr => leaf_node_ptr
            do while (t_)
                fid = node_ptr%node_p_ptr%split_fid
                val = node_ptr%node_p_ptr%split_val
                max_radius = maxval(nearest_dsts)

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
            call quick_argsort(nearest_dsts, nearest_idxs, size(nearest_idxs, kind=8))
            neighbors%indices(q_idx)%idx(1:n_neighbors)   = nearest_idxs
            neighbors%distances(q_idx)%dst(1:n_neighbors) = sqrt(relu(nearest_dsts))
            ! neighbors%distances(q_idx)%dst(1:n_neighbors) = sqrt(relu(nearest_dsts))
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
        logical(kind=4) :: goto_left, check_dup
        real(kind=8), allocatable :: XxQ(:), tmp_d(:)
        integer(kind=8) :: n
        integer(kind=8), save :: count_comp_dist = 0
        
        if (root_node_ptr%is_leaf) then
            ! Search Closest Point from Leaf Node
            leaf_ptr => root_node_ptr
            allocate(XxQ(leaf_ptr%n_samples_))
            allocate(indices(leaf_ptr%n_samples_))
            call multi_mat_vec(leaf_ptr%x_, q, XxQ, &
                leaf_ptr%n_samples_, n_columns, f_)
            count_comp_dist = count_comp_dist + 1
            XxQ(:) = q_sq + leaf_ptr%x_sq_(:) - 2d0*XxQ(:)
            if ( maxval(nearest_dsts) < minval(XxQ) ) then
                !  skip
            else
                do i=1, leaf_ptr%n_samples_
                    indices(i) = leaf_ptr%indices_(i)
                end do
                tmp_d = [XxQ, nearest_dsts]
                tmp_i = [indices, nearest_idxs]
                n = minval((/size(tmp_d)+0_8, n_neighbors/))
                call quick_argsort(tmp_d, tmp_i, size(tmp_d, kind=8_8))
                nearest_dsts(1:n) = tmp_d(1:n)
                nearest_idxs(1:n) = tmp_i(1:n)
            end if
            return
        end if

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
        logical(kind=4) :: check_dup
        integer(kind=8), save :: count_skip=0, count_all=0

        if (subtree_root_node_ptr%is_leaf) then
            ! Search Closest Point from Leaf Node
            allocate(XxQ(subtree_root_node_ptr%n_samples_))
            allocate(indices(subtree_root_node_ptr%n_samples_))
            call multi_mat_vec(subtree_root_node_ptr%x_, q, XxQ, &
                subtree_root_node_ptr%n_samples_, n_columns, f_)
            XxQ(:) = q_sq + subtree_root_node_ptr%x_sq_(:) - 2d0*XxQ(:)
            ! count_all=count_all+1
            if ( maxval(nearest_dsts) < minval(XxQ) ) then
                ! count_skip = count_skip + 1
            else
                do i=1, subtree_root_node_ptr%n_samples_
                    indices(i) = subtree_root_node_ptr%indices_(i)
                end do
                tmp_d = [XxQ, nearest_dsts]
                tmp_i = [indices, nearest_idxs]
                n = minval((/size(tmp_d)+0_8, n_neighbors/))
                call quick_argsort(tmp_d, tmp_i, size(tmp_d, kind=8))
                ! check_dup = check_duplication(tmp_i, size(tmp_i, kind=8))
                ! print*, "other leaf node duplication check: ", check_dup
                nearest_dsts(1:n) = tmp_d(1:n)
                nearest_idxs(1:n) = tmp_i(1:n)
            end if
            ! print*, "skip", count_skip, count_all
            return
        else
            ! Compute Distance From Q to Split Plane
            fid = subtree_root_node_ptr%split_fid
            val_x = subtree_root_node_ptr%split_val
            val_q = q(fid)
            dist_from_q_to_plane = (val_q - val_x)**2d0
            max_radius = maxval(nearest_dsts)

            if (max_radius < dist_from_q_to_plane) then
                if (val_q <= val_x) then
                    call this%query_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_l_ptr, &
                        q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
                else
                    call this%query_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_r_ptr, &
                        q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
                end if
            else
                if (val_q <= val_x) then
                    call this%query_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_l_ptr, &
                        q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
                    call this%query_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_r_ptr, &
                        q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
                else
                    call this%query_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_r_ptr, &
                        q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
                    call this%query_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_l_ptr, &
                        q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
                end if
            end if
        end if
    end subroutine query_nearest_n_neighbors_search_subtree


    ! !> Get Neighbors . Euclid distance Only.
    ! !! \param q query points
    ! !! \param n_neighbors number of nearest neighbors.
    ! !! \param radius ball raius size
    ! function query_new(this, q, n_neighbors, radius)
    !     implicit none
    !     class(kdtree)                         :: this
    !     type(neighbor_results)                  :: query_new
    !     real(kind=8), target, intent(in)      :: q(:,:)
    !     integer(kind=8), optional, intent(in) :: n_neighbors
    !     real(kind=8), optional, intent(in)    :: radius

    !     real(kind=8), pointer      :: q_ptr(:,:)
    !     real(kind=8) :: radius_sq

    !     if (present(n_neighbors) .and. present(radius)) then
    !         stop "Only one of 'n_neighbors' or 'radius' can be specified."
    !     end if

    !     q_ptr => q

    !     if (present(n_neighbors)) then
    !         if (n_neighbors < 1_8) then
    !             stop "'n_neighbors' must be greater equal 1."
    !         else
    !             if (this%use_brute_force) then
    !                 query_new = this%bfs%query(q_ptr, n_neighbors=n_neighbors)
    !             else
    !                 query_new = this%query_new_nearest_n_neighbors(q_ptr, n_neighbors)
    !             end if
    !         end if
    !     end if

    !     if (present(radius)) then
    !         if (radius < 0d0) then
    !             stop "'radius' must be greater equal 0."
    !         else
    !             if (this%use_brute_force) then
    !                 query_new = this%bfs%query(q_ptr, radius=radius)
    !             else
    !                 radius_sq = radius**2d0
    !                 query_new = this%query_new_nearest_radius(q_ptr, radius_sq)
    !             end if
    !         end if
    !     end if
    ! end function query_new


    ! !> Get the nearest neighbors in radius R
    ! !! \param q_ptr pointer to query_new points
    ! !! \param square of radius to search
    ! function query_new_nearest_radius(this, q_ptr, radius_sq)
    !     implicit none
    !     type(neighbor_results)     :: query_new_nearest_radius
    !     class(kdtree)            :: this
    !     real(kind=8), pointer, intent(in) :: q_ptr(:,:)
    !     real(kind=8), intent(in) :: radius_sq

    !     integer(kind=8)       :: n_samples, n_columns, q_shape(2)

    !     real(kind=8)                 :: val, max_radius, q_sq
    !     integer(kind=8)              :: i, nearest_idx, fid, q_idx
    !     integer(kind=8), allocatable :: indices(:), nearest_idxs(:)
    !     real(kind=8), allocatable :: nearest_dsts(:), q_vals(:)
    !     type(node_type), target :: root_node
    !     type(node_type), pointer :: root_node_ptr, node_ptr, leaf_node_ptr

    !     integer(kind=8) :: date_value1(8), date_value2(8), n_size
    !     integer(kind=8), save :: time_leaf, t_get_leaf, time_internal, t_search_best

    !     ! print*, 1
    !     q_shape = shape(q_ptr)
    !     n_samples = q_shape(1)
    !     n_columns = q_shape(2)
    !     call ifdealloc(this%q_sq_sum)
    !     allocate(this%q_sq_sum(n_samples))
    !     call matrix_sqsum_row(q_ptr, this%q_sq_sum, n_samples, n_columns, parallel=f_)

    !     ! print*, 2
    !     if ( allocated(query_new_nearest_radius%indices) ) &
    !         deallocate(query_new_nearest_radius%indices)
    !     if ( allocated(query_new_nearest_radius%distances) ) &
    !         deallocate(query_new_nearest_radius%distances)
    !     allocate(query_new_nearest_radius%indices(n_samples))
    !     allocate(query_new_nearest_radius%distances(n_samples))
    !     allocate(q_vals(n_columns))

    !     !$omp parallel num_threads(8)
    !     !$omp do private(leaf_node_ptr, nearest_dsts, nearest_idxs, i, node_ptr, fid, val, q_vals, q_idx, q_sq, max_radius) &
    !     !$omp reduction(+:t_get_leaf) reduction(+:t_search_best) reduction(+:time_leaf) reduction(+:time_internal)
    !     do i=1, n_samples, 1
    !         q_idx = i
    !         q_vals = q_ptr(q_idx,:)
    !         q_sq = this%q_sq_sum(q_idx)
    !         allocate(nearest_dsts(1))
    !         allocate(nearest_idxs(1))
    !         nearest_dsts = radius_sq
    !         nearest_idxs = -2
    !         call this%query_new_nearest_radius_rec_get_leaf_ptr(this%root_node_ptr_, q_vals, q_sq, & 
    !             leaf_node_ptr, nearest_dsts, nearest_idxs, n_columns, radius_sq)

    !         node_ptr => leaf_node_ptr

    !         do while (t_)
    !             fid = node_ptr%node_p_ptr%split_fid
    !             val = node_ptr%node_p_ptr%split_val
    !             max_radius = nearest_dsts(size(nearest_dsts))

    !             if ( (q_vals(fid)-val)**2d0 < max_radius ) then
    !                 if (node_ptr%is_left) then
    !                     call this%query_new_nearest_radius_search_subtree(node_ptr%node_p_ptr%node_r_ptr, &
    !                         q_vals, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !                 else
    !                     call this%query_new_nearest_radius_search_subtree(node_ptr%node_p_ptr%node_l_ptr, &
    !                         q_vals, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !                 end if
    !             end if
    !             node_ptr => node_ptr%node_p_ptr
    !             if (.not. ASSOCIATED(node_ptr%node_p_ptr)) exit
    !         end do

    !         call quick_argsort(nearest_dsts, nearest_idxs, size(nearest_idxs)+0_8)
    !         n_size = size(nearest_idxs)-1
    !         allocate(query_new_nearest_radius%indices(i)%idx(n_size))
    !         allocate(query_new_nearest_radius%distances(i)%dst(n_size))
    !         query_new_nearest_radius%indices(i)%idx = nearest_idxs(1:n_size)
    !         query_new_nearest_radius%distances(i)%dst = sqrt(abs(nearest_dsts(1:n_size)))
    !         deallocate(nearest_dsts)
    !         deallocate(nearest_idxs)
    !     end do
    !     !$omp end do
    !     !$omp end parallel
    ! end function query_new_nearest_radius


    ! !> Get Leaf Node pointer of Query point
    ! !! \param root_node_ptr pointer to root node
    ! !! \param q a coordinate of query_new point
    ! !! \param q_sq a square norm of query_new point
    ! !! \param leaf_ptr pointer to leaf node of 'q' to return
    ! !! \param nearest_dsts nearest distances in radius
    ! !! \param nearest_idxs nearest point indices in radius
    ! !! \param n_columns number of columns of q
    ! !! \param radius_sq square of raius
    ! recursive subroutine query_new_nearest_radius_rec_get_leaf_ptr(this, root_node_ptr, q, q_sq, leaf_ptr, &
    !     nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !     implicit none
    !     class(kdtree)            :: this
    !     type(node_type), pointer :: root_node_ptr
    !     real(kind=8), intent(in) :: q(n_columns)
    !     real(kind=8), intent(in) :: q_sq
    !     type(node_type), pointer :: leaf_ptr
    !     real(kind=8), allocatable, intent(inout) :: nearest_dsts(:)
    !     integer(kind=8), allocatable, intent(inout) :: nearest_idxs(:)
    !     integer(kind=8), intent(in)    :: n_columns
    !     real(kind=8), intent(in) :: radius_sq

    !     real(kind=8) :: f_val, dist, dist_new, dist_old, idx_new, idx_old, dist_q, dist_from_q_to_x
    !     integer(kind=8) :: i, j, idx, x_idx, f_idx, n_samples_l, n_samples_r, idx_l, idx_r
    !     integer(kind=8), allocatable :: indices_l(:), indices_r(:), indices(:), tmp_i(:)
    !     logical(kind=4), allocatable :: tmp_l(:)
    !     logical(kind=4) :: goto_left
    !     real(kind=8), allocatable :: XxQ(:), tmp_d(:)
    !     integer(kind=8) :: n

    !     if (root_node_ptr%is_leaf) then
    !         ! Search Closest Point from Leaf Node
    !         allocate(XxQ(root_node_ptr%n_samples_))
    !         allocate(indices(root_node_ptr%n_samples_))
    !         leaf_ptr => root_node_ptr
    !         call multi_mat_vec(root_node_ptr%x_, q, XxQ, &
    !             root_node_ptr%n_samples_, n_columns, f_)
    !         XxQ(:) = q_sq + root_node_ptr%x_sq_(:) - 2d0*XxQ(:)
    !         if ( radius_sq < minval(XxQ) ) then
    !             !  skip
    !         else
    !             do i=1, root_node_ptr%n_samples_
    !                 indices(i) = root_node_ptr%indices_(i)
    !             end do
    !             n = count(XxQ <= radius_sq)
    !             if ( n > 0) then
    !                 call quick_argselect(XxQ, indices, size(XxQ)+0_8, n)
    !                 nearest_dsts = [nearest_dsts, XxQ(1:n)]
    !                 nearest_idxs = [nearest_idxs, indices(1:n)]
    !             end if
    !         end if
    !         return
    !     end if

    !     f_idx = root_node_ptr%split_fid
    !     f_val = root_node_ptr%split_val

    !     goto_left = q(f_idx) <= f_val

    !     if (goto_left) then
    !         call query_new_nearest_radius_rec_get_leaf_ptr(this, root_node_ptr%node_l_ptr, q, q_sq, leaf_ptr, &
    !             nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !     else
    !         call query_new_nearest_radius_rec_get_leaf_ptr(this, root_node_ptr%node_r_ptr, q, q_sq, leaf_ptr, &
    !             nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !     end if
    ! end subroutine query_new_nearest_radius_rec_get_leaf_ptr


    ! !> Get nearest data point of Query point from subtree
    ! !! \param subtree_root_node_ptr pointer to root node of subtree to search
    ! !! \param q a coordinate of query_new point
    ! !! \param q_sq a square norm of query_new point
    ! !! \param nearest_dsts nearest distances in radius
    ! !! \param nearest_idxs nearest point indices in radius
    ! !! \param n_columns number of columns of q
    ! !! \param radius_sq square of raius
    ! recursive subroutine query_new_nearest_radius_search_subtree(this, subtree_root_node_ptr, q, q_sq, &
    !     nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !     implicit none
    !     class(kdtree) :: this
    !     type(node_type), pointer       :: subtree_root_node_ptr
    !     real(kind=8), intent(in)       :: q(n_columns), q_sq
    !     integer(kind=8), allocatable, intent(inout) :: nearest_idxs(:)
    !     real(kind=8), allocatable, intent(inout)    :: nearest_dsts(:)
    !     integer(kind=8), intent(in)    :: n_columns
    !     real(kind=8), intent(in)    :: radius_sq

    !     integer(kind=8) :: i, j, idx_old, idx_new, x_idx, fid, n, n_tmp
    !     real(kind=8) :: dist_old, dist_new, dist, dist_from_q_to_plane, val_x, val_q, dist_from_q_to_x
    !     real(kind=8) :: max_radius
    !     real(kind=8), allocatable :: XxQ(:), tmp_d(:)
    !     integer(kind=8), allocatable :: tmp_i(:), indices(:)

    !     if (subtree_root_node_ptr%is_leaf) then
    !         allocate(XxQ(subtree_root_node_ptr%n_samples_))
    !         allocate(indices(subtree_root_node_ptr%n_samples_))
    !         call multi_mat_vec(subtree_root_node_ptr%x_, q, XxQ, &
    !             subtree_root_node_ptr%n_samples_, n_columns, f_)
    !         XxQ(:) = q_sq + subtree_root_node_ptr%x_sq_(:) - 2d0*XxQ(:)
    !         if ( radius_sq < minval(XxQ) ) then
    !             !  skip
    !         else
    !             do i=1, subtree_root_node_ptr%n_samples_
    !                 indices(i) = subtree_root_node_ptr%indices_(i)
    !             end do
    !             n = count(XxQ <= radius_sq)
    !             if ( n > 0) then
    !                 ! call quick_argselect(XxQ, indices, size(XxQ)+0_8, n)
    !                 call quick_argsort(XxQ, indices, size(XxQ, kind=8))
    !                 nearest_dsts = [nearest_dsts, XxQ(1:n)]
    !                 nearest_idxs = [nearest_idxs, indices(1:n)]
    !             end if
    !         end if
    !         return
    !     else
    !         ! Compute Distance From Q to Split Plane
    !         fid = subtree_root_node_ptr%split_fid
    !         val_x = subtree_root_node_ptr%split_val
    !         val_q = q(fid)
    !         dist_from_q_to_plane = (val_q-val_x)**2d0
    !         max_radius = nearest_dsts(size(nearest_dsts))

    !         if (max_radius < dist_from_q_to_plane) then
    !             if (val_q <= val_x) then
    !                 call this%query_new_nearest_radius_search_subtree(subtree_root_node_ptr%node_l_ptr, &
    !                     q, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !             else
    !                 call this%query_new_nearest_radius_search_subtree(subtree_root_node_ptr%node_r_ptr, &
    !                     q, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !             end if
    !         else

    !             call this%query_new_nearest_radius_search_subtree(subtree_root_node_ptr%node_l_ptr, &
    !                 q, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !             call this%query_new_nearest_radius_search_subtree(subtree_root_node_ptr%node_r_ptr, &
    !                 q, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !         end if
    !     end if
    ! end subroutine query_new_nearest_radius_search_subtree


    ! !> Get N nearest neighbors
    ! !! \param q_ptr pointer to query points
    ! !! \param n_neighbors number of neighbors to return
    ! function query_new_nearest_n_neighbors(this, q_ptr, n_neighbors) result(neighbors)
    !     implicit none
    !     type(neighbor_results)     :: neighbors
    !     class(kdtree)            :: this
    !     real(kind=8), pointer, intent(in) :: q_ptr(:,:)
    !     integer(kind=8), intent(in) :: n_neighbors

    !     integer(kind=8)       :: n_samples, n_columns, q_shape(2)

    !     real(kind=8)                 :: val, max_radius, q_sq
    !     integer(kind=8)              :: i, nearest_idx, fid, q_idx
    !     integer(kind=8), allocatable :: indices(:), nearest_idxs(:)
    !     real(kind=8), allocatable :: nearest_dsts(:), q_vals(:), q_sq_sum(:)
    !     type(node_type), target :: root_node
    !     type(node_type), pointer :: root_node_ptr, node_ptr, leaf_node_ptr

    !     integer(kind=8) :: date_value1(8), date_value2(8)
    !     integer(kind=8), save :: time_leaf, t_get_leaf, time_internal, t_search_best

    !     ! print*, 1
    !     q_shape = shape(q_ptr)
    !     n_samples = q_shape(1)
    !     n_columns = q_shape(2)
    !     call ifdealloc(this%q_sq_sum)
    !     allocate(this%q_sq_sum(n_samples))
    !     call matrix_sqsum_row(q_ptr, this%q_sq_sum, n_samples, n_columns, parallel=f_)

    !     ! print*, 2
    !     if ( allocated(neighbors%indices) ) &
    !         deallocate(neighbors%indices)
    !     if ( allocated(neighbors%distances) ) &
    !         deallocate(neighbors%distances)
    !     allocate(neighbors%indices(n_samples))
    !     allocate(neighbors%distances(n_samples))
    !     do i=1, n_samples, 1
    !         allocate(neighbors%indices(i)%idx(n_neighbors))
    !         allocate(neighbors%distances(i)%dst(n_neighbors))
    !     end do
    !     allocate(q_vals(n_columns))
    !     allocate(nearest_dsts(n_neighbors))
    !     allocate(nearest_idxs(n_neighbors))

    !     !$omp parallel num_threads(8)
    !     !$omp do private(leaf_node_ptr, nearest_dsts, nearest_idxs, i, node_ptr, fid, val, q_vals, q_idx, q_sq, max_radius)
    !     do q_idx=1, n_samples, 1
    !         q_vals = q_ptr(q_idx,:)
    !         q_sq = this%q_sq_sum(q_idx)
    !         nearest_dsts = huge(0d0)
    !         nearest_idxs = -2
    !         call this%query_new_nearest_n_neighbors_rec_get_leaf_ptr(this%root_node_ptr_, q_vals, q_sq, & 
    !             leaf_node_ptr, nearest_dsts, nearest_idxs, n_columns, n_neighbors)

    !         node_ptr => leaf_node_ptr
    !         do while (t_)
    !             fid = node_ptr%node_p_ptr%split_fid
    !             val = node_ptr%node_p_ptr%split_val
    !             max_radius = nearest_dsts(n_neighbors)

    !             if ( (q_vals(fid)-val)**2d0 < max_radius ) then
    !                 if (node_ptr%is_left) then
    !                     call this%query_new_nearest_n_neighbors_search_subtree(node_ptr%node_p_ptr%node_r_ptr, &
    !                         q_vals, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !                 else
    !                     call this%query_new_nearest_n_neighbors_search_subtree(node_ptr%node_p_ptr%node_l_ptr, &
    !                         q_vals, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !                 end if
    !             end if
    !             node_ptr => node_ptr%node_p_ptr
    !             if (.not. ASSOCIATED(node_ptr%node_p_ptr)) exit
    !         end do
    !         call quick_argsort(nearest_dsts, nearest_idxs, size(nearest_idxs, kind=8))
    !         neighbors%indices(q_idx)%idx(1:n_neighbors)   = nearest_idxs
    !         neighbors%distances(q_idx)%dst(1:n_neighbors) = sqrt(relu(nearest_dsts))
    !         ! neighbors%distances(q_idx)%dst(1:n_neighbors) = sqrt(relu(nearest_dsts))
    !     end do
    !     !$omp end do
    !     !$omp end parallel 
    ! end function query_new_nearest_n_neighbors


    ! !> Get Leaf Node pointer of Query point
    ! !! \param root_node_ptr pointer to root node
    ! !! \param q a coordinate of query_new point
    ! !! \param q_sq a square norm of query_new point
    ! !! \param leaf_ptr pointer to leaf node of 'q' to return
    ! !! \param nearest_dsts nearest distances in radius
    ! !! \param nearest_idxs nearest point indices in radius
    ! !! \param n_columns number of columns of q
    ! !! \param n_neighbors number of neighbors to return
    ! recursive subroutine query_new_nearest_n_neighbors_rec_get_leaf_ptr(this, root_node_ptr, q, q_sq, leaf_ptr, &
    !     nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !     implicit none
    !     class(kdtree)            :: this
    !     type(node_type), pointer :: root_node_ptr
    !     real(kind=8), intent(in) :: q(n_columns)
    !     real(kind=8), intent(in) :: q_sq
    !     type(node_type), pointer :: leaf_ptr
    !     real(kind=8), intent(inout) :: nearest_dsts(n_neighbors)
    !     integer(kind=8), intent(inout) :: nearest_idxs(n_neighbors)
    !     integer(kind=8), intent(in)    :: n_columns, n_neighbors

    !     real(kind=8) :: f_val, dist, dist_new, dist_old, idx_new, idx_old, dist_q, dist_from_q_to_x
    !     integer(kind=8) :: i, j, idx, x_idx, f_idx, n_samples_l, n_samples_r, idx_l, idx_r
    !     integer(kind=8), allocatable :: indices_l(:), indices_r(:), indices(:), tmp_i(:)
    !     logical(kind=4), allocatable :: tmp_l(:)
    !     logical(kind=4) :: goto_left, check_dup
    !     real(kind=8), allocatable :: XxQ(:), tmp_d(:)
    !     integer(kind=8) :: n
    !     integer(kind=8), save :: count_comp_dist = 0
        
    !     if (root_node_ptr%is_leaf) then
    !         ! Search Closest Point from Leaf Node
    !         leaf_ptr => root_node_ptr
    !         allocate(XxQ(leaf_ptr%n_samples_))
    !         allocate(indices(leaf_ptr%n_samples_))
    !         call openblas_set_num_threads(1)
    !         call dgemv("N", root_node_ptr%n_samples_, n_columns, &
    !             1d0, root_node_ptr%x_, root_node_ptr%n_samples_, &
    !             q, 1_8, 0d0, XxQ, 1_8)
    !         XxQ(:) = q_sq + leaf_ptr%x_sq_(:) - 2d0*XxQ(:)
    !         if ( nearest_dsts(n_neighbors) < minval(XxQ) ) then
    !             !  skip
    !         else
    !             do i=1, leaf_ptr%n_samples_
    !                 indices(i) = leaf_ptr%indices_(i)
    !             end do
    !             tmp_d = [XxQ, nearest_dsts]
    !             tmp_i = [indices, nearest_idxs]
    !             n = minval((/size(tmp_d)+0_8, n_neighbors/))
    !             call quick_argsort(tmp_d, tmp_i, size(tmp_d, kind=8_8))
    !             nearest_dsts(1:n) = tmp_d(1:n)
    !             nearest_idxs(1:n) = tmp_i(1:n)
    !         end if
    !         return
    !     end if

    !     f_idx = root_node_ptr%split_fid
    !     f_val = root_node_ptr%split_val

    !     goto_left = q(f_idx) <= f_val

    !     if (goto_left) then
    !         call query_new_nearest_n_neighbors_rec_get_leaf_ptr(this, root_node_ptr%node_l_ptr, q, q_sq, leaf_ptr, &
    !             nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !     else
    !         call query_new_nearest_n_neighbors_rec_get_leaf_ptr(this, root_node_ptr%node_r_ptr, q, q_sq, leaf_ptr, &
    !             nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !     end if
    ! end subroutine query_new_nearest_n_neighbors_rec_get_leaf_ptr


    ! !> Get nearest data point of Query point from subtree
    ! !! \param subtree_root_node_ptr pointer to root node of subtree to search
    ! !! \param q a coordinate of query_new point
    ! !! \param q_sq a square norm of query_new point
    ! !! \param nearest_dsts nearest distances in radius
    ! !! \param nearest_idxs nearest point indices in radius
    ! !! \param n_columns number of columns of q
    ! !! \param n_neighbors number of neighbors to return 
    ! recursive subroutine query_new_nearest_n_neighbors_search_subtree(this, subtree_root_node_ptr, q, q_sq, &
    !     nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !     implicit none
    !     class(kdtree) :: this
    !     type(node_type), pointer       :: subtree_root_node_ptr
    !     real(kind=8), intent(in)       :: q(n_columns), q_sq
    !     integer(kind=8), intent(inout) :: nearest_idxs(n_neighbors)
    !     real(kind=8), intent(inout)    :: nearest_dsts(n_neighbors)
    !     integer(kind=8), intent(in)    :: n_columns, n_neighbors

    !     integer(kind=8) :: i, j, idx_old, idx_new, x_idx, fid, n, n_tmp
    !     real(kind=8) :: dist_old, dist_new, dist, dist_from_q_to_plane, val_x, val_q, dist_from_q_to_x
    !     real(kind=8) :: max_radius
    !     real(kind=8), allocatable :: XxQ(:), tmp_d(:)
    !     integer(kind=8), allocatable :: tmp_i(:), indices(:)
    !     logical(kind=4) :: check_dup
    !     real(kind=8) :: dist_q_to_rec

    !     ! dist_q_to_rec = subtree_root_node_ptr%comp_dist_rect_to_query(q, n_columns)
    !     ! if (nearest_dsts(n_neighbors) < dist_q_to_rec) return

    !     if (subtree_root_node_ptr%is_leaf) then
    !         ! Search Closest Point from Leaf Node
    !         allocate(XxQ(subtree_root_node_ptr%n_samples_))
    !         allocate(indices(subtree_root_node_ptr%n_samples_))
    !         call openblas_set_num_threads(1)
    !         XxQ(:) = q_sq + subtree_root_node_ptr%x_sq_(:)
    !         call dgemv("N", subtree_root_node_ptr%n_samples_, n_columns, &
    !             -2d0, subtree_root_node_ptr%x_, subtree_root_node_ptr%n_samples_, &
    !             q, 1_8, 1d0, XxQ, 1_8)
    !         if ( nearest_dsts(n_neighbors) < minval(XxQ) ) then
    !             ! skip
    !         else
    !             do i=1, subtree_root_node_ptr%n_samples_
    !                 indices(i) = subtree_root_node_ptr%indices_(i)
    !             end do
    !             tmp_d = [XxQ, nearest_dsts]
    !             tmp_i = [indices, nearest_idxs]
    !             n = minval((/size(tmp_d)+0_8, n_neighbors/))
    !             ! call quick_argsort(tmp_d, tmp_i, size(tmp_d, kind=8))
    !             call insertion_argsort(tmp_d, tmp_i, size(tmp_d, kind=8))
    !             ! check_dup = check_duplication(tmp_i, size(tmp_i, kind=8))
    !             ! print*, "other leaf node duplication check: ", check_dup
    !             nearest_dsts(1:n) = tmp_d(1:n)
    !             nearest_idxs(1:n) = tmp_i(1:n)
    !         end if
    !         return
    !     else
    !         ! Compute Distance From Q to Split Plane
    !         fid = subtree_root_node_ptr%split_fid
    !         val_x = subtree_root_node_ptr%split_val
    !         val_q = q(fid)
    !         dist_from_q_to_plane = (val_q - val_x)**2d0
    !         max_radius = nearest_dsts(n_neighbors)

    !         if (max_radius < dist_from_q_to_plane) then
    !             if (val_q <= val_x) then
    !                 call this%query_new_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_l_ptr, &
    !                     q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !             else
    !                 call this%query_new_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_r_ptr, &
    !                     q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !             end if
    !         else
    !             call this%query_new_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_l_ptr, &
    !                 q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !             call this%query_new_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_r_ptr, &
    !                 q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !         end if
    !     end if
    ! end subroutine query_new_nearest_n_neighbors_search_subtree


    ! !> Get Neighbors . Euclid distance Only.
    ! !! \param q query points
    ! !! \param n_neighbors number of nearest neighbors.
    ! !! \param radius ball raius size
    ! function query_new_T(this, q, n_neighbors, radius)
    !     implicit none
    !     class(kdtree)                         :: this
    !     type(neighbor_results)                  :: query_new_T
    !     real(kind=8), target, intent(in)      :: q(:,:)
    !     integer(kind=8), optional, intent(in) :: n_neighbors
    !     real(kind=8), optional, intent(in)    :: radius

    !     real(kind=8), pointer      :: q_ptr(:,:)
    !     real(kind=8) :: radius_sq

    !     if (present(n_neighbors) .and. present(radius)) then
    !         stop "Only one of 'n_neighbors' or 'radius' can be specified."
    !     end if

    !     q_ptr => q

    !     if (present(n_neighbors)) then
    !         if (n_neighbors < 1_8) then
    !             stop "'n_neighbors' must be greater equal 1."
    !         else
    !             if (this%use_brute_force) then
    !                 query_new_T = this%bfs%query(q_ptr, n_neighbors=n_neighbors)
    !             else
    !                 query_new_T = this%query_new_T_nearest_n_neighbors(q_ptr, n_neighbors)
    !             end if
    !         end if
    !     end if

    !     if (present(radius)) then
    !         if (radius < 0d0) then
    !             stop "'radius' must be greater equal 0."
    !         else
    !             if (this%use_brute_force) then
    !                 query_new_T = this%bfs%query(q_ptr, radius=radius)
    !             else
    !                 radius_sq = radius**2d0
    !                 query_new_T = this%query_new_T_nearest_radius(q_ptr, radius_sq)
    !             end if
    !         end if
    !     end if
    ! end function query_new_T


    ! !> Get the nearest neighbors in radius R
    ! !! \param q_ptr pointer to query_new_T points
    ! !! \param square of radius to search
    ! function query_new_T_nearest_radius(this, q_ptr, radius_sq)
    !     implicit none
    !     type(neighbor_results)     :: query_new_T_nearest_radius
    !     class(kdtree)            :: this
    !     real(kind=8), pointer, intent(in) :: q_ptr(:,:)
    !     real(kind=8), intent(in) :: radius_sq

    !     integer(kind=8)       :: n_samples, n_columns, q_shape(2)

    !     real(kind=8)                 :: val, max_radius, q_sq
    !     integer(kind=8)              :: i, nearest_idx, fid, q_idx
    !     integer(kind=8), allocatable :: indices(:), nearest_idxs(:)
    !     real(kind=8), allocatable :: nearest_dsts(:), q_vals(:)
    !     type(node_type), target :: root_node
    !     type(node_type), pointer :: root_node_ptr, node_ptr, leaf_node_ptr

    !     integer(kind=8) :: date_value1(8), date_value2(8), n_size
    !     integer(kind=8), save :: time_leaf, t_get_leaf, time_internal, t_search_best

    !     ! print*, 1
    !     q_shape = shape(q_ptr)
    !     n_samples = q_shape(1)
    !     n_columns = q_shape(2)
    !     call ifdealloc(this%q_sq_sum)
    !     allocate(this%q_sq_sum(n_samples))
    !     call matrix_sqsum_row(q_ptr, this%q_sq_sum, n_samples, n_columns, parallel=f_)

    !     ! print*, 2
    !     if ( allocated(query_new_T_nearest_radius%indices) ) &
    !         deallocate(query_new_T_nearest_radius%indices)
    !     if ( allocated(query_new_T_nearest_radius%distances) ) &
    !         deallocate(query_new_T_nearest_radius%distances)
    !     allocate(query_new_T_nearest_radius%indices(n_samples))
    !     allocate(query_new_T_nearest_radius%distances(n_samples))
    !     allocate(q_vals(n_columns))

    !     !$omp parallel num_threads(8)
    !     !$omp do private(leaf_node_ptr, nearest_dsts, nearest_idxs, i, node_ptr, fid, val, q_vals, q_idx, q_sq, max_radius) &
    !     !$omp reduction(+:t_get_leaf) reduction(+:t_search_best) reduction(+:time_leaf) reduction(+:time_internal)
    !     do i=1, n_samples, 1
    !         q_idx = i
    !         q_vals = q_ptr(q_idx,:)
    !         q_sq = this%q_sq_sum(q_idx)
    !         allocate(nearest_dsts(1))
    !         allocate(nearest_idxs(1))
    !         nearest_dsts = radius_sq
    !         nearest_idxs = -2
    !         call this%query_new_T_nearest_radius_rec_get_leaf_ptr(this%root_node_ptr_, q_vals, q_sq, & 
    !             leaf_node_ptr, nearest_dsts, nearest_idxs, n_columns, radius_sq)

    !         node_ptr => leaf_node_ptr

    !         do while (t_)
    !             fid = node_ptr%node_p_ptr%split_fid
    !             val = node_ptr%node_p_ptr%split_val
    !             max_radius = nearest_dsts(size(nearest_dsts))

    !             if ( (q_vals(fid)-val)**2d0 < max_radius ) then
    !                 if (node_ptr%is_left) then
    !                     call this%query_new_T_nearest_radius_search_subtree(node_ptr%node_p_ptr%node_r_ptr, &
    !                         q_vals, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !                 else
    !                     call this%query_new_T_nearest_radius_search_subtree(node_ptr%node_p_ptr%node_l_ptr, &
    !                         q_vals, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !                 end if
    !             end if
    !             node_ptr => node_ptr%node_p_ptr
    !             if (.not. ASSOCIATED(node_ptr%node_p_ptr)) exit
    !         end do

    !         call quick_argsort(nearest_dsts, nearest_idxs, size(nearest_idxs)+0_8)
    !         n_size = size(nearest_idxs)-1
    !         allocate(query_new_T_nearest_radius%indices(i)%idx(n_size))
    !         allocate(query_new_T_nearest_radius%distances(i)%dst(n_size))
    !         query_new_T_nearest_radius%indices(i)%idx = nearest_idxs(1:n_size)
    !         query_new_T_nearest_radius%distances(i)%dst = sqrt(abs(nearest_dsts(1:n_size)))
    !         deallocate(nearest_dsts)
    !         deallocate(nearest_idxs)
    !     end do
    !     !$omp end do
    !     !$omp end parallel
    ! end function query_new_T_nearest_radius


    ! !> Get Leaf Node pointer of Query point
    ! !! \param root_node_ptr pointer to root node
    ! !! \param q a coordinate of query_new_T point
    ! !! \param q_sq a square norm of query_new_T point
    ! !! \param leaf_ptr pointer to leaf node of 'q' to return
    ! !! \param nearest_dsts nearest distances in radius
    ! !! \param nearest_idxs nearest point indices in radius
    ! !! \param n_columns number of columns of q
    ! !! \param radius_sq square of raius
    ! recursive subroutine query_new_T_nearest_radius_rec_get_leaf_ptr(this, root_node_ptr, q, q_sq, leaf_ptr, &
    !     nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !     implicit none
    !     class(kdtree)            :: this
    !     type(node_type), pointer :: root_node_ptr
    !     real(kind=8), intent(in) :: q(n_columns)
    !     real(kind=8), intent(in) :: q_sq
    !     type(node_type), pointer :: leaf_ptr
    !     real(kind=8), allocatable, intent(inout) :: nearest_dsts(:)
    !     integer(kind=8), allocatable, intent(inout) :: nearest_idxs(:)
    !     integer(kind=8), intent(in)    :: n_columns
    !     real(kind=8), intent(in) :: radius_sq

    !     real(kind=8) :: f_val, dist, dist_new, dist_old, idx_new, idx_old, dist_q, dist_from_q_to_x
    !     integer(kind=8) :: i, j, idx, x_idx, f_idx, n_samples_l, n_samples_r, idx_l, idx_r
    !     integer(kind=8), allocatable :: indices_l(:), indices_r(:), indices(:), tmp_i(:)
    !     logical(kind=4), allocatable :: tmp_l(:)
    !     logical(kind=4) :: goto_left
    !     real(kind=8), allocatable :: XxQ(:), tmp_d(:)
    !     integer(kind=8) :: n

    !     if (root_node_ptr%is_leaf) then
    !         ! Search Closest Point from Leaf Node
    !         allocate(XxQ(root_node_ptr%n_samples_))
    !         allocate(indices(root_node_ptr%n_samples_))
    !         leaf_ptr => root_node_ptr
    !         call multi_mat_vec(root_node_ptr%x_, q, XxQ, &
    !             root_node_ptr%n_samples_, n_columns, f_)
    !         XxQ(:) = q_sq + root_node_ptr%x_sq_(:) - 2d0*XxQ(:)
    !         if ( radius_sq < minval(XxQ) ) then
    !             !  skip
    !         else
    !             do i=1, root_node_ptr%n_samples_
    !                 indices(i) = root_node_ptr%indices_(i)
    !             end do
    !             n = count(XxQ <= radius_sq)
    !             if ( n > 0) then
    !                 call quick_argselect(XxQ, indices, size(XxQ)+0_8, n)
    !                 nearest_dsts = [nearest_dsts, XxQ(1:n)]
    !                 nearest_idxs = [nearest_idxs, indices(1:n)]
    !             end if
    !         end if
    !         return
    !     end if

    !     f_idx = root_node_ptr%split_fid
    !     f_val = root_node_ptr%split_val

    !     goto_left = q(f_idx) <= f_val

    !     if (goto_left) then
    !         call query_new_T_nearest_radius_rec_get_leaf_ptr(this, root_node_ptr%node_l_ptr, q, q_sq, leaf_ptr, &
    !             nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !     else
    !         call query_new_T_nearest_radius_rec_get_leaf_ptr(this, root_node_ptr%node_r_ptr, q, q_sq, leaf_ptr, &
    !             nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !     end if
    ! end subroutine query_new_T_nearest_radius_rec_get_leaf_ptr


    ! !> Get nearest data point of Query point from subtree
    ! !! \param subtree_root_node_ptr pointer to root node of subtree to search
    ! !! \param q a coordinate of query_new_T point
    ! !! \param q_sq a square norm of query_new_T point
    ! !! \param nearest_dsts nearest distances in radius
    ! !! \param nearest_idxs nearest point indices in radius
    ! !! \param n_columns number of columns of q
    ! !! \param radius_sq square of raius
    ! recursive subroutine query_new_T_nearest_radius_search_subtree(this, subtree_root_node_ptr, q, q_sq, &
    !     nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !     implicit none
    !     class(kdtree) :: this
    !     type(node_type), pointer       :: subtree_root_node_ptr
    !     real(kind=8), intent(in)       :: q(n_columns), q_sq
    !     integer(kind=8), allocatable, intent(inout) :: nearest_idxs(:)
    !     real(kind=8), allocatable, intent(inout)    :: nearest_dsts(:)
    !     integer(kind=8), intent(in)    :: n_columns
    !     real(kind=8), intent(in)    :: radius_sq

    !     integer(kind=8) :: i, j, idx_old, idx_new, x_idx, fid, n, n_tmp
    !     real(kind=8) :: dist_old, dist_new, dist, dist_from_q_to_plane, val_x, val_q, dist_from_q_to_x
    !     real(kind=8) :: max_radius
    !     real(kind=8), allocatable :: XxQ(:), tmp_d(:)
    !     integer(kind=8), allocatable :: tmp_i(:), indices(:)

    !     if (subtree_root_node_ptr%is_leaf) then
    !         allocate(XxQ(subtree_root_node_ptr%n_samples_))
    !         allocate(indices(subtree_root_node_ptr%n_samples_))
    !         call multi_mat_vec(subtree_root_node_ptr%x_, q, XxQ, &
    !             subtree_root_node_ptr%n_samples_, n_columns, f_)
    !         XxQ(:) = q_sq + subtree_root_node_ptr%x_sq_(:) - 2d0*XxQ(:)
    !         if ( radius_sq < minval(XxQ) ) then
    !             !  skip
    !         else
    !             do i=1, subtree_root_node_ptr%n_samples_
    !                 indices(i) = subtree_root_node_ptr%indices_(i)
    !             end do
    !             n = count(XxQ <= radius_sq)
    !             if ( n > 0) then
    !                 ! call quick_argselect(XxQ, indices, size(XxQ)+0_8, n)
    !                 call quick_argsort(XxQ, indices, size(XxQ, kind=8))
    !                 nearest_dsts = [nearest_dsts, XxQ(1:n)]
    !                 nearest_idxs = [nearest_idxs, indices(1:n)]
    !             end if
    !         end if
    !         return
    !     else
    !         ! Compute Distance From Q to Split Plane
    !         fid = subtree_root_node_ptr%split_fid
    !         val_x = subtree_root_node_ptr%split_val
    !         val_q = q(fid)
    !         dist_from_q_to_plane = (val_q-val_x)**2d0
    !         max_radius = nearest_dsts(size(nearest_dsts))

    !         if (max_radius < dist_from_q_to_plane) then
    !             if (val_q <= val_x) then
    !                 call this%query_new_T_nearest_radius_search_subtree(subtree_root_node_ptr%node_l_ptr, &
    !                     q, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !             else
    !                 call this%query_new_T_nearest_radius_search_subtree(subtree_root_node_ptr%node_r_ptr, &
    !                     q, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !             end if
    !         else

    !             call this%query_new_T_nearest_radius_search_subtree(subtree_root_node_ptr%node_l_ptr, &
    !                 q, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !             call this%query_new_T_nearest_radius_search_subtree(subtree_root_node_ptr%node_r_ptr, &
    !                 q, q_sq, nearest_dsts, nearest_idxs, n_columns, radius_sq)
    !         end if
    !     end if
    ! end subroutine query_new_T_nearest_radius_search_subtree


    ! !> Get N nearest neighbors
    ! !! \param q_ptr pointer to query points
    ! !! \param n_neighbors number of neighbors to return
    ! function query_new_T_nearest_n_neighbors(this, q_ptr, n_neighbors) result(neighbors)
    !     implicit none
    !     type(neighbor_results)     :: neighbors
    !     class(kdtree)            :: this
    !     real(kind=8), pointer, intent(in) :: q_ptr(:,:)
    !     integer(kind=8), intent(in) :: n_neighbors

    !     integer(kind=8)       :: n_samples, n_columns, q_shape(2)

    !     real(kind=8)                 :: val, max_radius, q_sq
    !     integer(kind=8)              :: i, nearest_idx, fid, q_idx
    !     integer(kind=8), allocatable :: indices(:), nearest_idxs(:)
    !     real(kind=8), allocatable :: nearest_dsts(:), q_vals(:), q_sq_sum(:)
    !     type(node_type), target :: root_node
    !     type(node_type), pointer :: root_node_ptr, node_ptr, leaf_node_ptr

    !     integer(kind=8) :: date_value1(8), date_value2(8)
    !     integer(kind=8), save :: time_leaf, t_get_leaf, time_internal, t_search_best

    !     ! print*, 1
    !     q_shape = shape(q_ptr)
    !     n_samples = q_shape(1)
    !     n_columns = q_shape(2)
    !     call ifdealloc(this%q_sq_sum)
    !     allocate(this%q_sq_sum(n_samples))
    !     call matrix_sqsum_row(q_ptr, this%q_sq_sum, n_samples, n_columns, parallel=f_)

    !     ! print*, 2
    !     if ( allocated(neighbors%indices) ) &
    !         deallocate(neighbors%indices)
    !     if ( allocated(neighbors%distances) ) &
    !         deallocate(neighbors%distances)
    !     allocate(neighbors%indices(n_samples))
    !     allocate(neighbors%distances(n_samples))
    !     do i=1, n_samples, 1
    !         allocate(neighbors%indices(i)%idx(n_neighbors))
    !         allocate(neighbors%distances(i)%dst(n_neighbors))
    !     end do
    !     allocate(q_vals(n_columns))
    !     allocate(nearest_dsts(n_neighbors))
    !     allocate(nearest_idxs(n_neighbors))

    !     !$omp parallel num_threads(8)
    !     !$omp do private(leaf_node_ptr, nearest_dsts, nearest_idxs, i, node_ptr, fid, val, q_vals, q_idx, q_sq, max_radius)
    !     do q_idx=1, n_samples, 1
    !         q_vals = q_ptr(q_idx,:)
    !         q_sq = sum(q_vals**2d0)
    !         nearest_dsts = huge(0d0)
    !         nearest_idxs = -2
    !         call this%query_new_T_nearest_n_neighbors_rec_get_leaf_ptr(this%root_node_ptr_, q_vals, q_sq, & 
    !             leaf_node_ptr, nearest_dsts, nearest_idxs, n_columns, n_neighbors)

    !         node_ptr => leaf_node_ptr
    !         do while (t_)
    !             fid = node_ptr%node_p_ptr%split_fid
    !             val = node_ptr%node_p_ptr%split_val
    !             max_radius = nearest_dsts(n_neighbors)

    !             if ( (q_vals(fid)-val)**2d0 < max_radius ) then
    !                 if (node_ptr%is_left) then
    !                     call this%query_new_T_nearest_n_neighbors_search_subtree(node_ptr%node_p_ptr%node_r_ptr, &
    !                         q_vals, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !                 else
    !                     call this%query_new_T_nearest_n_neighbors_search_subtree(node_ptr%node_p_ptr%node_l_ptr, &
    !                         q_vals, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !                 end if
    !             end if
    !             node_ptr => node_ptr%node_p_ptr
    !             if (.not. ASSOCIATED(node_ptr%node_p_ptr)) exit
    !         end do
    !         call quick_argsort(nearest_dsts, nearest_idxs, size(nearest_idxs, kind=8))
    !         neighbors%indices(q_idx)%idx(1:n_neighbors)   = nearest_idxs
    !         neighbors%distances(q_idx)%dst(1:n_neighbors) = sqrt(relu(nearest_dsts))
    !         ! neighbors%distances(q_idx)%dst(1:n_neighbors) = sqrt(relu(nearest_dsts))
    !     end do
    !     !$omp end do
    !     !$omp end parallel 
    ! end function query_new_T_nearest_n_neighbors


    ! !> Get Leaf Node pointer of Query point
    ! !! \param root_node_ptr pointer to root node
    ! !! \param q a coordinate of query_new_T point
    ! !! \param q_sq a square norm of query_new_T point
    ! !! \param leaf_ptr pointer to leaf node of 'q' to return
    ! !! \param nearest_dsts nearest distances in radius
    ! !! \param nearest_idxs nearest point indices in radius
    ! !! \param n_columns number of columns of q
    ! !! \param n_neighbors number of neighbors to return
    ! recursive subroutine query_new_T_nearest_n_neighbors_rec_get_leaf_ptr(this, root_node_ptr, q, q_sq, leaf_ptr, &
    !     nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !     implicit none
    !     class(kdtree)            :: this
    !     type(node_type), pointer :: root_node_ptr
    !     real(kind=8), intent(in) :: q(n_columns)
    !     real(kind=8), intent(in) :: q_sq
    !     type(node_type), pointer :: leaf_ptr
    !     real(kind=8), intent(inout) :: nearest_dsts(n_neighbors)
    !     integer(kind=8), intent(inout) :: nearest_idxs(n_neighbors)
    !     integer(kind=8), intent(in)    :: n_columns, n_neighbors

    !     real(kind=8) :: f_val, dist, dist_new, dist_old, idx_new, idx_old, dist_q, dist_from_q_to_x
    !     integer(kind=8) :: i, j, idx, x_idx, f_idx, n_samples_l, n_samples_r, idx_l, idx_r
    !     integer(kind=8), allocatable :: indices_l(:), indices_r(:), indices(:), tmp_i(:)
    !     logical(kind=4), allocatable :: tmp_l(:)
    !     logical(kind=4) :: goto_left, check_dup
    !     real(kind=8), allocatable :: XxQ(:), tmp_d(:)
    !     integer(kind=8) :: n
    !     integer(kind=8), save :: count_comp_dist = 0
        
    !     if (root_node_ptr%is_leaf) then
    !         ! Search Closest Point from Leaf Node
    !         leaf_ptr => root_node_ptr
    !         allocate(XxQ(leaf_ptr%n_samples_))
    !         allocate(indices(leaf_ptr%n_samples_))
    !         call openblas_set_num_threads(1)
    !         call dgemv("T", n_columns, root_node_ptr%n_samples_, &
    !             1d0, root_node_ptr%xt_, n_columns, &
    !             q, 1_8, 0d0, XxQ, 1_8)
    !         XxQ(:) = q_sq + leaf_ptr%x_sq_(:) - 2d0*XxQ(:)
    !         if ( nearest_dsts(n_neighbors) < minval(XxQ) ) then
    !             !  skip
    !         else
    !             do i=1, leaf_ptr%n_samples_
    !                 indices(i) = leaf_ptr%indices_(i)
    !             end do
    !             tmp_d = [XxQ, nearest_dsts]
    !             tmp_i = [indices, nearest_idxs]
    !             n = minval((/size(tmp_d)+0_8, n_neighbors/))
    !             call quick_argsort(tmp_d, tmp_i, size(tmp_d, kind=8_8))
    !             nearest_dsts(1:n) = tmp_d(1:n)
    !             nearest_idxs(1:n) = tmp_i(1:n)
    !         end if
    !         return
    !     end if

    !     f_idx = root_node_ptr%split_fid
    !     f_val = root_node_ptr%split_val

    !     goto_left = q(f_idx) <= f_val

    !     if (goto_left) then
    !         call query_new_T_nearest_n_neighbors_rec_get_leaf_ptr(this, root_node_ptr%node_l_ptr, q, q_sq, leaf_ptr, &
    !             nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !     else
    !         call query_new_T_nearest_n_neighbors_rec_get_leaf_ptr(this, root_node_ptr%node_r_ptr, q, q_sq, leaf_ptr, &
    !             nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !     end if
    ! end subroutine query_new_T_nearest_n_neighbors_rec_get_leaf_ptr


    ! !> Get nearest data point of Query point from subtree
    ! !! \param subtree_root_node_ptr pointer to root node of subtree to search
    ! !! \param q a coordinate of query_new_T point
    ! !! \param q_sq a square norm of query_new_T point
    ! !! \param nearest_dsts nearest distances in radius
    ! !! \param nearest_idxs nearest point indices in radius
    ! !! \param n_columns number of columns of q
    ! !! \param n_neighbors number of neighbors to return 
    ! recursive subroutine query_new_T_nearest_n_neighbors_search_subtree(this, subtree_root_node_ptr, q, q_sq, &
    !     nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !     implicit none
    !     class(kdtree) :: this
    !     type(node_type), pointer       :: subtree_root_node_ptr
    !     real(kind=8), intent(in)       :: q(n_columns), q_sq
    !     integer(kind=8), intent(inout) :: nearest_idxs(n_neighbors)
    !     real(kind=8), intent(inout)    :: nearest_dsts(n_neighbors)
    !     integer(kind=8), intent(in)    :: n_columns, n_neighbors

    !     integer(kind=8) :: i, j, idx_old, idx_new, x_idx, fid, n, n_tmp
    !     real(kind=8) :: dist_old, dist_new, dist, dist_from_q_to_plane, val_x, val_q, dist_from_q_to_x
    !     real(kind=8) :: max_radius
    !     real(kind=8), allocatable :: XxQ(:), tmp_d(:)
    !     integer(kind=8), allocatable :: tmp_i(:), indices(:)
    !     logical(kind=4) :: check_dup

    !     if (subtree_root_node_ptr%is_leaf) then
    !         ! Search Closest Point from Leaf Node
    !         allocate(XxQ(subtree_root_node_ptr%n_samples_))
    !         allocate(indices(subtree_root_node_ptr%n_samples_))
    !         call openblas_set_num_threads(1)
    !         call dgemv("T", n_columns, subtree_root_node_ptr%n_samples_, &
    !             1d0, subtree_root_node_ptr%xt_, n_columns, &
    !             q, 1_8, 0d0, XxQ, 1_8)
    !         XxQ(:) = q_sq + subtree_root_node_ptr%x_sq_(:) - 2d0*XxQ(:)
    !         if ( nearest_dsts(n_neighbors) < minval(XxQ) ) then
    !             ! skip
    !         else
    !             do i=1, subtree_root_node_ptr%n_samples_
    !                 indices(i) = subtree_root_node_ptr%indices_(i)
    !             end do
    !             tmp_d = [XxQ, nearest_dsts]
    !             tmp_i = [indices, nearest_idxs]
    !             n = minval((/size(tmp_d)+0_8, n_neighbors/))
    !             call quick_argsort(tmp_d, tmp_i, size(tmp_d, kind=8))
    !             ! check_dup = check_duplication(tmp_i, size(tmp_i, kind=8))
    !             ! print*, "other leaf node duplication check: ", check_dup
    !             nearest_dsts(1:n) = tmp_d(1:n)
    !             nearest_idxs(1:n) = tmp_i(1:n)
    !         end if
    !         return
    !     else
    !         ! Compute Distance From Q to Split Plane
    !         fid = subtree_root_node_ptr%split_fid
    !         val_x = subtree_root_node_ptr%split_val
    !         val_q = q(fid)
    !         dist_from_q_to_plane = (val_q - val_x)**2d0
    !         max_radius = nearest_dsts(n_neighbors)

    !         if (max_radius < dist_from_q_to_plane) then
    !             if (val_q <= val_x) then
    !                 call this%query_new_T_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_l_ptr, &
    !                     q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !             else
    !                 call this%query_new_T_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_r_ptr, &
    !                     q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !             end if
    !         else
    !             call this%query_new_T_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_l_ptr, &
    !                 q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !             call this%query_new_T_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_r_ptr, &
    !                 q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !         end if
    !     end if
    ! end subroutine query_new_T_nearest_n_neighbors_search_subtree



















    ! !> Get Neighbors . Euclid distance Only.
    ! !! \param q query points
    ! !! \param n_neighbors number of nearest neighbors.
    ! !! \param radius ball raius size
    ! function query_red(this, q, n_neighbors, radius)
    !     implicit none
    !     class(kdtree)                         :: this
    !     type(neighbor_results)                  :: query_red
    !     real(kind=8), target, intent(in)      :: q(:,:)
    !     integer(kind=8), optional, intent(in) :: n_neighbors
    !     real(kind=8), optional, intent(in)    :: radius

    !     real(kind=8), pointer      :: q_ptr(:,:)
    !     real(kind=8) :: radius_sq

    !     if (present(n_neighbors) .and. present(radius)) then
    !         stop "Only one of 'n_neighbors' or 'radius' can be specified."
    !     end if

    !     q_ptr => q

    !     if (present(n_neighbors)) then
    !         if (n_neighbors < 1_8) then
    !             stop "'n_neighbors' must be greater equal 1."
    !         else
    !             if (this%use_brute_force) then
    !                 query_red = this%bfs%query(q_ptr, n_neighbors=n_neighbors)
    !             else
    !                 query_red = this%query_red_nearest_n_neighbors(q_ptr, n_neighbors)
    !             end if
    !         end if
    !     end if
    ! end function query_red


    ! !> Get N nearest neighbors
    ! !! \param q_ptr pointer to query points
    ! !! \param n_neighbors number of neighbors to return
    ! function query_red_nearest_n_neighbors(this, q_ptr, n_neighbors) result(neighbors)
    !     implicit none
    !     type(neighbor_results)     :: neighbors
    !     class(kdtree)            :: this
    !     real(kind=8), pointer, intent(in) :: q_ptr(:,:)
    !     integer(kind=8), intent(in) :: n_neighbors

    !     integer(kind=8)       :: n_samples, n_columns, q_shape(2)

    !     real(kind=8)                 :: val, max_radius, q_sq
    !     integer(kind=8)              :: i, nearest_idx, fid, q_idx
    !     integer(kind=8), allocatable :: indices(:), nearest_idxs(:)
    !     real(kind=8), allocatable :: nearest_dsts(:), q_vals(:), q_sq_sum(:)
    !     type(node_type), target :: root_node
    !     type(node_type), pointer :: root_node_ptr, node_ptr, leaf_node_ptr

    !     integer(kind=8) :: date_value1(8), date_value2(8)
    !     integer(kind=8), save :: time_leaf, t_get_leaf, time_internal, t_search_best

    !     ! print*, 1
    !     q_shape = shape(q_ptr)
    !     n_samples = q_shape(1)
    !     n_columns = q_shape(2)
    !     call ifdealloc(this%q_sq_sum)
    !     allocate(this%q_sq_sum(n_samples))
    !     call matrix_sqsum_row(q_ptr, this%q_sq_sum, n_samples, n_columns, parallel=f_)

    !     ! print*, 2
    !     if ( allocated(neighbors%indices) ) &
    !         deallocate(neighbors%indices)
    !     if ( allocated(neighbors%distances) ) &
    !         deallocate(neighbors%distances)
    !     allocate(neighbors%indices(n_samples))
    !     allocate(neighbors%distances(n_samples))
    !     do i=1, n_samples, 1
    !         allocate(neighbors%indices(i)%idx(n_neighbors))
    !         allocate(neighbors%distances(i)%dst(n_neighbors))
    !     end do
    !     allocate(q_vals(n_columns))
    !     allocate(nearest_dsts(n_neighbors))
    !     allocate(nearest_idxs(n_neighbors))

    !     !$omp parallel num_threads(8)
    !     !$omp do private(leaf_node_ptr, nearest_dsts, nearest_idxs, i, node_ptr, fid, val, q_vals, q_idx, q_sq, max_radius)
    !     do q_idx=1, n_samples, 1
    !         q_vals = q_ptr(q_idx,:)
    !         q_sq = this%q_sq_sum(q_idx)
    !         nearest_dsts = huge(0d0)
    !         nearest_idxs = -2
    !         call this%query_red_nearest_n_neighbors_rec_get_leaf_ptr(this%root_node_ptr_, q_vals, q_sq, & 
    !             leaf_node_ptr, nearest_dsts, nearest_idxs, n_columns, n_neighbors)

    !         node_ptr => leaf_node_ptr
    !         do while (t_)
    !             fid = node_ptr%node_p_ptr%split_fid
    !             val = node_ptr%node_p_ptr%split_val
    !             max_radius = maxval(nearest_dsts)

    !             if ( (q_vals(fid)-val)**2d0 < max_radius ) then
    !                 if (node_ptr%is_left) then
    !                     call this%query_red_nearest_n_neighbors_search_subtree(node_ptr%node_p_ptr%node_r_ptr, &
    !                         q_vals, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !                 else
    !                     call this%query_red_nearest_n_neighbors_search_subtree(node_ptr%node_p_ptr%node_l_ptr, &
    !                         q_vals, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !                 end if
    !             end if
    !             node_ptr => node_ptr%node_p_ptr
    !             if (.not. ASSOCIATED(node_ptr%node_p_ptr)) exit
    !         end do
    !         call quick_argsort(nearest_dsts, nearest_idxs, size(nearest_idxs, kind=8))
    !         neighbors%indices(q_idx)%idx(1:n_neighbors)   = nearest_idxs
    !         neighbors%distances(q_idx)%dst(1:n_neighbors) = sqrt(relu(nearest_dsts))
    !         ! neighbors%distances(q_idx)%dst(1:n_neighbors) = sqrt(relu(nearest_dsts))
    !     end do
    !     !$omp end do
    !     !$omp end parallel 
    ! end function query_red_nearest_n_neighbors


    ! !> Get Leaf Node pointer of Query point
    ! !! \param root_node_ptr pointer to root node
    ! !! \param q a coordinate of query_red point
    ! !! \param q_sq a square norm of query_red point
    ! !! \param leaf_ptr pointer to leaf node of 'q' to return
    ! !! \param nearest_dsts nearest distances in radius
    ! !! \param nearest_idxs nearest point indices in radius
    ! !! \param n_columns number of columns of q
    ! !! \param n_neighbors number of neighbors to return
    ! recursive subroutine query_red_nearest_n_neighbors_rec_get_leaf_ptr(this, root_node_ptr, q, q_sq, leaf_ptr, &
    !     nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !     implicit none
    !     class(kdtree)            :: this
    !     type(node_type), pointer :: root_node_ptr
    !     real(kind=8), intent(in) :: q(n_columns)
    !     real(kind=8), intent(in) :: q_sq
    !     type(node_type), pointer :: leaf_ptr
    !     real(kind=8), intent(inout) :: nearest_dsts(n_neighbors)
    !     integer(kind=8), intent(inout) :: nearest_idxs(n_neighbors)
    !     integer(kind=8), intent(in)    :: n_columns, n_neighbors

    !     real(kind=8) :: f_val, dist, dist_new, dist_old, idx_new, idx_old, dist_q, dist_from_q_to_x
    !     integer(kind=8) :: i, j, idx, x_idx, f_idx, n_samples_l, n_samples_r, idx_l, idx_r
    !     integer(kind=8), allocatable :: indices_l(:), indices_r(:), indices(:), tmp_i(:)
    !     logical(kind=4), allocatable :: tmp_l(:)
    !     logical(kind=4) :: goto_left, check_dup
    !     real(kind=8), allocatable :: XxQ(:), tmp_d(:)
    !     integer(kind=8) :: n
    !     integer(kind=8), save :: count_comp_dist = 0

    !     if (root_node_ptr%is_leaf) then
    !         ! Search Closest Point from Leaf Node
    !         leaf_ptr => root_node_ptr
    !         allocate(XxQ(leaf_ptr%n_samples_))
    !         allocate(indices(leaf_ptr%n_samples_))
    !         call openblas_set_num_threads(1)
    !         call dgemv("N", root_node_ptr%n_samples_, n_columns, &
    !             1d0, root_node_ptr%x_, root_node_ptr%n_samples_, &
    !             q, 1_8, 0d0, XxQ, 1_8)
    !         XxQ(:) = q_sq + leaf_ptr%x_sq_(:) - 2d0*XxQ(:)
    !         if ( nearest_dsts(n_neighbors) < minval(XxQ) ) then
    !             !  skip
    !         else
    !             do i=1, leaf_ptr%n_samples_
    !                 indices(i) = leaf_ptr%indices_(i)
    !             end do
    !             tmp_d = [XxQ, nearest_dsts]
    !             tmp_i = [indices, nearest_idxs]
    !             n = minval((/size(tmp_d)+0_8, n_neighbors/))
    !             call quick_argsort(tmp_d, tmp_i, size(tmp_d, kind=8_8))
    !             nearest_dsts(1:n) = tmp_d(1:n)
    !             nearest_idxs(1:n) = tmp_i(1:n)
    !         end if
    !         return
    !     end if        
    !     ! if (root_node_ptr%is_leaf) then
    !     !     ! Search Closest Point from Leaf Node
    !     !     leaf_ptr => root_node_ptr
    !     !     dist = sum((q - leaf_ptr%center)**2d0)

    !     !     if (dist < maxval(nearest_dsts) + leaf_ptr%radius) then
    !     !         allocate(XxQ(leaf_ptr%n_samples_))
    !     !         allocate(indices(leaf_ptr%n_samples_))
    !     !         call multi_mat_vec(leaf_ptr%x_, q, XxQ, &
    !     !             leaf_ptr%n_samples_, n_columns, f_)
    !     !         ! count_comp_dist = count_comp_dist + 1
    !     !         XxQ(:) = q_sq + leaf_ptr%x_sq_(:) - 2d0*XxQ(:)
    !     !         do i=1, leaf_ptr%n_samples_
    !     !             indices(i) = leaf_ptr%indices_(i)
    !     !         end do
    !     !         tmp_d = [XxQ, nearest_dsts]
    !     !         tmp_i = [indices, nearest_idxs]
    !     !         n = minval((/size(tmp_d)+0_8, n_neighbors/))
    !     !         call quick_argsort(tmp_d, tmp_i, size(tmp_d, kind=8_8))
    !     !         nearest_dsts(1:n) = tmp_d(1:n)
    !     !         nearest_idxs(1:n) = tmp_i(1:n)
    !     !     end if
    !     !     return
    !     ! end if

    !     f_idx = root_node_ptr%split_fid
    !     f_val = root_node_ptr%split_val

    !     goto_left = q(f_idx) <= f_val

    !     if (goto_left) then
    !         call query_red_nearest_n_neighbors_rec_get_leaf_ptr(this, root_node_ptr%node_l_ptr, q, q_sq, leaf_ptr, &
    !             nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !     else
    !         call query_red_nearest_n_neighbors_rec_get_leaf_ptr(this, root_node_ptr%node_r_ptr, q, q_sq, leaf_ptr, &
    !             nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !     end if
    ! end subroutine query_red_nearest_n_neighbors_rec_get_leaf_ptr


    ! !> Get nearest data point of Query point from subtree
    ! !! \param subtree_root_node_ptr pointer to root node of subtree to search
    ! !! \param q a coordinate of query_red point
    ! !! \param q_sq a square norm of query_red point
    ! !! \param nearest_dsts nearest distances in radius
    ! !! \param nearest_idxs nearest point indices in radius
    ! !! \param n_columns number of columns of q
    ! !! \param n_neighbors number of neighbors to return
    ! recursive subroutine query_red_nearest_n_neighbors_search_subtree(this, subtree_root_node_ptr, q, q_sq, &
    !     nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !     implicit none
    !     class(kdtree) :: this
    !     type(node_type), pointer       :: subtree_root_node_ptr
    !     real(kind=8), intent(in)       :: q(n_columns), q_sq
    !     integer(kind=8), intent(inout) :: nearest_idxs(n_neighbors)
    !     real(kind=8), intent(inout)    :: nearest_dsts(n_neighbors)
    !     integer(kind=8), intent(in)    :: n_columns, n_neighbors

    !     integer(kind=8) :: i, j, idx_old, idx_new, x_idx, fid, n, n_tmp
    !     real(kind=8) :: dist_old, dist_new, dist, dist_from_q_to_plane, val_x, val_q, dist_from_q_to_x
    !     real(kind=8) :: max_radius
    !     real(kind=8), allocatable :: XxQ(:), tmp_d(:)
    !     integer(kind=8), allocatable :: tmp_i(:), indices(:)
    !     logical(kind=4) :: check_dup
    !     integer(kind=8), save :: count_skip=0, count_all=0
    !     type(node_type), pointer :: leaf_ptr
    !     real(kind=8) :: dist_from_q_to_center

    !     dist_from_q_to_center = sum( (q-subtree_root_node_ptr%center)**2d0 ) ** 0.5d0
    !     if (dist_from_q_to_center > nearest_dsts(n_neighbors) ** 0.5d0 + subtree_root_node_ptr%radius) return

    !     if (subtree_root_node_ptr%is_leaf) then
    !         ! Search Closest Point from Leaf Node
    !         allocate(XxQ(subtree_root_node_ptr%n_samples_))
    !         allocate(indices(subtree_root_node_ptr%n_samples_))
    !         call dgemv("T", n_columns, subtree_root_node_ptr%n_samples_, &
    !             -2d0, subtree_root_node_ptr%xt_, n_columns, &
    !             q, 1_8, 0d0, XxQ, 1_8)
    !         ! call multi_mat_vec_t(subtree_root_node_ptr%xt_, q, XxQ, &
    !         !     leaf_ptr%n_samples_, n_columns)
    !         XxQ(:) = q_sq + subtree_root_node_ptr%x_sq_(:) + XxQ(:)
    !         if ( nearest_dsts(n_neighbors) < minval(XxQ) ) then
    !             ! skip
    !         else
    !             do i=1, subtree_root_node_ptr%n_samples_
    !                 indices(i) = subtree_root_node_ptr%indices_(i)
    !             end do
    !             tmp_d = [XxQ, nearest_dsts]
    !             tmp_i = [indices, nearest_idxs]
    !             n = minval((/size(tmp_d)+0_8, n_neighbors/))
    !             ! call quick_argsort(tmp_d, tmp_i, size(tmp_d, kind=8))
    !             call insertion_argsort(tmp_d, tmp_i, size(tmp_d, kind=8))
    !             ! check_dup = check_duplication(tmp_i, size(tmp_i, kind=8))
    !             ! print*, "other leaf node duplication check: ", check_dup
    !             nearest_dsts(1:n) = tmp_d(1:n)
    !             nearest_idxs(1:n) = tmp_i(1:n)
    !         end if
    !         return
    !     ! if (subtree_root_node_ptr%is_leaf) then
    !     !     ! Search Closest Point from Leaf Node
    !     !     leaf_ptr => subtree_root_node_ptr
    !     !     dist = sum((q - leaf_ptr%center)**2d0)
    !     !     count_all = count_all + 1
    !     !     if (dist < maxval(nearest_dsts) + leaf_ptr%radius) then
    !     !         count_skip = count_skip + 1
    !     !         allocate(XxQ(leaf_ptr%n_samples_))
    !     !         allocate(indices(leaf_ptr%n_samples_))
    !     !         call multi_mat_vec(leaf_ptr%x_, q, XxQ, &
    !     !             leaf_ptr%n_samples_, n_columns, f_)
    !     !         ! count_comp_dist = count_comp_dist + 1
    !     !         XxQ(:) = q_sq + leaf_ptr%x_sq_(:) - 2d0*XxQ(:)
    !     !         if (minval(XxQ) > nearest_dsts(n_neighbors)) return
    !     !         do i=1, leaf_ptr%n_samples_
    !     !             indices(i) = leaf_ptr%indices_(i)
    !     !         end do
    !     !         tmp_d = [XxQ, nearest_dsts]
    !     !         tmp_i = [indices, nearest_idxs]
    !     !         n = minval((/size(tmp_d)+0_8, n_neighbors/))
    !     !         call quick_argsort(tmp_d, tmp_i, size(tmp_d, kind=8_8))
    !     !         nearest_dsts(1:n) = tmp_d(1:n)
    !     !         nearest_idxs(1:n) = tmp_i(1:n)
    !     !     end if
    !     !     ! print*, "  ---- ", count_skip, count_all, count_skip/dble(count_all)*100d0
    !     !     return
    !     else
    !         ! Compute Distance From Q to Split Plane
    !         fid = subtree_root_node_ptr%split_fid
    !         val_x = subtree_root_node_ptr%split_val
    !         val_q = q(fid)
    !         dist_from_q_to_plane = (val_q - val_x)**2d0
    !         ! dist_from_q_to_center = sum( (q-subtree_root_node_ptr%center)**2d0 )
    !         max_radius = maxval(nearest_dsts)

    !         if (max_radius < dist_from_q_to_plane) then
    !             if (val_q <= val_x) then
    !                 call this%query_red_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_l_ptr, &
    !                     q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !             else
    !                 call this%query_red_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_r_ptr, &
    !                     q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !             end if
    !         else
    !             if (val_q <= val_x) then
    !                 call this%query_red_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_l_ptr, &
    !                     q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !                 call this%query_red_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_r_ptr, &
    !                     q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !             else
    !                 call this%query_red_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_r_ptr, &
    !                     q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !                 call this%query_red_nearest_n_neighbors_search_subtree(subtree_root_node_ptr%node_l_ptr, &
    !                     q, q_sq, nearest_dsts, nearest_idxs, n_columns, n_neighbors)
    !             end if
    !         end if
    !     end if
    ! end subroutine query_red_nearest_n_neighbors_search_subtree


    ! subroutine multi_mat_vec_t(xt, v_in, v_out, n_rows, n_cols)
    !     implicit none
    !     real(kind=8), intent(in) :: xt(n_cols, n_rows)
    !     real(kind=8), intent(in) :: v_in(n_cols)
    !     real(kind=8), intent(inout) :: v_out(n_rows)
    !     integer(kind=8), intent(in) :: n_cols, n_rows

    !     integer(kind=8) :: c, r, c_unroll, u
    !     real(kind=8) :: tmp1, tmp2
    !     real(kind=8), allocatable :: xt_c(:)

    !     c_unroll = c / dble(8)

    !     allocate(xt_c(n_cols))
    !     ! do r=1, n_rows, 1
    !     !     tmp1 = 0d0
    !     !     tmp2 = 0d0
    !     !     xt_c = xt(:,r)
    !     !     do c=1, c_unroll, 8
    !     !         tmp1 = tmp1 + xt_c(c+0)*v_in(c+0)
    !     !         tmp2 = tmp2 + xt_c(c+1)*v_in(c+1)
    !     !         tmp1 = tmp1 + xt_c(c+2)*v_in(c+2)
    !     !         tmp2 = tmp2 + xt_c(c+3)*v_in(c+3)
    !     !         tmp1 = tmp1 + xt_c(c+4)*v_in(c+4)
    !     !         tmp2 = tmp2 + xt_c(c+5)*v_in(c+5)
    !     !         tmp1 = tmp1 + xt_c(c+6)*v_in(c+6)
    !     !         tmp2 = tmp2 + xt_c(c+7)*v_in(c+7)
    !     !     end do
            
    !     !     do u=c_unroll+1, n_cols, 1
    !     !         tmp2 = tmp2 + xt(u,r)*v_in(u)
    !     !     end do
    !     !     v_out(r) = tmp1+tmp2
    !     ! end do
    !     do r=1, n_rows, 1
    !         v_out(r) = dot_product(xt(:,r), v_in)
    !     end do
    ! end subroutine multi_mat_vec_t




    ! !> Get Neighbors . Euclid distance Only.
    ! !! \param q query points
    ! !! \param n_neighbors number of nearest neighbors.
    ! !! \param radius ball raius size
    ! function query_rec(this, q, n_neighbors, radius)
    !     implicit none
    !     class(kdtree)                         :: this
    !     type(neighbor_results)                  :: query_rec
    !     real(kind=8), target, intent(in)      :: q(:,:)
    !     integer(kind=8), optional, intent(in) :: n_neighbors
    !     real(kind=8), optional, intent(in)    :: radius

    !     real(kind=8), pointer      :: q_ptr(:,:)
    !     real(kind=8) :: radius_sq

    !     if (present(n_neighbors) .and. present(radius)) then
    !         stop "Only one of 'n_neighbors' or 'radius' can be specified."
    !     end if

    !     q_ptr => q

    !     if (present(n_neighbors)) then
    !         if (n_neighbors < 1_8) then
    !             stop "'n_neighbors' must be greater equal 1."
    !         else
    !             if (this%use_brute_force) then
    !                 query_rec = this%bfs%query(q_ptr, n_neighbors=n_neighbors)
    !             else
    !                 query_rec = this%query_rec_nearest_n_neighbors(q_ptr, n_neighbors)
    !             end if
    !         end if
    !     end if
    ! end function query_rec


    ! function query_rec_nearest_n_neighbors(this, q_ptr, n_neighbors) result(neighbors)
    !     implicit none
    !     class(kdtree) :: this
    !     real(kind=8), pointer, intent(in) :: q_ptr(:,:)
    !     integer(kind=8), intent(in) :: n_neighbors
    !     type(neighbor_results) :: neighbors

    !     integer(kind=8) :: i, n_samples, n_columns, j
    !     real(kind=8), allocatable :: q(:)
    !     real(kind=8) :: q_sq
    !     real(kind=8), allocatable    :: nearest_dsts(:)
    !     integer(kind=8), allocatable :: nearest_idxs(:)

    !     type(node_type), pointer :: root_node_ptr_
    !     real(kind=8) :: dist_sq
    !     real(kind=8), allocatable :: feat_dists(:)

    !     n_samples = size(q_ptr, dim=1)
    !     n_columns = size(q_ptr, dim=2)

    !     call ifdealloc(this%q_sq_sum)
    !     allocate(this%q_sq_sum(n_samples))
    !     call matrix_sqsum_row(q_ptr, this%q_sq_sum, n_samples, n_columns, parallel=f_)

    !     if ( allocated(neighbors%indices) )   deallocate(neighbors%indices)
    !     if ( allocated(neighbors%distances) ) deallocate(neighbors%distances)
    !     allocate(neighbors%indices(n_samples))
    !     allocate(neighbors%distances(n_samples))
    !     do i=1, n_samples, 1
    !         allocate(neighbors%indices(i)%idx(n_neighbors)); neighbors%indices(i)%idx = -1
    !         allocate(neighbors%distances(i)%dst(n_neighbors)); neighbors%distances(i)%dst = huge(0d0)
    !     end do

    !     allocate(q(n_columns))
    !     allocate(feat_dists(n_columns))
    !     allocate(nearest_dsts(n_neighbors))
    !     allocate(nearest_idxs(n_neighbors))

    !     !$omp parallel num_threads(8)
    !     !$omp do private(j, q, q_sq, dist_sq, feat_dists, nearest_dsts, nearest_idxs)
    !     do j=1, n_samples, 1
    !         q = q_ptr(j,:)
    !         q_sq = this%q_sq_sum(j)
    !         dist_sq = this%root_node_ptr_%comp_dist_rect_to_query(q, n_columns)
    !         feat_dists = 0d0
    !         nearest_dsts = huge(0d0)
    !         nearest_idxs = -1
    !         call search_nearests_n_neighbors(q, q_sq, dist_sq, feat_dists, this%root_node_ptr_, &
    !                 nearest_dsts, nearest_idxs, n_neighbors)
    !         neighbors%indices(j)%idx   = nearest_idxs
    !         neighbors%distances(j)%dst = sqrt(relu(nearest_dsts))
    !     end do
    !     !$omp end do
    !     !$omp end parallel

    ! end function query_rec_nearest_n_neighbors


    ! recursive subroutine search_nearests_n_neighbors(q, q_sq, dist_sq, feat_dists, root_node_ptr, &
    !     nearest_dsts, nearest_idxs, n_neighbors)
    !     implicit none
    !     real(kind=8), intent(in) :: q(:), q_sq
    !     real(kind=8), intent(inout) :: dist_sq, feat_dists(:)
    !     type(node_type), pointer, intent(in) :: root_node_ptr
    !     real(kind=8), intent(inout) :: nearest_dsts(n_neighbors)
    !     integer(kind=8), intent(inout) :: nearest_idxs(n_neighbors)
    !     integer(kind=8), intent(in) :: n_neighbors

    !     real(kind=8), allocatable :: x2q(:), tmp_d(:)
    !     real(kind=8) :: q_val, s_val, dist
    !     integer(kind=8) :: n_rows_, n_cols_, fid, n, max_loc
    !     type(node_type), pointer :: node_near, node_far
    !     integer(kind=8), allocatable :: tmp_i(:)

    !     real(kind=8) :: max_dist, diff1, diff2, cut_dist
    !     logical(kind=4) :: is_update

    !     if (root_node_ptr%is_leaf) then
    !         n_rows_ = root_node_ptr%n_samples_
    !         n_cols_ = root_node_ptr%n_columns
    !         max_dist = nearest_dsts(n_neighbors)
    !         max_loc = n_neighbors
    !         is_update = f_
    !         do n=1, n_rows_, 1
    !             dist =  sum((root_node_ptr%xt_(:,n) - q)**2d0)
    !             if (dist >= max_dist) cycle
    !             max_dist = dist
    !             nearest_dsts(max_loc) = max_dist
    !             nearest_idxs(max_loc) = root_node_ptr%indices_(n)
    !             max_loc = maxloc(nearest_dsts, dim=1)
    !             is_update = t_
    !         end do
    !         if (is_update) call quick_argsort(nearest_dsts, nearest_idxs, n_neighbors)
    !         return
    !     end if

    !     fid = root_node_ptr%split_fid
    !     q_val = q(fid)
    !     diff1 = q_val - root_node_ptr%node_l_ptr%f_max(fid)
    !     diff2 = q_val - root_node_ptr%node_r_ptr%f_min(fid)

    !     if (diff1+diff2<0d0) then
    !         node_near => root_node_ptr%node_l_ptr
    !         node_far  => root_node_ptr%node_r_ptr
    !         cut_dist  = (q_val - root_node_ptr%node_l_ptr%f_max(fid))**2d0
    !     else
    !         node_near => root_node_ptr%node_r_ptr
    !         node_far  => root_node_ptr%node_l_ptr
    !         cut_dist  = (q_val - root_node_ptr%node_r_ptr%f_min(fid))**2d0
    !     end if
    !     call search_nearests_n_neighbors(q, q_sq, dist_sq, feat_dists, node_near, &
    !             nearest_dsts, nearest_idxs, n_neighbors)

    !     dist = feat_dists(fid)
    !     dist_sq = dist_sq + cut_dist - dist
    !     feat_dists(fid) = cut_dist
    !     if (dist_sq <= nearest_dsts(n_neighbors)) then
    !         call search_nearests_n_neighbors(q, q_sq, dist_sq, feat_dists, node_far, &
    !                 nearest_dsts, nearest_idxs, n_neighbors)
    !     end if
    !     feat_dists(fid) = dist
    ! end subroutine search_nearests_n_neighbors
    




end module mod_kdtree
