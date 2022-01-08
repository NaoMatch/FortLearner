module mod_exact_duplicate_search
    !$ use omp_lib
    use mod_nearest_neighbour
    use mod_random
    use mod_hash
    use mod_common
    use mod_common_type
    use mod_linalg, only: multi_mat_vec
    implicit none

    type node_eds
        integer(kind=8) :: idx !< node index
        logical(kind=4) :: is_root=f_ !< is root node or not
        logical(kind=4) :: is_leaf=f_ !< is leaf node or not
        logical(kind=4) :: is_left=f_ !< is left child node or not
        integer(kind=8) :: depth !< node depth
        integer(kind=8) :: n_samples !< number of samples
        integer(kind=8) :: n_columns !< number of samples
        integer(kind=8) :: min_samples_in_leaf !< minimum number of saples in leaf

        integer(kind=8) :: split_val_i8 = huge(0_8) !< split threshold, for 'exact_dup_search'
        real(kind=8)    :: split_val_r8 = huge(0d0) !< split threshold
        integer(kind=8) :: split_fid    = -1_8 !< split feature index

        integer(kind=8), ALLOCATABLE :: indices(:) !< sample point indices

        type(node_eds), pointer :: node_p_ptr => null() !< pointer to parent node
        type(node_eds), pointer :: node_l_ptr => null() !< pointer to child left node
        type(node_eds), pointer :: node_r_ptr => null() !< pointer to child right node
    contains
        procedure :: info => info_node_eds
        procedure :: init => init_node_eds
        procedure :: adopting_twins_in_eds
    end type node_eds

    type node_eds_pointer
        type(node_eds), pointer :: ptr
    end type node_eds_pointer

    type, extends(jagged_vector_i8) ::  duplicate_index
    end type duplicate_index

    type exact_duplicate_search
        integer(kind=8) :: n_samples, n_columns
        integer(kind=8) :: min_samples_in_leaf=1
        type(node_eds), pointer :: root_node_ptr
    contains
        procedure :: init => init_exact_duplicate_search
        procedure :: search => search_exact_duplicate
        procedure :: build_exact_duplicate
        procedure :: build_exact_duplicate_rec
    end type exact_duplicate_search

    interface exact_duplicate_search
        module procedure :: new_exact_duplicate_search
    end interface exact_duplicate_search

    type hash_exact_duplicate_search
    contains
        procedure :: search => search_hash_exact_duplicate_search_matrix
    end type hash_exact_duplicate_search

    interface hash_exact_duplicate_search
        module procedure :: new_hash_exact_duplicate_search
    end interface hash_exact_duplicate_search

contains

    !> Create new 'hash_exact_duplicate_search' object
    function new_hash_exact_duplicate_search()
        implicit none
        type(hash_exact_duplicate_search) :: new_hash_exact_duplicate_search
    end function new_hash_exact_duplicate_search

    function search_hash_exact_duplicate_search_matrix(this, x)
        implicit none
        class(hash_exact_duplicate_search) :: this
        integer(kind=8), intent(in) :: x(:,:)
        type(duplicate_index), ALLOCATABLE, target  :: search_hash_exact_duplicate_search_matrix(:)
        type(duplicate_index), pointer :: res_ptr(:)
        integer(kind=8) :: n_samples, n_columns, x_shape(2), n_uniq
        integer(kind=8) :: i, l, ini, fin
        integer(kind=8), allocatable :: hashes(:), indices(:)

        x_shape = shape(x)
        n_samples = x_shape(1)
        n_columns = x_shape(2)

        allocate( hashes(n_samples) )
        allocate( indices(n_samples) )
        do i=1, n_samples, 1
            indices(i) = i
        end do
        hashes = one_at_a_time_hash(x, n_samples, n_columns)
        call quick_argsort(hashes, indices, n_samples)

        n_uniq = count_unique(hashes, n_samples)
        allocate( search_hash_exact_duplicate_search_matrix(n_uniq) )
        res_ptr => search_hash_exact_duplicate_search_matrix
        if ( n_uniq .eq. n_samples ) then
            do l=1, n_uniq, 1
                res_ptr(l)%vector = (/l/)
            end do
        else
            ini = 1
            do l=1, n_uniq-1, 1
                fin = ini
                do while (t_)
                    if (hashes(fin) .ne. hashes(fin+1)) exit
                    fin = fin+1
                end do
                ! call quick_sort(indices(ini:fin), fin-ini+1)
                res_ptr(l)%vector = indices(ini:fin)
                ini = fin + 1
            end do
            ! call quick_sort(indices(ini:n_samples), n_samples-ini+1)
            res_ptr(n_uniq)%vector = indices(ini:n_samples)
        end if
        ! print*, hashes(:10)
    end function search_hash_exact_duplicate_search_matrix

    ! -------------------------------------------------------------
    ! Exaxt Duplicate Search---------------------------------------
    ! -------------------------------------------------------------

    !> Create new 'exact_duplicate_search' object
    function new_exact_duplicate_search(min_samples_in_leaf)
        implicit none
        integer(kind=8), optional :: min_samples_in_leaf
        type(exact_duplicate_search) :: new_exact_duplicate_search
        if ( present(min_samples_in_leaf) )  new_exact_duplicate_search%min_samples_in_leaf = min_samples_in_leaf
    end function new_exact_duplicate_search


    !> Initialize 'exact_duplicate_search' object
    subroutine init_exact_duplicate_search(this)
        implicit none
        class(exact_duplicate_search) :: this
        if ( ASSOCIATED(this%root_node_ptr) ) nullify(this%root_node_ptr)
    end subroutine init_exact_duplicate_search


    !> Search Exact Duplicate from input data
    function search_exact_duplicate(this, x)
        implicit none
        class(exact_duplicate_search)       :: this
        integer(kind=8), target, intent(in) :: x(:,:)
        type(duplicate_index), ALLOCATABLE  :: search_exact_duplicate(:)

        integer(kind=8)          :: x_shape(2), l
        integer(kind=8), pointer :: x_ptr(:,:)

        type(node_eds), target   :: root_node
        type(node_eds_pointer), ALLOCATABLE   :: leaf_node_ptrs(:)

        call this%init()

        x_ptr => x
        x_shape = shape(x)
        this%n_samples = x_shape(1)
        this%n_columns = x_shape(2)

        allocate(this%root_node_ptr)
        call this%root_node_ptr%init(t_, f_, f_, 0_8, this%n_samples, this%n_columns, 1_8, allocate_indices=t_)
        call this%build_exact_duplicate(x_ptr)

        allocate(leaf_node_ptrs(0))

        call extract_leaf_nodes_eds(this%root_node_ptr, leaf_node_ptrs)

        ! print*, "size(leaf_node_ptrs): ", size(leaf_node_ptrs)
        allocate( search_exact_duplicate(size(leaf_node_ptrs)) )

        do l=1, size(leaf_node_ptrs), 1
            ! call quick_sort(leaf_node_ptrs(l)%ptr%indices(:), leaf_node_ptrs(l)%ptr%n_samples)
            search_exact_duplicate(l)%vector = leaf_node_ptrs(l)%ptr%indices(:)
        end do
    end function search_exact_duplicate


    !> Build 'exact_duplicate_search' object
    subroutine build_exact_duplicate(this, x_ptr)
        implicit none
        class(exact_duplicate_search)        :: this
        integer(kind=8), pointer, intent(in) :: x_ptr(:,:)

        integer(kind=8) :: node_idx
        integer(kind=8), allocatable :: indices(:) !< sample indices in current node.
        logical(kind=4), allocatable :: is_useless(:)

        allocate( is_useless(this%n_columns) )
        is_useless(:) = f_

        node_idx = 1_8
        allocate(indices(0))
        call this%build_exact_duplicate_rec(x_ptr, this%root_node_ptr, node_idx, is_useless)
    end subroutine build_exact_duplicate


    !> Build 'exact_duplicate_search' object recursively
    recursive subroutine build_exact_duplicate_rec(this, x_ptr, node_ptr, node_idx, is_useless)
        implicit none
        class(exact_duplicate_search)               :: this
        integer(kind=8), pointer, intent(in)        :: x_ptr(:,:)
        type(node_eds), pointer, intent(inout)      :: node_ptr
        integer(kind=8), intent(inout)              :: node_idx
        logical(kind=4), intent(inout)              :: is_useless(:)

        integer(kind=8) :: f, f_idx, i, i_idx, eval_count
        integer(kind=8) :: m_idx, m_idx_base
        integer(kind=8) :: f_val
        integer(kind=8), allocatable :: f_idxs(:)
        integer(kind=8), allocatable :: f_tmp(:), i_tmp(:), uniq(:)
        logical(kind=4), allocatable :: is_useless_l(:), is_useless_r(:)
        integer(kind=8), save :: count_leaf=0

        node_ptr%idx = node_idx

        ! Terminate Induction
        if (this%min_samples_in_leaf .ge. node_ptr%n_samples .or. all(is_useless)) then ! number of samples is less than 2 or Duplicate Example Only
            999 continue
            node_ptr%is_leaf = t_
            count_leaf = count_leaf + 1
            ! call node_ptr%info()
            ! print*, count_leaf
            return
        end if

        ! Initialize
        allocate(i_tmp(node_ptr%n_samples))
        allocate(f_tmp(node_ptr%n_samples))
        allocate(f_idxs(node_ptr%n_columns))
        do f=1, this%n_columns, 1
            f_idxs(f) = f
        end do
        call permutation(f_idxs, this%n_columns)

        eval_count = 0_8
        do f=1, this%n_columns, 1
            if ( eval_count .ge. 1_8 ) exit
            ! Select Feature Index
            f_idx = f_idxs(f)
            if ( is_useless(f_idx) ) cycle

            ! Extract Data
            do i=1, node_ptr%n_samples, 1
                i_idx = node_ptr%indices(i)
                f_tmp(i) = x_ptr(i_idx, f_idx)
                i_tmp(i) = i_idx
            end do

            ! Sort Extracted Data
            ! call quick_argsort(f_tmp, i_tmp, node_ptr%n_samples)
            call pbucket_argsort(f_tmp, i_tmp, node_ptr%n_samples)
            if ( f_tmp(1) .eq. f_tmp(node_ptr%n_samples) ) then
                is_useless(f_idx) = t_
                cycle
            end if

            ! Choose Median Index
            m_idx_base = node_ptr%n_samples / 2_8
            m_idx = -2_8
            do i=2, node_ptr%n_samples, 1
                if ( f_tmp(i-1) .ne. f_tmp(i) ) then
                    if ( i .ge. m_idx_base ) then
                        m_idx = i-1
                        exit
                    end if
                end if
            end do

            if ( m_idx .eq. -2_8 ) then
                do i=node_ptr%n_samples, 2, -1
                    if ( f_tmp(i-1) .ne. f_tmp(i) ) then
                        m_idx = i-1
                        exit
                    end if
                end do
            end if
            f_val = f_tmp(m_idx)

            eval_count = eval_count + 1
        end do
        if ( eval_count .eq. 0_8 ) goto 999

        node_ptr%split_val_i8 = f_tmp(m_idx)
        node_ptr%split_fid = f_idx

        call node_ptr%adopting_twins_in_eds(m_idx)

        allocate(is_useless_l(node_ptr%n_columns)); is_useless_l(:) = is_useless(:)
        allocate(is_useless_r(node_ptr%n_columns)); is_useless_r(:) = is_useless(:)
        
        node_idx = node_idx + 1
        allocate( node_ptr%node_l_ptr%indices(m_idx) )
        node_ptr%node_l_ptr%indices(1:m_idx) = i_tmp(1:m_idx)
        call this%build_exact_duplicate_rec(x_ptr, node_ptr%node_l_ptr, node_idx, is_useless_l)

        node_idx = node_idx + 1
        allocate( node_ptr%node_r_ptr%indices(node_ptr%n_samples-m_idx) )
        node_ptr%node_r_ptr%indices(1:node_ptr%n_samples-m_idx) = i_tmp(m_idx+1:)
        call this%build_exact_duplicate_rec(x_ptr, node_ptr%node_r_ptr, node_idx, is_useless_r)
    end subroutine build_exact_duplicate_rec


    ! -------------------------------------------------------------
    ! node_eds: node for exact duplicate search -------------------
    ! -------------------------------------------------------------

    !> Print Node information
    subroutine info_node_eds(this)
        implicit none
        class(node_eds) :: this
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

        print*, "node%split_val_i8            : ", this%split_val_i8
        print*, "node%split_val_r8            : ", this%split_val_r8
        print*, "node%split_fid               : ", this%split_fid

        print*, "allocated(node%indices)      : ", allocated(this%indices)
        if (allocated(this%indices)) then
            fin = minval((/5, size(this%indices)/))
            print*, "this%indices                 : ", this%indices(1:fin)
        end if
        print*, "associated(node%node_l)      : ", associated(this%node_l_ptr)
        print*, "associated(node%node_r)      : ", associated(this%node_r_ptr)
    end subroutine info_node_eds


    !> Initialize 'node_eds' node
    !! \param is_root root node or not
    !! \param is_leaf leaf node or not
    subroutine init_node_eds(this, is_root, is_leaf, is_left, depth, n_samples, n_columns, min_samples_in_leaf, &
        parent_node, allocate_indices)
        implicit none
        class(node_eds), intent(inout), target :: this
        logical(kind=4), intent(in) :: is_root, is_leaf, is_left
        integer(kind=8), intent(in) :: depth, n_samples, n_columns, min_samples_in_leaf
        type(node_eds), intent(in), optional, target :: parent_node
        logical(kind=4), intent(in), optional :: allocate_indices
        integer(kind=8) :: i

        

        this%is_root = is_root
        this%is_leaf = is_leaf
        this%is_left = is_left
        this%depth = depth
        this%n_samples = n_samples
        this%n_columns = n_columns
        this%min_samples_in_leaf = min_samples_in_leaf

        this%split_val_r8 = huge(0d0)
        this%split_val_i8 = huge(0_8)
        this%split_fid = -2_8

        call ifdealloc(this%indices)

        if (ASSOCIATED(this%node_p_ptr)) nullify(this%node_p_ptr)
        if (ASSOCIATED(this%node_l_ptr)) nullify(this%node_l_ptr)
        if (ASSOCIATED(this%node_r_ptr)) nullify(this%node_r_ptr)

        if (present(parent_node)) then
            if (ASSOCIATED(this%node_p_ptr)) deallocate(this%node_p_ptr)
            allocate(this%node_p_ptr)
            this%node_p_ptr => parent_node
        end if

        if (present(allocate_indices)) then
            if (allocate_indices) then
                allocate(this%indices(this%n_samples))
                do i=1, this%n_samples, 1
                    this%indices(i) = i
                end do
            end if
        end if
    end subroutine init_node_eds


    !> Create Child Node.
    !! \param median_index median point index
    subroutine adopting_twins_in_eds(this, median_index)
        implicit none
        class(node_eds), intent(inout) :: this
        integer(kind=8), intent(in) :: median_index

        integer(kind=8) :: n_samples_l, n_samples_r, i

        n_samples_l = median_index
        n_samples_r = this%n_samples-median_index

        allocate(this%node_l_ptr)
        call this%node_l_ptr%init(f_, f_, t_, this%depth+1, &
            n_samples_l, this%n_columns, this%min_samples_in_leaf, this)

        allocate(this%node_r_ptr)
        call this%node_r_ptr%init(f_, f_, f_, this%depth+1, &
            n_samples_r, this%n_columns, this%min_samples_in_leaf, this)
    end subroutine adopting_twins_in_eds


    ! -------------------------------------------------------------
    ! others ------------------------------------------------------
    ! -------------------------------------------------------------

    !> Extract All leaf nodes
    recursive subroutine extract_leaf_nodes_eds(root_node_ptr, leaf_node_ptr_array)
        implicit none
        type(node_eds), pointer :: root_node_ptr
        type(node_eds_pointer), ALLOCATABLE, intent(inout) :: leaf_node_ptr_array(:)
        type(node_eds_pointer)  :: tmp_ptr

        if ( root_node_ptr%is_leaf ) then
            tmp_ptr%ptr => root_node_ptr
            leaf_node_ptr_array = [leaf_node_ptr_array, tmp_ptr]
            return
        end if

        call extract_leaf_nodes_eds(root_node_ptr%node_l_ptr, leaf_node_ptr_array)
        call extract_leaf_nodes_eds(root_node_ptr%node_r_ptr, leaf_node_ptr_array)
    end subroutine extract_leaf_nodes_eds

end module mod_exact_duplicate_search
