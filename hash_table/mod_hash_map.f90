module mod_hash_map
    use mod_const
    use mod_hash
    implicit none

    type node 
        integer(kind=8) :: key = huge(0_8)
        integer(kind=8), allocatable :: keys(:)
        integer(kind=8) :: val
        integer(kind=8) :: raw_hash_val
        type(node), pointer :: child => null()
    end type node
    
    type node_ptr
        type(node), pointer :: ptr => null()
    end type node_ptr

    type hash_map
        integer(kind=8) :: size_root_list = 8192_8
        type(node_ptr), allocatable :: root(:) 
    contains
        procedure, pass :: compute_hash_hash_map
        procedure, pass :: compute_hash_hash_map_vector
        generic         :: compute_hash => compute_hash_hash_map, compute_hash_hash_map_vector

        procedure, pass :: insert_hash_map
        procedure, pass :: insert_hash_map_vector
        generic         :: insert => insert_hash_map, insert_hash_map_vector

        procedure, pass :: upsert_hash_map
        procedure, pass :: upsert_hash_map_vector
        generic         :: upsert => upsert_hash_map, upsert_hash_map_vector

        procedure, pass :: query_hash_map
        procedure, pass :: query_hash_map_vector
        generic         :: query => query_hash_map, query_hash_map_vector    

        procedure, pass :: get_unassociated_node_pointer_scalar
        procedure, pass :: get_unassociated_node_pointer_vector
        generic         :: get_unassociated_node_pointer & 
            => get_unassociated_node_pointer_scalar, get_unassociated_node_pointer_vector

        procedure, pass :: get_node_pointer_with_key_scalar
        procedure, pass :: get_node_pointer_with_key_vector
        generic         :: get_node_pointer_with_key & 
            => get_node_pointer_with_key_scalar, get_node_pointer_with_key_vector

        procedure :: get_list_size

        ! procedure :: remap
    end type hash_map

    interface hash_map
        module procedure :: new_hash_map
    end interface hash_map

contains


    !> Construct new 'hash_map' object.
    function new_hash_map() result(res)
        implicit none
        type(hash_map) :: res
        integer(kind=8) :: i
        allocate(res%root(0:res%size_root_list-1))

        do i=0, res%size_root_list-1, 1
            allocate(res%root(i)%ptr)
        end do
    end function new_hash_map


    !> Compute 'root list' index by one_at_a_time_hash
    !! \param key key
    !! \param hash_val mod(raw_hash_val, size_root_list)
    !! \param raw_hash_val one_at_a_time_hash(key)
    subroutine compute_hash_hash_map(this, key, hash_val, raw_hash_val)
        implicit none
        class(hash_map) :: this
        integer(kind=8) :: key
        integer(kind=8) :: hash_val, raw_hash_val
        raw_hash_val = one_at_a_time_hash(key)
        hash_val = abs(mod(raw_hash_val, this%size_root_list)) 
    end subroutine compute_hash_hash_map


    !> Compute 'root list' index by one_at_a_time_hash
    !! \param key key
    !! \param hash_val mod(raw_hash_val, size_root_list)
    !! \param raw_hash_val one_at_a_time_hash(key)
    subroutine compute_hash_hash_map_vector(this, keys, hash_val, raw_hash_val)
        implicit none
        class(hash_map) :: this
        integer(kind=8) :: keys(:)
        integer(kind=8) :: hash_val, raw_hash_val, key_size
        key_size = size(keys)
        raw_hash_val = xorshift64_hash(keys, key_size)
        hash_val = abs(mod(raw_hash_val, this%size_root_list)) 
    end subroutine compute_hash_hash_map_vector


    !> Insert new key and value
    !! \param key key
    !! \param val value
    subroutine insert_hash_map(this, key, val)
        implicit none
        class(hash_map) :: this
        integer(kind=8), intent(in) :: key
        integer(kind=8), intent(in) :: val
        integer(kind=8) :: hash_val, raw_hash_val
        type(node), pointer :: node_new
        call this%compute_hash(key, hash_val, raw_hash_val)

        call this%get_unassociated_node_pointer(key, hash_val, node_new, is_update=f_)
        if (.not. associated(node_new)) return
        node_new%key = key
        node_new%val = val
        node_new%raw_hash_val = raw_hash_val

        allocate(node_new%child)
    end subroutine insert_hash_map


    !> Insert new key and value
    !! \param key key
    !! \param val value
    subroutine insert_hash_map_vector(this, keys, val)
        implicit none
        class(hash_map) :: this
        integer(kind=8), intent(in) :: keys(:)
        integer(kind=8), intent(in) :: val
        integer(kind=8) :: hash_val, raw_hash_val
        type(node), pointer :: node_new
        call this%compute_hash(keys, hash_val, raw_hash_val)
        print*, hash_val, raw_hash_val

        call this%get_unassociated_node_pointer(keys, hash_val, node_new, is_update=f_)
        if (.not. associated(node_new)) return
        node_new%keys = keys
        node_new%val = val
        node_new%raw_hash_val = raw_hash_val

        allocate(node_new%child)
    end subroutine insert_hash_map_vector


    !> Upsert new key and value
    !! \param key key
    !! \param val value
    subroutine upsert_hash_map(this, key, val)
        implicit none
        class(hash_map) :: this
        integer(kind=8), intent(in) :: key
        integer(kind=8), intent(in) :: val
        integer(kind=8) :: hash_val, raw_hash_val
        type(node), pointer :: node_new
        call this%compute_hash(key, hash_val, raw_hash_val)

        call this%get_unassociated_node_pointer(key, hash_val, node_new, is_update=t_)
        node_new%key = key
        node_new%val = val
        node_new%raw_hash_val = raw_hash_val

        allocate(node_new%child)
    end subroutine upsert_hash_map


    !> Upsert new key and value
    !! \param key key
    !! \param val value
    subroutine upsert_hash_map_vector(this, keys, val)
        implicit none
        class(hash_map) :: this
        integer(kind=8), intent(in) :: keys(:)
        integer(kind=8), intent(in) :: val
        integer(kind=8) :: hash_val, raw_hash_val
        type(node), pointer :: node_new
        call this%compute_hash(keys, hash_val, raw_hash_val)

        call this%get_unassociated_node_pointer(keys, hash_val, node_new, is_update=t_)
        node_new%keys = keys
        node_new%val = val
        node_new%raw_hash_val = raw_hash_val

        allocate(node_new%child)
    end subroutine upsert_hash_map_vector


    !> Insert new key and value
    !! \param key key
    !! \param val value
    function query_hash_map(this, key, status) result(val)
        implicit none
        class(hash_map) :: this
        integer(kind=8), intent(in) :: key
        integer(kind=8) :: val, status
        integer(kind=8) :: hash_val, raw_hash_val
        type(node), pointer :: node_new
        call this%compute_hash(key, hash_val, raw_hash_val)

        val = -huge(val)

        call this%get_node_pointer_with_key(key, hash_val, node_new)
        if (associated(node_new)) then
            val = node_new%val
            status = 0
        else
            status = 1
        end if
    end function query_hash_map


    !> Insert new key and value
    !! \param key key
    !! \param val value
    function query_hash_map_vector(this, keys, status) result(val)
        implicit none
        class(hash_map) :: this
        integer(kind=8), intent(in) :: keys(:)
        integer(kind=8) :: val, status
        integer(kind=8) :: hash_val, raw_hash_val
        type(node), pointer :: node_new
        call this%compute_hash(keys, hash_val, raw_hash_val)

        val = -huge(val)

        call this%get_node_pointer_with_key(keys, hash_val, node_new)
        if (associated(node_new)) then
            val = node_new%val
            status = 0
        else
            status = 1
        end if
    end function query_hash_map_vector


    !> Get node pointer with key.
    !! \param key key
    !! \param hash_val root list index computed by 'compute_hash'
    !! \param node_new node pointer with key
    subroutine get_node_pointer_with_key_scalar(this, key, hash_val, node_new)
        implicit none
        class(hash_map) :: this
        integer(kind=8), intent(in) :: key
        integer(kind=8) :: hash_val
        type(node), pointer :: node_new
        type(node), pointer :: root_node_ptr

        integer(kind=8) :: l, list_size

        if (associated(node_new)) nullify(node_new)
        root_node_ptr => this%root(hash_val)%ptr

        list_size = this%get_list_size(hash_val)

        do l=1, list_size, 1
            if (root_node_ptr%key == key) then
                node_new => root_node_ptr
                exit
            end if
            root_node_ptr => root_node_ptr%child
        end do
    end subroutine get_node_pointer_with_key_scalar


    !> Get node pointer with key.
    !! \param key key
    !! \param hash_val root list index computed by 'compute_hash'
    !! \param node_new node pointer with key
    subroutine get_node_pointer_with_key_vector(this, keys, hash_val, node_new)
        implicit none
        class(hash_map) :: this
        integer(kind=8), intent(in) :: keys(:)
        integer(kind=8) :: hash_val
        type(node), pointer :: node_new
        type(node), pointer :: root_node_ptr

        integer(kind=8) :: l, list_size

        if (associated(node_new)) nullify(node_new)
        root_node_ptr => this%root(hash_val)%ptr

        list_size = this%get_list_size(hash_val)

        do l=1, list_size, 1
            if (all(root_node_ptr%keys == keys)) then
                node_new => root_node_ptr
                exit
            end if
            root_node_ptr => root_node_ptr%child
        end do
    end subroutine get_node_pointer_with_key_vector


    !> Get unassociated node pointer for insertion.
    !! \param key key
    !! \param hash_val root list index computed by 'compute_hash'
    !! \param node_new new node pointer
    subroutine get_unassociated_node_pointer_scalar(this, key, hash_val, node_new, is_update)
        implicit none
        class(hash_map) :: this
        integer(kind=8), intent(in) :: key
        integer(kind=8), intent(in) :: hash_val
        type(node), pointer :: node_new
        logical(kind=4), intent(in) :: is_update

        type(node), pointer :: root_node_ptr

        if (associated(node_new)) nullify(node_new)
        root_node_ptr => this%root(hash_val)%ptr

        do while (t_)
            if (root_node_ptr%key == key) then
                if (is_update) node_new => root_node_ptr
                exit
            end if
            if (.not. associated(root_node_ptr%child)) then
                node_new => root_node_ptr
                exit
            end if
            root_node_ptr => root_node_ptr%child
        end do
    end subroutine get_unassociated_node_pointer_scalar


    !> Get unassociated node pointer for insertion.
    !! \param key key
    !! \param hash_val root list index computed by 'compute_hash'
    !! \param node_new new node pointer
    subroutine get_unassociated_node_pointer_vector(this, keys, hash_val, node_new, is_update)
        implicit none
        class(hash_map) :: this
        integer(kind=8), intent(in) :: keys(:)
        integer(kind=8), intent(in) :: hash_val
        type(node), pointer :: node_new
        logical(kind=4), intent(in) :: is_update

        type(node), pointer :: root_node_ptr

        if (associated(node_new)) nullify(node_new)
        root_node_ptr => this%root(hash_val)%ptr

        do while (t_)
            if (allocated(root_node_ptr%keys)) then
                if (all(root_node_ptr%keys == keys)) then
                    if (is_update) node_new => root_node_ptr
                    exit
                end if
            end if
            if (.not. associated(root_node_ptr%child)) then
                node_new => root_node_ptr
                exit
            end if
            root_node_ptr => root_node_ptr%child
        end do
    end subroutine get_unassociated_node_pointer_vector


    !> Get list size
    !! \param list_idx
    function get_list_size(this, list_idx) result(list_size)
        implicit none
        class(hash_map) :: this
        integer(kind=8) :: list_idx
        integer(kind=8) :: list_size

        type(node), pointer :: root_node_ptr

        list_size = 0
        root_node_ptr => this%root(list_idx)%ptr

        do while (t_)
            if (.not. associated(root_node_ptr%child)) then
                exit
            end if
            root_node_ptr => root_node_ptr%child
            list_size = list_size + 1
        end do
    end function get_list_size

    
end module mod_hash_map