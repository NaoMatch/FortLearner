!> A module for hash map.
module mod_hash_map
    use mod_const
    use mod_hash
    implicit none

    !> A base type for node in hash map
    type base_node
        integer(kind=8)              :: key = huge(0_8)
        integer(kind=8), allocatable :: keys(:)
        integer(kind=8)              :: raw_hash_val
    end type base_node

    !> A node type
    type, extends(base_node) :: node_keyi8_valsr8
        real(kind=8), allocatable    :: vals(:)
        type(node_keyi8_valsr8), pointer  :: child => null()
    end type node_keyi8_valsr8

    !> A node type
    type, extends(base_node) :: node_keyi8_vali8s
        integer(kind=8), allocatable    :: vals(:)
        type(node_keyi8_vali8s), pointer  :: child => null()
    end type node_keyi8_vali8s

    type, extends(base_node) ::  node_r8_val
        real(kind=8) :: val
        type(node_r8_val), pointer :: child => null()
    end type node_r8_val

    type, extends(base_node) ::  node_keyi8_vali8
        integer(kind=8) :: val
        type(node_keyi8_vali8), pointer :: child => null()
    end type node_keyi8_vali8
    


    type node_keyi8_valsr8_ptr
        type(node_keyi8_valsr8), pointer :: ptr => null()
    end type node_keyi8_valsr8_ptr
    
    type node_keyi8_vali8s_ptr
        type(node_keyi8_vali8s), pointer :: ptr => null()
    end type node_keyi8_vali8s_ptr
    
    type node_r8_val_ptr
        type(node_r8_val), pointer :: ptr => null()
    end type node_r8_val_ptr
    
    type node_keyi8_vali8_ptr
        type(node_keyi8_vali8), pointer :: ptr => null()
    end type node_keyi8_vali8_ptr



    !> A base type of hash map
    type base_hash_map
        integer(kind=8) :: size_root_list = 8192_8
    contains
        procedure :: compute_hash => compute_hash_hash_map
    end type base_hash_map

    type, extends(base_hash_map) :: hash_map_keyi8_valsr8
        type(node_keyi8_valsr8_ptr), allocatable :: root(:)
    contains
        procedure :: insert => insert_hash_map_keyi8_valsr8
        procedure :: upsert => upsert_hash_map_keyi8_valsr8
        procedure :: query => query_hash_map_keyi8_valsr8
        procedure :: get_unassociated_node_pointer => get_unassociated_node_pointer_keyi8_valsr8
        procedure :: get_node_pointer_with_key => get_node_pointer_with_key_keyi8_valsr8
        procedure :: get_list_size => get_list_size_keyi8_valsr8
    end type hash_map_keyi8_valsr8

    type, extends(base_hash_map) ::  hash_map_keyi8_vali8
        type(node_keyi8_vali8_ptr), allocatable :: root(:) 
    contains
        procedure :: insert => insert_hash_map_keyi8_vali8
        procedure :: upsert => upsert_hash_map_keyi8_vali8
        procedure :: query => query_hash_map_keyi8_vali8
        procedure :: get_unassociated_node_pointer => get_unassociated_node_pointer_keyi8_vali8
        procedure :: get_node_pointer_with_key => get_node_pointer_with_key_keyi8_vali8
        procedure :: get_list_size => get_list_size_keyi8_vali8
    end type hash_map_keyi8_vali8

    
    interface hash_map_keyi8_vali8
        module procedure :: new_hash_map_keyi8_vali8
    end interface hash_map_keyi8_vali8

    interface hash_map_keyi8_valsr8
        module procedure :: new_hash_map_keyi8_valsr8
    end interface hash_map_keyi8_valsr8

contains


    !> Construct new 'hash_map' object.
    function new_hash_map_keyi8_vali8() result(res)
        implicit none
        type(hash_map_keyi8_vali8) :: res
        integer(kind=8) :: i
        allocate(res%root(0:res%size_root_list-1))

        do i=0, res%size_root_list-1, 1
            allocate(res%root(i)%ptr)
        end do
    end function new_hash_map_keyi8_vali8


    !> Construct new 'hash_map' object.
    function new_hash_map_keyi8_valsr8(size_root_list) result(res)
        implicit none
        type(hash_map_keyi8_valsr8) :: res

        integer(kind=8), optional :: size_root_list
        integer(kind=8) :: i
        if (present(size_root_list)) then
            res%size_root_list = size_root_list
        end if
        allocate(res%root(0:res%size_root_list-1))

        do i=0, res%size_root_list-1, 1
            allocate(res%root(i)%ptr)
        end do
    end function new_hash_map_keyi8_valsr8


    !> Compute 'root list' index by one_at_a_time_hash
    !! \param key key
    !! \param hash_val mod(raw_hash_val, size_root_list)
    !! \param raw_hash_val one_at_a_time_hash(key)
    subroutine compute_hash_hash_map(this, key, hash_val, raw_hash_val)
        implicit none
        class(base_hash_map) :: this
        integer(kind=8) :: key
        integer(kind=8) :: hash_val, raw_hash_val
        raw_hash_val = one_at_a_time_hash(key)
        hash_val = abs(mod(raw_hash_val, this%size_root_list)) 
    end subroutine compute_hash_hash_map

    !> Insert new key and value
    !! \param key key
    !! \param val value
    subroutine insert_hash_map_keyi8_vali8(this, key, val)
        implicit none
        class(hash_map_keyi8_vali8) :: this
        integer(kind=8), intent(in) :: key
        integer(kind=8), intent(in) :: val
        integer(kind=8) :: hash_val, raw_hash_val
        type(node_keyi8_vali8), pointer :: node_new
        call this%compute_hash(key, hash_val, raw_hash_val)

        call this%get_unassociated_node_pointer(key, hash_val, node_new, is_update=f_)
        if (.not. associated(node_new)) return
        node_new%key = key
        node_new%val = val
        node_new%raw_hash_val = raw_hash_val

        allocate(node_new%child)
    end subroutine insert_hash_map_keyi8_vali8

    !> Insert new key and value
    !! \param key key
    !! \param val value
    subroutine insert_hash_map_keyi8_valsr8(this, key, vals)
        implicit none
        class(hash_map_keyi8_valsr8) :: this
        integer(kind=8), intent(in) :: key
        real(kind=8), intent(in) :: vals(:)
        integer(kind=8) :: hash_val, raw_hash_val
        type(node_keyi8_valsr8), pointer :: node_new
        call this%compute_hash(key, hash_val, raw_hash_val)

        call this%get_unassociated_node_pointer(key, hash_val, node_new, is_update=f_)
        if (.not. associated(node_new)) return
        node_new%key = key
        if (allocated(node_new%vals)) deallocate(node_new%vals)
        allocate(node_new%vals, source=vals)
        node_new%raw_hash_val = raw_hash_val

        allocate(node_new%child)
    end subroutine insert_hash_map_keyi8_valsr8


    !> Upsert new key and value
    !! \param key key
    !! \param val value
    subroutine upsert_hash_map_keyi8_vali8(this, key, val)
        implicit none
        class(hash_map_keyi8_vali8) :: this
        integer(kind=8), intent(in) :: key
        integer(kind=8), intent(in) :: val
        integer(kind=8) :: hash_val, raw_hash_val
        type(node_keyi8_vali8), pointer :: node_new
        call this%compute_hash(key, hash_val, raw_hash_val)

        call this%get_unassociated_node_pointer(key, hash_val, node_new, is_update=t_)
        node_new%key = key
        node_new%val = val
        node_new%raw_hash_val = raw_hash_val

        allocate(node_new%child)
    end subroutine upsert_hash_map_keyi8_vali8


    !> Upsert new key and value
    !! \param key key
    !! \param val value
    subroutine upsert_hash_map_keyi8_valsr8(this, key, vals)
        implicit none
        class(hash_map_keyi8_valsr8) :: this
        integer(kind=8), intent(in) :: key
        real(kind=8), intent(in) :: vals(:)
        integer(kind=8) :: hash_val, raw_hash_val
        type(node_keyi8_valsr8), pointer :: node_new
        call this%compute_hash(key, hash_val, raw_hash_val)

        call this%get_unassociated_node_pointer(key, hash_val, node_new, is_update=t_)
        node_new%key = key
        if (allocated(node_new%vals)) deallocate(node_new%vals)
        allocate(node_new%vals, source=vals)
        node_new%raw_hash_val = raw_hash_val

        allocate(node_new%child)
    end subroutine upsert_hash_map_keyi8_valsr8


    !> Insert new key and value
    !! \param key key
    !! \param val value
    function query_hash_map_keyi8_vali8(this, key, status) result(val)
        implicit none
        class(hash_map_keyi8_vali8) :: this
        integer(kind=8), intent(in) :: key
        integer(kind=8) :: val, status
        integer(kind=8) :: hash_val, raw_hash_val
        type(node_keyi8_vali8), pointer :: node_new
        call this%compute_hash(key, hash_val, raw_hash_val)

        val = -huge(val)

        call this%get_node_pointer_with_key(key, hash_val, node_new)
        if (associated(node_new)) then
            val = node_new%val
            status = 0
        else
            status = 1
        end if
    end function query_hash_map_keyi8_vali8


    !> Insert new key and value
    !! \param key key
    !! \param val value
    function query_hash_map_keyi8_valsr8(this, key, status) result(vals)
        implicit none
        class(hash_map_keyi8_valsr8) :: this
        integer(kind=8), intent(in) :: key
        real(kind=8), allocatable :: vals(:)
        integer(kind=8) :: status
        integer(kind=8) :: hash_val, raw_hash_val
        type(node_keyi8_valsr8), pointer :: node_new
        call this%compute_hash(key, hash_val, raw_hash_val)

        call this%get_node_pointer_with_key(key, hash_val, node_new)
        if (associated(node_new)) then
            allocate(vals, source=node_new%vals)
            status = 0
        else
            allocate(vals(0))
            status = 1
        end if
    end function query_hash_map_keyi8_valsr8


    !> Get node pointer with key.
    !! \param key key
    !! \param hash_val root list index computed by 'compute_hash'
    !! \param node_new node pointer with key
    subroutine get_node_pointer_with_key_keyi8_vali8(this, key, hash_val, node_new)
        implicit none
        class(hash_map_keyi8_vali8) :: this
        integer(kind=8), intent(in) :: key
        integer(kind=8) :: hash_val
        type(node_keyi8_vali8), pointer :: node_new
        type(node_keyi8_vali8), pointer :: root_node_ptr

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
    end subroutine get_node_pointer_with_key_keyi8_vali8


    !> Get node pointer with key.
    !! \param key key
    !! \param hash_val root list index computed by 'compute_hash'
    !! \param node_new node pointer with key
    subroutine get_node_pointer_with_key_keyi8_valsr8(this, key, hash_val, node_new)
        implicit none
        class(hash_map_keyi8_valsr8) :: this
        integer(kind=8), intent(in) :: key
        integer(kind=8) :: hash_val
        type(node_keyi8_valsr8), pointer :: node_new
        type(node_keyi8_valsr8), pointer :: root_node_ptr

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
    end subroutine get_node_pointer_with_key_keyi8_valsr8


    !> Get unassociated node pointer for insertion.
    !! \param key key
    !! \param hash_val root list index computed by 'compute_hash'
    !! \param node_new new node pointer
    subroutine get_unassociated_node_pointer_keyi8_vali8(this, key, hash_val, node_new, is_update)
        implicit none
        class(hash_map_keyi8_vali8) :: this
        integer(kind=8), intent(in) :: key
        integer(kind=8), intent(in) :: hash_val
        type(node_keyi8_vali8), pointer :: node_new
        logical(kind=4), intent(in) :: is_update

        type(node_keyi8_vali8), pointer :: root_node_ptr

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
    end subroutine get_unassociated_node_pointer_keyi8_vali8


    !> Get unassociated node pointer for insertion.
    !! \param key key
    !! \param hash_val root list index computed by 'compute_hash'
    !! \param node_new new node pointer
    subroutine get_unassociated_node_pointer_keyi8_valsr8(this, key, hash_val, node_new, is_update)
        implicit none
        class(hash_map_keyi8_valsr8) :: this
        integer(kind=8), intent(in) :: key
        integer(kind=8), intent(in) :: hash_val
        type(node_keyi8_valsr8), pointer :: node_new
        logical(kind=4), intent(in) :: is_update

        type(node_keyi8_valsr8), pointer :: root_node_ptr

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
    end subroutine get_unassociated_node_pointer_keyi8_valsr8


    !> Get list size
    !! \param list_idx
    function get_list_size_keyi8_vali8(this, list_idx) result(list_size)
        implicit none
        class(hash_map_keyi8_vali8) :: this
        integer(kind=8) :: list_idx
        integer(kind=8) :: list_size

        type(node_keyi8_vali8), pointer :: root_node_ptr

        list_size = 0
        root_node_ptr => this%root(list_idx)%ptr

        do while (t_)
            if (.not. associated(root_node_ptr%child)) then
                exit
            end if
            root_node_ptr => root_node_ptr%child
            list_size = list_size + 1
        end do
    end function get_list_size_keyi8_vali8


    !> Get list size
    !! \param list_idx
    function get_list_size_keyi8_valsr8(this, list_idx) result(list_size)
        implicit none
        class(hash_map_keyi8_valsr8) :: this
        integer(kind=8) :: list_idx
        integer(kind=8) :: list_size

        type(node_keyi8_valsr8), pointer :: root_node_ptr

        list_size = 0
        root_node_ptr => this%root(list_idx)%ptr

        do while (t_)
            if (.not. associated(root_node_ptr%child)) then
                exit
            end if
            root_node_ptr => root_node_ptr%child
            list_size = list_size + 1
        end do
    end function get_list_size_keyi8_valsr8

    
end module mod_hash_map