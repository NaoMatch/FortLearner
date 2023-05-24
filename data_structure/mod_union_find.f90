module mod_union_find
    implicit none

    type union_find
        integer(kind=8) :: n_samples
        integer(kind=8) :: next_label
        integer(kind=8), allocatable :: labels(:)
        integer(kind=8), allocatable :: parents(:)
    contains
        procedure :: reset => reset_union_find
        procedure :: find => find_union_find
        procedure :: union => union_union_find
        procedure :: is_same_root => is_same_root_union_find
        procedure :: rank => rank_union_find
    end type union_find

    interface union_find
        module procedure new_union_find
    end interface union_find

contains

    function new_union_find(n_samples) result(obj)
        implicit none
        integer(kind=8) :: n_samples
        type(union_find) :: obj

        obj%n_samples = n_samples
        obj%next_label = n_samples+1

        allocate(obj%parents(n_samples))
        allocate(obj%labels(n_samples))

        obj%parents(:) = -1_8
        obj%labels(:) = -1_8
    end function new_union_find


    subroutine reset_union_find(this)
        implicit none
        class(union_find) :: this
        this%next_label = this%n_samples+1
        this%parents = -1
        this%labels = -1
    end subroutine reset_union_find


    function find_union_find(this, id) result(root_id)
        implicit none
        class(union_find) :: this
        integer(kind=8), intent(in) :: id
        integer(kind=8) :: root_id, parent_id, counter

        root_id = id
        do while (this%parents(root_id) > 0)
            root_id = this%parents(root_id)
        end do

        parent_id = id
        do while (this%parents(parent_id) > 0)
            this%parents(parent_id) = root_id
            parent_id = this%parents(parent_id)
        end do
        this%parents(root_id) = -2
    end function find_union_find


    subroutine union_union_find(this, id1, id2)
        implicit none
        class(union_find) :: this
        integer(kind=8), intent(in) :: id1, id2
        integer(kind=8) :: root_id1, root_id2
        integer(kind=8) :: rank_id1, rank_id2, swap

        root_id1 = this%find(id1)
        root_id2 = this%find(id2)
        if (root_id1 == root_id2) return ! already connected

        rank_id1 = -this%parents(root_id1)
        rank_id2 = -this%parents(root_id2)

        if (rank_id2 > rank_id1) then
            swap = root_id2
            root_id2 = root_id1
            root_id1 = swap
        end if
        this%parents(root_id1) = this%parents(root_id1) + this%parents(root_id2)
        this%parents(root_id2) = root_id1
    end subroutine union_union_find


    function is_same_root_union_find(this, id1, id2) result(is_same)
        implicit none
        class(union_find) :: this
        integer(kind=8), intent(in) :: id1, id2
        logical(kind=4) :: is_same

        integer(kind=8) :: root_id1, root_id2

        root_id1 = this%find(id1)
        root_id2 = this%find(id2)

        is_same = root_id1 == root_id2
    end function is_same_root_union_find


    function rank_union_find(this, id) result(rank)
        implicit none
        class(union_find) :: this
        integer(kind=8), intent(in) :: id
        integer(kind=8) :: rank

        integer(kind=8) :: parent_id

        parent_id = this%find(id)

        rank = - this%parents(parent_id)
    end function rank_union_find

end module mod_union_find
