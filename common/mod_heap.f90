module mod_heap
    implicit none

    type heap
        integer(kind=8) :: n_space             ! number of spc
        integer(kind=8) :: n_count             ! number of cnt
        integer(kind=8) :: n_used_spc          ! current number of used spc
        integer(kind=8) :: n_used_cnt          ! current number of used spc
        integer(kind=8), allocatable :: spc(:) ! heap space
        integer(kind=8), allocatable :: cnt(:) ! count
    contains
        procedure :: add => add_heap
        procedure :: pop => pop_heap
    end type heap

    interface heap
        procedure new_heap
    end interface heap
    
contains
    
    function new_heap(n_space, n_count) result(obj)
        implicit none
        type(heap) :: obj
        integer(kind=8), optional :: n_space, n_count
        
        obj%n_space = n_space*2
        obj%n_count = n_count*2

        allocate(obj%spc(obj%n_space))
        allocate(obj%cnt(obj%n_count))

        obj%n_used_spc = 0_8
        obj%n_used_cnt = 0_8
    end function new_heap

    subroutine add_heap(this, values, n_values)
        implicit none
        class(heap) :: this
        integer(kind=8), intent(in) :: values(n_values)
        integer(kind=8), intent(in) :: n_values
        integer(kind=8), allocatable :: tmp_s(:), tmp_c(:)
        integer(kind=8) :: n_space
        integer(kind=8) :: n_used_spc, n_used_cnt
        
        n_used_spc = this%n_used_spc + n_values
        n_used_cnt = this%n_used_cnt + 1_8
        
        if (n_used_spc>=this%n_space) then
            this%n_space = n_used_spc*2
            allocate(tmp_s(n_used_spc))

            tmp_s(:) = this%spc(1:n_used_spc)

            deallocate(this%spc)
            allocate(this%spc(this%n_space))

            this%spc(1:n_used_spc) = tmp_s(:)
        end if
        
        if (n_used_cnt>=this%n_count) then
            this%n_count = n_used_cnt*2
            allocate(tmp_c(n_used_cnt))

            tmp_c(:) = this%cnt(1:n_used_cnt)

            deallocate(this%cnt)
            allocate(this%cnt(this%n_count))

            this%cnt(1:n_used_cnt) = tmp_c(:)
        end if

        this%spc(this%n_used_spc+1:n_used_spc) = values(:)
        this%cnt(this%n_used_cnt+1) = n_values

        this%n_used_spc = n_used_spc
        this%n_used_cnt = n_used_cnt
    end subroutine 

    function pop_heap(this) result(values)
        implicit none
        class(heap) :: this
        integer(kind=8), allocatable :: values(:)
        integer(kind=8) :: n_values
        if (this%n_used_cnt==0_8) then
            allocate(values(0))    
            return
        end if

        n_values = this%cnt(this%n_used_cnt)
        allocate(values(n_values))

        values = this%spc(this%n_used_spc-n_values+1:this%n_used_spc)

        this%n_used_cnt = this%n_used_cnt - 1
        this%n_used_spc = this%n_used_spc - n_values
    end function pop_heap


end module mod_heap