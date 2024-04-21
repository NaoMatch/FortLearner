module mod_get_item
    use mod_variable
    implicit none

    type, extends(base_function) :: get_item
    contains
        procedure :: forward_1in_1out => forward_get_item
        procedure :: backward_1in_2out => backward_get_item
    end type get_item
    
    interface get_item
        module procedure new_get_item
    end interface get_item

contains

    function get_item_(var_in, indices) result(var_out)
        implicit none
        type(variable) :: var_in
        type(variable) :: var_out
        integer(kind=8) :: indices(:)

        type(get_item) :: gi_

        gi_ = get_item(indices)

        var_out = gi_%act(var_in)
    end function get_item_


    function new_get_item(indices)
        implicit none
        type(get_item) :: new_get_item
        integer(kind=8) :: indices(:)
        new_get_item%fname = "get_item"
        new_get_item%n_in = 1
        new_get_item%n_out = 1
        new_get_item%indices = indices
    end function new_get_item


    function forward_get_item(this, v_in) result(v_out)
        implicit none
        class(get_item) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable :: v_out(:,:)
        v_out = v_in(this%indices,:)
    end function forward_get_item

    function backward_get_item(this, g_in) result(g_outs)
        implicit none
        class(get_item) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        integer(kind=8) :: i, id

        allocate(g_outs(1)%g, source=vstack(this%id_in_1)%v)
        g_outs(1)%g = 0d0

        do i=1, size(this%indices), 1
            id = this%indices(i)
            g_outs(1)%g(id,:) = g_outs(1)%g(id,:) + g_in(i,:)
        end do
    end function backward_get_item

end module mod_get_item