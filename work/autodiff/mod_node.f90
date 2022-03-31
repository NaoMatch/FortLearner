module mod_node
    implicit none
    private

    public :: node
    
    type node
        real(kind=8) :: val

        type(node), pointer :: node_l => null()
        real(kind=8) :: grad_l

        type(node), pointer :: node_r => null()
        real(kind=8) :: grad_r
    contains

    end type node

    interface node
        module procedure new_node
    end interface node

contains

    function new_node(val)
        implicit none
        type(node)               :: new_node
        real(kind=8), intent(in) :: val
        new_node%val = val
    end function new_node


end module mod_node
