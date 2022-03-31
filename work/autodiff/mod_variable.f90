module mod_variable
    implicit none

    type node
        real(kind=8) :: val
        real(kind=8) :: grad

        type(node), pointer :: node_l => null()
        real(kind=8) :: grad_l

        type(node), pointer :: node_r => null()
        real(kind=8) :: grad_r
    contains
        procedure :: node_backward
    end type node


    type variable
        type(node), pointer :: nptr => null() ! node pointer
    contains
        procedure :: get_val
        procedure :: get_grad
        procedure :: backward => variable_backward
    end type variable

    interface variable
        module procedure new_variable
    end interface variable

contains

    function new_variable(val)
        implicit none
        type(variable) :: new_variable
        real(kind=8), intent(in) :: val

        allocate(new_variable%nptr)
        new_variable%nptr%val = val
        new_variable%nptr%grad = 0d0
    end function new_variable

    function get_val(this)
        class(variable) :: this
        real(kind=8) :: get_val

        get_val = this%nptr%val
    end function get_val

    function get_grad(this)
        class(variable) :: this
        real(kind=8) :: get_grad

        get_grad = this%nptr%grad
    end function get_grad

    recursive subroutine node_backward(this, out)
        class(node), intent(inout) :: this
        real(kind=8), intent(in) :: out

        this%grad = this%grad + out
        if (associated(this%node_l)) call this%node_l%node_backward(out*this%grad_l)
        if (associated(this%node_r)) call this%node_r%node_backward(out*this%grad_r)
    end subroutine node_backward

    subroutine variable_backward(this)
        class(variable), intent(inout) :: this
        associate (node_ptr => this%nptr)
            node_ptr%grad = 1.0d0
            if (associated(node_ptr%node_l)) call node_ptr%node_l%node_backward(node_ptr%grad_l)
            if (associated(node_ptr%node_r)) call node_ptr%node_r%node_backward(node_ptr%grad_r)
        end associate
    end subroutine variable_backward



end module mod_variable
