module mod_tangent
    use mod_variable
    implicit none

    type, extends(base_function) :: tangent
    contains
        procedure :: forward_1in_1out => forward_tangent
        procedure :: backward_1in_2out => backward_tangent
    end type tangent
    
    interface tangent
        module procedure new_tangent
    end interface tangent

contains


    function tan_(var_in) result(var_out)
        implicit none
        type(variable) :: var_in
        type(variable) :: var_out

        type(tangent) :: tange

        tange = tangent()

        var_out = tange%act(var_out)
    end function 


    function new_tangent()
        implicit none
        type(tangent) :: new_tangent
        new_tangent%fname = "tangent"
        new_tangent%n_in = 1
        new_tangent%n_out = 1
    end function new_tangent

    subroutine forward_tangent(this, v_out, v_in)
        implicit none
        class(tangent) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)
        v_out = tan(v_in)
    end subroutine forward_tangent

    function backward_tangent(this, g_in) result(g_outs)
        implicit none
        class(tangent) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        g_outs(1)%g = g_in * cos(vstack(this%id_in_1)%v)**2d0
    end function backward_tangent

end module mod_tangent