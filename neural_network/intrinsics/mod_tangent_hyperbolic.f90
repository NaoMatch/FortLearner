module mod_tangent_hyperbolic
    use mod_variable
    implicit none

    type, extends(base_function) :: tangent_hyperbolic
    contains
        procedure :: forward_1in_1out => forward_tangent_hyperbolic
        procedure :: backward_1in_2out => backward_tangent_hyperbolic
    end type tangent_hyperbolic
    
    interface tangent_hyperbolic
        module procedure new_tangent_hyperbolic
    end interface tangent_hyperbolic

contains


    function tanh_(var_in) result(var_out)
        implicit none
        type(variable) :: var_in
        type(variable) :: var_out

        type(tangent_hyperbolic) :: tan_hypo

        tan_hypo = tangent_hyperbolic()

        var_out = tan_hypo%act(var_in)
    end function tanh_


    function new_tangent_hyperbolic()
        implicit none
        type(tangent_hyperbolic) :: new_tangent_hyperbolic
        new_tangent_hyperbolic%fname = "tangent_hyperbolic"
        new_tangent_hyperbolic%n_in = 1
        new_tangent_hyperbolic%n_out = 1
    end function new_tangent_hyperbolic

    subroutine forward_tangent_hyperbolic(this, v_out, v_in)
        implicit none
        class(tangent_hyperbolic) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)
        allocate(v_out, source=tanh(v_in))
        v_out = tanh(v_in)
    end subroutine forward_tangent_hyperbolic

    function backward_tangent_hyperbolic(this, g_in) result(g_outs)
        implicit none
        class(tangent_hyperbolic) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        g_outs(1)%g = g_in * vstack(this%id_out_1)%v * (1d0 - vstack(this%id_out_1)%v)
    end function backward_tangent_hyperbolic

end module mod_tangent_hyperbolic