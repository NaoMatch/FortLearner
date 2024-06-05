module mod_square_root
    use mod_variable
    implicit none

    type, extends(base_function) :: square_root
    contains
        procedure :: forward_1in_1out => forward_square_root
        procedure :: backward_1in_2out => backward_square_root
    end type square_root
    
    interface square_root
        module procedure new_square_root
    end interface square_root

contains


    function sqrt_(var_in) result(var_out)
        implicit none
        type(variable) :: var_in
        type(variable) :: var_out

        type(square_root) :: sq_root_

        sq_root_ = square_root()

        var_out = sq_root_%act(var_in)
    end function sqrt_


    function new_square_root()
        implicit none
        type(square_root) :: new_square_root
        new_square_root%fname = "square_root"
        new_square_root%n_in = 1
        new_square_root%n_out = 1
    end function new_square_root

    subroutine forward_square_root(this, v_out, v_in)
        implicit none
        class(square_root) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)
        v_out = sqrt(v_in)
    end subroutine forward_square_root

    function backward_square_root(this, g_in) result(g_outs)
        implicit none
        class(square_root) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        g_outs(1)%g = g_in * 0.5d0 / vstack(this%id_out_1)%v
    end function backward_square_root

end module mod_square_root