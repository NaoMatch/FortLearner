module mod_sinusoidal
    use mod_variable
    implicit none

    type, extends(base_function) :: sinusoidal
    contains
        procedure :: forward_1in_1out => forward_sinusoidal
        procedure :: backward_1in_2out => backward_sinusoidal
    end type sinusoidal
    
    interface sinusoidal
        module procedure new_sinusoidal
    end interface sinusoidal

contains


    function sin_(var_in) result(var_out)
        implicit none
        type(variable) :: var_in
        type(variable) :: var_out

        type(sinusoidal) :: sine

        sine = sinusoidal()

        var_out = sine%act(var_out)
    end function 


    function new_sinusoidal()
        implicit none
        type(sinusoidal) :: new_sinusoidal
        new_sinusoidal%fname = "sinusoidal"
        new_sinusoidal%n_in = 1
        new_sinusoidal%n_out = 1
    end function new_sinusoidal

    function forward_sinusoidal(this, v_in) result(v_out)
        implicit none
        class(sinusoidal) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable :: v_out(:,:)
        v_out = sin(v_in)
    end function forward_sinusoidal

    function backward_sinusoidal(this, g_in) result(g_outs)
        implicit none
        class(sinusoidal) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        g_outs(1)%g = g_in * cos(vstack(this%id_in_1)%v)
    end function backward_sinusoidal

end module mod_sinusoidal