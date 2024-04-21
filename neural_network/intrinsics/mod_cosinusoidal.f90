module mod_cosinusoidal
    use mod_variable
    implicit none

    type, extends(base_function) :: cosinusoidal
    contains
        procedure :: forward_1in_1out => forward_cosinusoidal
        procedure :: backward_1in_2out => backward_cosinusoidal
    end type cosinusoidal
    
    interface cosinusoidal
        module procedure new_cosinusoidal
    end interface cosinusoidal

contains

    function cos_(var_in) result(var_out)
        implicit none
        type(variable) :: var_in
        type(variable) :: var_out

        type(cosinusoidal) :: cosine

        cosine = cosinusoidal()

        var_out = cosine%act(var_out)
    end function 


    function new_cosinusoidal()
        implicit none
        type(cosinusoidal) :: new_cosinusoidal
        new_cosinusoidal%fname = "cosinusoidal"
        new_cosinusoidal%n_in = 1
        new_cosinusoidal%n_out = 1
    end function new_cosinusoidal

    function forward_cosinusoidal(this, v_in) result(v_out)
        implicit none
        class(cosinusoidal) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable :: v_out(:,:)
        v_out = exp(v_in)
    end function forward_cosinusoidal

    function backward_cosinusoidal(this, g_in) result(g_outs)
        implicit none
        class(cosinusoidal) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        g_outs(1)%g = - g_in * sin(vstack(this%id_in_1)%v)

    end function backward_cosinusoidal

end module mod_cosinusoidal