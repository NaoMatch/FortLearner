module mod_exponential
    use mod_variable
    implicit none

    type, extends(base_function) :: exponential
    contains
        procedure :: forward_1in_1out => forward_exponential
        procedure :: backward_1in_2out => backward_exponential
    end type exponential
    
    interface exponential
        module procedure new_exponential
    end interface exponential

contains
    function new_exponential()
        implicit none
        type(exponential) :: new_exponential
        new_exponential%fname = "exponential"
        new_exponential%n_in = 1
        new_exponential%n_out = 1
    end function new_exponential

    function forward_exponential(this, v_in) result(v_out)
        implicit none
        class(exponential) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable :: v_out(:,:)
        v_out = exp(v_in)
    end function forward_exponential

    function backward_exponential(this, g_in) result(g_outs)
        implicit none
        class(exponential) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        g_outs(1)%g = g_in * vstack(this%id_out_1)%v
    end function backward_exponential

end module mod_exponential