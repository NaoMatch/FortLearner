module mod_substraction
    use mod_variable
    implicit none

    type, extends(base_function) :: substraction
    contains
        procedure :: forward_2in_1out => forward_substraction
        procedure :: backward_1in_2out => backward_substraction
    end type substraction
    
    interface substraction
        module procedure new_substraction
    end interface substraction

    interface operator(-)
        module procedure substraction_var_var
        module procedure substraction_var_r8
        module procedure substraction_r8_var
        module procedure substraction_var
    end interface operator(-)

contains

    function substraction_var_var(var_in_1, var_in_2) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in_1, var_in_2
        type(variable) :: var_out
        type(substraction) :: sub
        sub = substraction()
        var_out = sub%act(var_in_1, var_in_2)
    end function substraction_var_var    

    function substraction_var_r8(var_in, scl) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in
        real(kind=8), intent(in) :: scl
        type(variable) :: var_out
        type(variable) :: var_scl
        type(substraction) :: sub
        var_scl = variable(scl)
        sub = substraction()
        var_out = sub%act(var_in, var_scl)
    end function substraction_var_r8    

    function substraction_r8_var(scl, var_in) result(var_out)
        implicit none
        real(kind=8), intent(in) :: scl
        type(variable), intent(in) :: var_in
        type(variable) :: var_out
        type(variable) :: var_scl
        type(substraction) :: sub
        var_scl = variable(scl)
        sub = substraction()
        var_out = sub%act(var_scl, var_in)
    end function substraction_r8_var    

    function substraction_var(var_in) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in
        type(variable) :: var_out
        type(variable) :: var_zero
        type(substraction) :: sub
        var_zero = variable(0d0)
        sub = substraction()
        var_out = sub%act(var_zero, var_in)
    end function substraction_var    

    function new_substraction()
        implicit none
        type(substraction) :: new_substraction
        new_substraction%fname = "substraction"
        new_substraction%n_in = 2
        new_substraction%n_out = 1
    end function new_substraction

    subroutine forward_substraction(this, v_out, v_in_1, v_in_2)
        implicit none
        class(substraction) :: this
        real(kind=8), intent(in) :: v_in_1(:,:), v_in_2(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)
        v_out = v_in_1 - v_in_2
    end subroutine forward_substraction

    function backward_substraction(this, g_in) result(g_outs)
        implicit none
        class(substraction) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        g_outs(1)%g = g_in
        g_outs(2)%g = -g_in
    end function backward_substraction

end module mod_substraction