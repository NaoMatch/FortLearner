module mod_power
    use mod_variable
    implicit none

    type, extends(base_function) :: power
    contains
        procedure :: forward_1in_1out => forward_power
        procedure :: backward_1in_2out => backward_power
    end type power
    
    interface power
        module procedure new_power_i8
        module procedure new_power_i4
        module procedure new_power_r8
        module procedure new_power_r4
    end interface power

    interface operator(**)
        module procedure power_var_i8
        module procedure power_var_i4
        module procedure power_var_r8
        module procedure power_var_r4
    end interface operator(**)

contains

    function power_var_i8(var_in, scl) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in
        integer(kind=8), intent(in) :: scl
        type(variable) :: var_out
        type(power) :: pow
        pow = power(pow=scl)
        var_out = pow%act(var_in)
    end function power_var_i8    

    function power_var_i4(var_in, scl) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in
        integer(kind=4), intent(in) :: scl
        type(variable) :: var_out
        type(power) :: pow
        pow = power(pow=scl)
        var_out = pow%act(var_in)
    end function power_var_i4    

    function power_var_r8(var_in, scl) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in
        real(kind=8), intent(in) :: scl
        type(variable) :: var_out
        type(power) :: pow
        pow = power(pow=scl)
        var_out = pow%act(var_in)
    end function power_var_r8    

    function power_var_r4(var_in, scl) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in
        real(kind=4), intent(in) :: scl
        type(variable) :: var_out
        type(power) :: pow
        pow = power(pow=scl)
        var_out = pow%act(var_in)
    end function power_var_r4    

    function new_power_i8(pow)
        implicit none
        type(power) :: new_power_i8
        integer(kind=8) :: pow
        new_power_i8%fname = "power"
        new_power_i8%n_in = 2
        new_power_i8%n_out = 1
        new_power_i8%pow = pow
    end function new_power_i8

    function new_power_i4(pow)
        implicit none
        type(power) :: new_power_i4
        integer(kind=4) :: pow
        new_power_i4%fname = "power"
        new_power_i4%n_in = 2
        new_power_i4%n_out = 1
        new_power_i4%pow = pow
    end function new_power_i4

    function new_power_r8(pow)
        implicit none
        type(power) :: new_power_r8
        real(kind=8) :: pow
        new_power_r8%fname = "power"
        new_power_r8%n_in = 2
        new_power_r8%n_out = 1
        new_power_r8%pow = pow
    end function new_power_r8

    function new_power_r4(pow)
        implicit none
        type(power) :: new_power_r4
        real(kind=4) :: pow
        new_power_r4%fname = "power"
        new_power_r4%n_in = 2
        new_power_r4%n_out = 1
        new_power_r4%pow = pow
    end function new_power_r4

    subroutine forward_power(this, v_out, v_in)
        implicit none
        class(power) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)
        v_out = v_in**this%pow
    end subroutine forward_power

    function backward_power(this, g_in) result(g_outs)
        implicit none
        class(power) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        g_outs(1)%g = this%pow &
            * (vstack(this%id_in_1)%v ** (this%pow-1)) &
            * g_in
    end function backward_power

end module mod_power