module mod_absolute_value
    use mod_variable
    implicit none

    type, extends(base_function) :: absolute_value
    contains
        procedure :: forward_1in_1out => forward_absolute_value
        procedure :: backward_1in_2out => backward_absolute_value
    end type absolute_value
    
    interface absolute_value
        module procedure new_absolute_value
    end interface absolute_value

contains


    function abs_(var_in) result(var_out)
        implicit none
        type(variable) :: var_in
        type(variable) :: var_out

        type(absolute_value) :: abs_val

        abs_val = absolute_value()

        var_out = abs_val%act(var_in)
    end function abs_


    function new_absolute_value()
        implicit none
        type(absolute_value) :: new_absolute_value
        new_absolute_value%fname = "absolute_value"
        new_absolute_value%n_in = 1
        new_absolute_value%n_out = 1
    end function new_absolute_value

    subroutine forward_absolute_value(this, v_out, v_in)
        implicit none
        class(absolute_value) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)
        v_out = abs(v_in)
    end subroutine forward_absolute_value

    elemental function sign_(x) result(y)
        implicit none
        real(kind=8), intent(in) :: x
        real(kind=8) :: y

        integer(kind=8) :: flg 

        flg = x>0d0

        y = maxval([-1d0, minval([1d0, 2d0*flg-1d0])])
    end function sign_

    function backward_absolute_value(this, g_in) result(g_outs)
        implicit none
        class(absolute_value) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        g_outs(1)%g = g_in * sign_(vstack(this%id_in_1)%v)
    end function backward_absolute_value

end module mod_absolute_value