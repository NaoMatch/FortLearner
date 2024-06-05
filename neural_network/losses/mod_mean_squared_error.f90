module mod_mean_squared_error
    use mod_variable
    implicit none

    type, extends(base_function) :: mean_squared_error
    contains
        procedure :: forward_2in_1out => forward_mean_squared_error
        procedure :: backward_1in_2out => backward_mean_squared_error
    end type mean_squared_error
    
    interface mean_squared_error
        module procedure new_mean_squared_error
    end interface mean_squared_error

contains

    function mean_squared_error_(var_in_1, var_in_2) result(var_out)
        implicit none
        type(variable), intent(in) :: var_in_1, var_in_2
        type(variable) :: var_out
        type(mean_squared_error) :: mse_
        mse_ = mean_squared_error()
        var_out = mse_%act(var_in_1, var_in_2)
    end function mean_squared_error_

    function new_mean_squared_error()
        implicit none
        type(mean_squared_error) :: new_mean_squared_error
        new_mean_squared_error%fname = "mean_squared_error"
        new_mean_squared_error%n_in = 2
        new_mean_squared_error%n_out = 1
    end function new_mean_squared_error

    subroutine forward_mean_squared_error(this, v_out, v_in_1, v_in_2)
        implicit none
        class(mean_squared_error) :: this
        real(kind=8), intent(in) :: v_in_1(:,:), v_in_2(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)

        v_out = reshape([sum( (v_in_1 - v_in_2)**2d0 ) / size(v_in_1, dim=1)], [1,1])
    end subroutine forward_mean_squared_error

    function backward_mean_squared_error(this, g_in) result(g_outs)
        implicit none
        class(mean_squared_error) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)

        integer(kind=8) :: shape1(2), shape2(2)

        real(kind=8), allocatable :: diff(:,:)

        diff = vstack(this%id_in_1)%v - vstack(this%id_in_2)%v

        g_outs(1)%g = g_in(1,1) * diff(:,:) * (2d0 / size(diff, dim=1))
        g_outs(2)%g = - g_outs(1)%g
    end function backward_mean_squared_error

end module mod_mean_squared_error