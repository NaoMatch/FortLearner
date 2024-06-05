module mod_log_natural
    use mod_variable
    implicit none

    type, extends(base_function) :: log_natural
    contains
        procedure :: forward_1in_1out => forward_log_natural
        procedure :: backward_1in_2out => backward_log_natural
    end type log_natural
    
    interface log_natural
        module procedure new_log_natural
    end interface log_natural

contains


    function abs_(var_in) result(var_out)
        implicit none
        type(variable) :: var_in
        type(variable) :: var_out

        type(log_natural) :: log_e_

        log_e_ = log_natural()

        var_out = log_e_%act(var_in)
    end function abs_


    function new_log_natural()
        implicit none
        type(log_natural) :: new_log_natural
        new_log_natural%fname = "log_natural"
        new_log_natural%n_in = 1
        new_log_natural%n_out = 1
    end function new_log_natural

    subroutine forward_log_natural(this, v_out, v_in)
        implicit none
        class(log_natural) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable, intent(inout) :: v_out(:,:)
        v_out = log(v_in)
    end subroutine forward_log_natural

    function backward_log_natural(this, g_in) result(g_outs)
        implicit none
        class(log_natural) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        g_outs(1)%g = g_in / vstack(this%id_in_1)%v
    end function backward_log_natural

end module mod_log_natural