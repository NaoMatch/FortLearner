module mod_sigmoid
    use mod_variable
    implicit none

    type, extends(base_function) :: sigmoid
    contains
        procedure :: forward_1in_1out => forward_sigmoid
        procedure :: backward_1in_2out => backward_sigmoid
    end type sigmoid
    
    interface sigmoid
        module procedure new_sigmoid
    end interface sigmoid

contains


    function sigmoid_(var_in) result(var_out)
        implicit none
        type(variable) :: var_in
        type(variable) :: var_out

        type(sigmoid) :: sig

        sig = sigmoid()

        var_out = sig%act(var_in)
    end function sigmoid_


    function new_sigmoid()
        implicit none
        type(sigmoid) :: new_sigmoid
        new_sigmoid%fname = "sigmoid"
        new_sigmoid%n_in = 1
        new_sigmoid%n_out = 1
    end function new_sigmoid

    function forward_sigmoid(this, v_in) result(v_out)
        implicit none
        class(sigmoid) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable :: v_out(:,:)
        v_out = 1d0 / (1d0 + exp(-v_in))
    end function forward_sigmoid

    function backward_sigmoid(this, g_in) result(g_outs)
        implicit none
        class(sigmoid) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        g_outs(1)%g = g_in * vstack(this%id_out_1)%v * (1d0 - vstack(this%id_out_1)%v)
    end function backward_sigmoid

end module mod_sigmoid