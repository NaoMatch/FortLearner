module mod_relu
    use mod_variable
    implicit none

    type, extends(base_function) :: relu
    contains
        procedure :: forward_1in_1out => forward_relu
        procedure :: backward_1in_2out => backward_relu
    end type relu
    
    interface relu
        module procedure new_relu
    end interface relu

contains

    function relu_(var_in) result(var_out)
        implicit none
        type(variable) :: var_in
        type(variable) :: var_out

        type(relu) :: rec

        rec = relu()

        var_out = rec%act(var_in)
    end function relu_

    function new_relu()
        implicit none
        type(relu) :: new_relu
        new_relu%fname = "relu"
        new_relu%n_in = 1
        new_relu%n_out = 1
    end function new_relu

    elemental function zero_rectifier(x) result(y)
        implicit none
        real(kind=8), intent(in) :: x
        real(kind=8) :: y

        y = maxval([x, 0d0])
    end function zero_rectifier

    elemental function flag_rectifier(x) result(y)
        implicit none
        real(kind=8), intent(in) :: x
        real(kind=8) :: y
        integer(kind=8) :: flg

        flg = x>0d0

        y = dble(flg)
    end function flag_rectifier

    function forward_relu(this, v_in) result(v_out)
        implicit none
        class(relu) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable :: v_out(:,:)
        v_out = zero_rectifier(v_in)
    end function forward_relu

    function backward_relu(this, g_in) result(g_outs)
        implicit none
        class(relu) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        g_outs(1)%g = g_in * flag_rectifier(vstack(this%id_out_1)%v)
    end function backward_relu

end module mod_relu