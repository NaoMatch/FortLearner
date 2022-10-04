module mod_my_mlp
    use mod_random
    use mod_const
    use mod_wengert_list
    include "./inc_use_activation_functions.f90"
    implicit none

    type, extends(neural_network) :: my_mlp
        type(dense) :: dense1 = dense(1_8, 10_8, bias=t_)
        type(dense) :: dense2 = dense(10_8, 1_8, bias=t_)
    contains
        procedure :: forward => forward_my_mlp
    end type my_mlp

    interface my_mlp
        module procedure new_my_mlp
    end interface my_mlp

contains

    function new_my_mlp(opt)
        implicit none
        type(my_mlp) :: new_my_mlp
        type(optimizer), target :: opt
        new_my_mlp%opt_ptr => opt
        call new_my_mlp%dense1%init()
        call new_my_mlp%dense2%init()
    end function new_my_mlp

    function forward_my_mlp(this, input_var) result(output_var)
        implicit none
        class(my_mlp) :: this
        type(variable), target :: input_var
        type(variable) :: output_var, h

        ! Preprocess
        call this%preprocess(input_var)

        h = this%dense1%act(input_var)
        h = sigmoid(h)
        h = square(h)
        output_var = this%dense2%act(h)

        ! Postprocess
        call this%postprocess(output_var)
    end function forward_my_mlp
    
end module mod_my_mlp