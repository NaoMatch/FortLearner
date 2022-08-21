module mod_my_mlp
    use mod_var
    use mod_wengert_list
    use mod_square
    use mod_sinusoid
    implicit none

    type, extends(neural_network) :: my_mlp
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
        type(optimizer_) :: opt
        new_my_mlp%opt = opt
    end function new_my_mlp

    function forward_my_mlp(this, input_var) result(output_vars)
        implicit none
        class(my_mlp) :: this
        type(variable_) :: input_var
        type(variable_), allocatable :: output_vars(:)
        type(variable_) :: var_sq, var_sin, var
        integer(kind=8) :: id
        call this%init(input_var)

        var = square%forward(input_var)
        var = sinusoid%forward(var)

        allocate(output_vars(1))
        output_vars = var
    end function forward_my_mlp


end module mod_my_mlp