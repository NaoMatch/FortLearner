module mod_my_mlp
    use mod_var
    use mod_wengert_list
    include "./inc_use_activation_functions.f90"
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
        type(variable_) :: var_add, var_sub, var_mul, var_div
        type(variable_) :: var_exp, var_log, var_cos, var_sin
        type(variable_) :: var_sq, var_sqr, var_tan
        type(variable_) :: var_asin, var_acos, var_atan
        type(variable_) :: var_out, var_sq_sq_1, var_sq_sq_2
        integer(kind=8) :: id
        call this%init(input_var)

        var_sq = square%forward(input_var)
        var_sq_sq_1 = square%forward(var_sq)
        var_sq_sq_2 = square%forward(var_sq)
        var_add = addition%forward(var_sq_sq_1, var_sq_sq_2)

        allocate(output_vars(1))
        output_vars = var_add

    end function forward_my_mlp


end module mod_my_mlp