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

    function forward_my_mlp(this, input_vars) result(output_vars)
        implicit none
        class(my_mlp) :: this
        type(variable_) :: input_vars(2)
        type(variable_), allocatable :: output_vars(:)
        type(variable_) :: var_add, var_sub, var_mul, var_div
        type(variable_) :: var_exp, var_log, var_cos, var_sin
        type(variable_) :: var_sq, var_sqr, var_tan
        type(variable_) :: var_asin, var_acos, var_atan
        integer(kind=8) :: id
        call this%init(input_vars)

        var_add = addition%forward(input_vars(1), input_vars(2))
        var_sub = substraction%forward(input_vars(1), input_vars(2))

        var_mul = multiplication%forward(var_add, var_sub)
        var_div = division%forward(var_mul, var_sub)

        var_exp = exponential%forward(var_div)
        var_log = log_natural%forward(var_div)
        var_cos = cosine%forward(var_log)
        var_sin = sinusoid%forward(var_cos)
        var_sq = square%forward(var_sin)
        var_sqr = square_root%forward(var_sq)
        var_tan = tangent%forward(var_sqr)
        var_asin = arcsinusoid%forward(var_tan)
        var_acos = arccosine%forward(var_asin)
        var_atan = arctangent%forward(var_acos)

        allocate(output_vars(1))
        output_vars = var_atan
    end function forward_my_mlp


end module mod_my_mlp