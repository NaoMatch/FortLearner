module mod_two_layer_model
    use mod_variable
    use mod_intrinsics
    use mod_functions
    use mod_layers
    implicit none

    type, extends(base_model) :: two_layer_model
        type(linear) :: l1
        type(linear) :: l2
    contains
        procedure :: forward => forward_two_layer_model
    end type two_layer_model

    interface two_layer_model
        module procedure :: new_two_layer_model
    end interface two_layer_model
    
contains

    function new_two_layer_model(hidden_size, out_size) result(model)
        implicit none
        type(two_layer_model) :: model
        integer(kind=8) :: hidden_size, out_size

        model%l1 = linear(out_size=hidden_size, no_bias=.false.)
        model%l2 = linear(out_size=out_size, no_bias=.false.)
    end function new_two_layer_model


    function forward_two_layer_model(this, var_in) result(var_out)
        implicit none
        class(two_layer_model) :: this
        type(variable) :: var_in
        type(variable) :: var_out
        var_out = this%l1%act(var_in)
        var_out = sigmoid_(var_out)
        var_out = this%l2%act(var_out)
    end function forward_two_layer_model
    
end module mod_two_layer_model