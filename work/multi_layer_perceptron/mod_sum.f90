module mod_sum
    use mod_const
    use mod_neural_network_utils
    use mod_variable
    implicit none
    
contains

    function sum_var_var(vars_in, layers) result(var_out)
        implicit none
        type(variable), target    :: vars_in(2)
        type(variable), target    :: var_out
        type(layer_ptr), allocatable, target :: layers(:)
        type(layer_ptr), target       :: new_layer
        integer(kind=8) :: n_idx, n

        ! compute
        var_out%x = vars_in(1)%x + vars_in(2)%x

        ! set layer information
        call allocate_new_layer(layers, new_layer, n_idx)
        call is_multi_inout(layers, n_idx, t_, f_)
        call set_layer_inout_shape(layers, n_idx, shape(vars_in(1)%x), shape(var_out%x))
        call set_layer_type(layers, n_idx, "sum")

        ! link: output <-> creator
        call var_out%set_creator(layers, n_idx)

        ! link: input <-> creator
        call layers(n_idx)%ptr%set_input(vars_in)

        ! link: output <-> creator
        call layers(n_idx)%set_output_var(var_out)
    end function sum_var_var

end module mod_sum
