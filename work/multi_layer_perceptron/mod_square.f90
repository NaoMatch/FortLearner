module mod_square
    use mod_const
    use mod_neural_network_utils
    use mod_variable
    implicit none
    
contains

    function square_var_var(var_in, layers) result(var_out)
        implicit none
        type(variable), target    :: var_out
        type(variable), target    :: var_in
        type(layer_ptr), allocatable, target :: layers(:)
        type(layer_ptr), target       :: new_layer
        integer(kind=8) :: n_idx, n

        ! compute
        var_out%x_shape = shape(var_in%x)
        var_out%x = var_in%x ** 2d0

        ! set layer information
        call allocate_new_layer(layers, new_layer, n_idx)
        call is_multi_inout(layers, n_idx, f_, f_)
        call set_layer_inout_shape(layers, n_idx, shape(var_in%x), shape(var_out%x))
        call set_layer_type(layers, n_idx, "square")

        ! link: output <-> creator
        call var_out%set_creator(layers, n_idx)

        ! link: input <-> creator
        allocate(layers(n_idx)%ptr)
        call layers(n_idx)%ptr%set_input(var_in)

        ! link: output <-> creator
        ! call layers(n_idx)%set_output_var(var_out)
        allocate(layers(n_idx)%ptr%var_out); layers(n_idx)%ptr%var_out => var_out
        layers(n_idx)%ptr%var_out%x_shape = var_out%x_shape
    end function square_var_var

    ! allocate(layers(n_idx)%ptr%vars_in(size(vars_in)));  
    ! layers(n_idx)%ptr%vars_in(1:)  => vars_in(1:)
    ! allocate(layers(n_idx)%ptr%vars_out(size(vars_in))); 
    ! layers(n_idx)%ptr%vars_out(1:) => vars_out(1:)
end module mod_square
