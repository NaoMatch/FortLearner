module mod_add
    use mod_variable
    implicit none
    
contains

    function square_var_var_sub_2(vars_in, layers) result(var_out)
        implicit none
        type(variable), target    :: var_out
        type(variable), target    :: vars_in(1)
        type(layer_ptr), allocatable, target :: layers(:)
        type(layer_ptr), target       :: add_layer
        integer(kind=8) :: n_idx

        ! compute
        var_out%x = vars_in(1)%x**2d0

        ! set layer information
        n_idx = size(layers(:))+1
        layers = [layers, add_layer]
        allocate(layers(n_idx)%ptr)
        layers(n_idx)%ptr%dim_in = 1
        layers(n_idx)%ptr%dim_out = 1
        layers(n_idx)%ptr%layer_type = "add"

        ! link
        allocate(var_out%creator_ptr);       var_out%creator_ptr       => layers(n_idx)
        allocate(layers(n_idx)%ptr%var_in);  layers(n_idx)%ptr%var_in  => vars_in
        allocate(layers(n_idx)%ptr%var_out); layers(n_idx)%ptr%var_out => var_out
    end function square_var_var_sub_2

end module mod_add
