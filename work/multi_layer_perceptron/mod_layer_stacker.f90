module mod_layer_stacker
    use mod_variable
    implicit none
    
    type layer_stacker
        type(variable_ptr), allocatable :: var_ptrs(:)
        type(layer_ptr), allocatable    :: lyr_ptrs(:)
        integer(kind=8) :: n_stacked_layers=0
    contains
    end type layer_stacker

end module mod_layer_stacker
