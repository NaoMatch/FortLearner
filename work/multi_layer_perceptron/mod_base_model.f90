module mod_base_model
    use mod_variable
    implicit none
    
    type base_model
        type(variable_ptr), allocatable :: var_ptrs(:)
        type(layer_ptr),    allocatable :: lyr_ptrs(:)
        type(variable), allocatable :: vars(:)
        type(layer), allocatable    :: lyrs(:)
    contains
    end type base_model


end module mod_base_model
