module mod_layer
    use mod_variable
    implicit none
    
    type layer 
        type(variable), pointer :: var_in
        type(variable), pointer :: var_out
        character(len=256)      :: layer_type
        integer(kind=8)         :: dim_in, dim_out
    end type layer

end module mod_layer
