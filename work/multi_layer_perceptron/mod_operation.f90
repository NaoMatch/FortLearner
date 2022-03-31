module mod_operation
    use mod_layer_stacker
    use mod_variable

    implicit none

    interface add
        module procedure :: add_var_var
    end interface add

contains

    function add_var_var(stacker, var_a, var_b) result(r)
        implicit none
        type(layer_stacker), intent(inout)    :: stacker
        type(variable), intent(inout), target :: var_a, var_b
        type(variable)                        :: r
        type(variable_ptr)                    :: var_ptr_a, var_ptr_b

        ! Compute
        r%x = var_a%x + var_b%x

        ! Link
        allocate(var_ptr_a%ptr); var_ptr_a%ptr => var_a
        allocate(var_ptr_b%ptr); var_ptr_b%ptr => var_b
        stacker%var_ptrs = [stacker%var_ptrs, var_ptr_a, var_ptr_b]
    end function add_var_var

end module mod_operation
