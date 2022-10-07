module mod_square_root
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: square_root_base
    contains
        procedure :: forward  => forward_square_root
        procedure :: backward => backward_square_root
    end type square_root_base
    type(square_root_base) :: square_root

    interface sqrt
        module procedure :: sqrt_var
    end interface sqrt    
contains

    function forward_square_root(this, input_var) result(output_var)
        implicit none
        class(square_root_base) :: this
        type(variable) :: input_var
        type(variable) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("square_root")

        ! Operation
        output_var%var = sqrt(input_var%var)

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var=input_var, output_var=output_var, dim=-1_8)
    end function forward_square_root
    
    subroutine backward_square_root(this, elm)
        implicit none
        class(square_root_base) :: this
        type(element)      :: elm

        type(variable), pointer :: input_var_ptr
        type(variable), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        if (input_var_ptr%grd%dtype==-1) then
            input_var_ptr%grd = output_var_ptr%grd * 0.5d0 / sqrt(input_var_ptr%var)
        else
            input_var_ptr%grd = input_var_ptr%grd + output_var_ptr%grd * 0.5d0 / sqrt(input_var_ptr%var)
        end if
    end subroutine backward_square_root

    function sqrt_var(input_var) result(output_var)
        implicit none
        type(variable) :: input_var
        type(variable) :: output_var
        output_var = square_root%forward(input_var)
    end function sqrt_var

end module mod_square_root