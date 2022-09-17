module mod_absolute_value
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: absolute_value_base
    contains
        procedure :: forward  => forward_absolute_value
        procedure :: backward => backward_absolute_value
    end type absolute_value_base
    type(absolute_value_base) :: absolute_value

    interface abs
        module procedure :: abs_var
    end interface abs
    
contains

    function forward_absolute_value(this, input_var) result(output_var)
        implicit none
        class(absolute_value_base) :: this
        type(variable_) :: input_var
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("absolute_value")

        ! Operation
        output_var%var = abs(input_var%var)

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var=input_var, output_var=output_var, dim=-1_8)
    end function forward_absolute_value
    
    subroutine backward_absolute_value(this, elm)
        implicit none
        class(absolute_value_base) :: this
        type(element)      :: elm

        type(variable_), pointer :: input_var_ptr
        type(variable_), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        if (input_var_ptr%grd%dtype==-1) then
            input_var_ptr%grd = output_var_ptr%grd * input_var_ptr%var / abs(input_var_ptr%var)
        else
            input_var_ptr%grd = input_var_ptr%grd + output_var_ptr%grd * input_var_ptr%var / abs(input_var_ptr%var)
        end if
    end subroutine backward_absolute_value

    function abs_var(input_var) result(output_var)
        implicit none
        type(variable_) :: input_var
        type(variable_) :: output_var
        output_var = absolute_value%forward(input_var)
    end function abs_var


end module mod_absolute_value