module mod_substraction
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: substraction_base
    contains
        procedure :: forward  => forward_substraction
        procedure :: backward => backward_substraction
    end type substraction_base
    type(substraction_base) :: substraction
    
contains
    function forward_substraction(this, input_var1, input_var2) result(output_var)
        implicit none
        class(substraction_base) :: this
        type(variable_) :: input_var1, input_var2, input_vars(2)
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("substraction")

        ! Operation
        output_var%v = input_var1%v - input_var2%v

        ! Append 'variables' to Stack
        input_vars = [input_var1, input_var2]
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var1=input_var1, input_var2=input_var2, output_var=output_var)
    end function forward_substraction

    subroutine backward_substraction(this, elm)
        implicit none
        class(substraction_base) :: this
        type(element)      :: elm

        type(variable_), pointer :: input_var1_ptr, input_var2_ptr
        type(variable_), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var1_ptr, input_var2_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        ! print*, '*********************************************************************************************'
        ! print*, " ---- Substraction Backward"
        ! print*, "      input_var1_ptr%g: ", allocated(input_var1_ptr%g)
        ! print*, "      input_var2_ptr%g: ", allocated(input_var2_ptr%g)

        if (allocated(input_var1_ptr%g)) then
            input_var1_ptr%g = input_var1_ptr%g + output_var_ptr%g
        else
            input_var1_ptr%g =                    output_var_ptr%g
        end if

        if (allocated(input_var2_ptr%g)) then
            input_var2_ptr%g = input_var2_ptr%g - output_var_ptr%g
        else
            input_var2_ptr%g =                  - output_var_ptr%g
        end if        
        ! print*, input_var1_ptr%g        
        ! print*, input_var2_ptr%g        
    end subroutine backward_substraction    
end module mod_substraction