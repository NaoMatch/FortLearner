module mod_addition
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: addition_base
    contains
        procedure :: forward  => forward_addition
        procedure :: backward => backward_addition
    end type addition_base
    type(addition_base) :: addition
    
contains
    function forward_addition(this, input_var1, input_var2) result(output_var)
        implicit none
        class(addition_base) :: this
        type(variable_) :: input_var1, input_var2, input_vars(2)
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("addition")

        ! Operation
        output_var%v = input_var1%v + input_var2%v

        ! Append 'variables' to Stack
        input_vars = [input_var1, input_var2]
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var1=input_var1, input_var2=input_var2, output_var=output_var)
    end function forward_addition

    subroutine backward_addition(this, elm)
        implicit none
        class(addition_base) :: this
        type(element)      :: elm

        type(variable_), pointer :: input_var1_ptr, input_var2_ptr
        type(variable_), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var1_ptr, input_var2_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        ! print*, '*********************************************************************************************'
        ! print*, " ---- Addition Backward"
        ! print*, "      input_var1_ptr%g: ", allocated(input_var1_ptr%g)
        ! print*, "      input_var2_ptr%g: ", allocated(input_var2_ptr%g)

        if (allocated(input_var1_ptr%g)) then
            input_var1_ptr%g = input_var1_ptr%g + output_var_ptr%g
        else
            input_var1_ptr%g =                    output_var_ptr%g
        end if

        if (allocated(input_var2_ptr%g)) then
            input_var2_ptr%g = input_var2_ptr%g + output_var_ptr%g
        else
            input_var2_ptr%g =                    output_var_ptr%g
        end if
        ! print*, input_var1_ptr%g        
        ! print*, input_var2_ptr%g        
    end subroutine backward_addition    


end module mod_addition