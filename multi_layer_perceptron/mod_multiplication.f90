module mod_multiplication
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: multiplication_base
    contains
        procedure :: forward  => forward_multiplication
        procedure :: backward => backward_multiplication
    end type multiplication_base
    type(multiplication_base) :: multiplication
    
contains
    function forward_multiplication(this, input_var1, input_var2) result(output_var)
        implicit none
        class(multiplication_base) :: this
        type(variable_) :: input_var1, input_var2, input_vars(2)
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("multiplication")

        ! Operation
        output_var%v = input_var1%v * input_var2%v

        ! Append 'variables' to Stack
        input_vars = [input_var1, input_var2]
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var1=input_var1, input_var2=input_var2, output_var=output_var)
    end function forward_multiplication

    subroutine backward_multiplication(this, elm)
        implicit none
        class(multiplication_base) :: this
        type(element)      :: elm

        type(variable_), pointer :: input_var1_ptr, input_var2_ptr
        type(variable_), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var1_ptr, input_var2_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        print*, '*********************************************************************************************'
        print*, " ---- Multiplication Backward"
        print*, "      input_var1_ptr%g: ", allocated(input_var1_ptr%g)
        print*, "      input_var2_ptr%g: ", allocated(input_var2_ptr%g)

        input_var1_ptr%g = input_var2_ptr%v * output_var_ptr%g
        input_var2_ptr%g = input_var1_ptr%v * output_var_ptr%g
    end subroutine backward_multiplication    

end module mod_multiplication