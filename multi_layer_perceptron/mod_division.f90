module mod_division
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: division_base
    contains
        procedure :: forward  => forward_division
        procedure :: backward => backward_division
    end type division_base
    type(division_base) :: division
    
contains
    function forward_division(this, input_var1, input_var2) result(output_var)
        implicit none
        class(division_base) :: this
        type(variable_) :: input_var1, input_var2, input_vars(2)
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("division")

        ! Operation
        output_var%v = input_var1%v / input_var2%v

        ! Append 'variables' to Stack
        input_vars = [input_var1, input_var2]
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var1=input_var1, input_var2=input_var2, output_var=output_var)
    end function forward_division

    subroutine backward_division(this, elm)
        implicit none
        class(division_base) :: this
        type(element)      :: elm

        type(variable_), pointer :: input_var1_ptr, input_var2_ptr
        type(variable_), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var1_ptr, input_var2_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        print*, '*********************************************************************************************'
        print*, " ---- Multiplication Backward"
        print*, "      input_var1_ptr%g: ", allocated(input_var1_ptr%g)
        print*, "      input_var2_ptr%g: ", allocated(input_var2_ptr%g)

        input_var1_ptr%g =                1d0/input_var2_ptr%v      * output_var_ptr%g
        input_var2_ptr%g = - input_var1_ptr%v/input_var2_ptr%v**2d0 * output_var_ptr%g
    end subroutine backward_division    
end module mod_division