module mod_relu
    use mod_variable_in_variable
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: relu_base
    contains
        procedure :: forward  => forward_relu
        procedure :: backward => backward_relu
    end type relu_base
    type(relu_base) :: relu_function
        
    interface relu
        module procedure :: relu_var
    end interface relu
contains


        

    function forward_relu(this, input_var) result(output_var)
        implicit none
        class(relu_base) :: this
        type(variable_) :: input_var
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("relu")

        ! Operation
        output_var%var = relu(input_var%var)
        
        ! print*, __FILE__, __LINE__, 4
        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var=input_var, output_var=output_var, dim=-1_8)
    end function forward_relu
    
    subroutine backward_relu(this, elm)
        implicit none
        class(relu_base) :: this
        type(element)      :: elm

        type(variable_), pointer :: input_var_ptr
        type(variable_), pointer :: output_var_ptr
        type(variable_) :: out_var
        call get_input_variable_pointer(elm, input_var_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        call debug_print(__FILE__, __LINE__, elm, input_var_ptr, output_var_ptr, t_)        
        out_var%var = clip(input_var_ptr%var, 1d0)
        if (input_var_ptr%grd%dtype==-1) then
            input_var_ptr%grd = out_var%var * output_var_ptr%grd
        else
            input_var_ptr%grd = input_var_ptr%grd + out_var%var * output_var_ptr%grd
        end if
        call debug_print(__FILE__, __LINE__, elm, input_var_ptr, output_var_ptr, f_)        
    end subroutine backward_relu

    function relu_var(input_var) result(output_var)
        implicit none
        type(variable_), intent(in) :: input_var
        type(variable_) :: output_var
        output_var = relu_function%forward(input_var)
    end function relu_var

end module mod_relu