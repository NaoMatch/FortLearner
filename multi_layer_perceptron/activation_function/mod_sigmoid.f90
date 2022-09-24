module mod_sigmoid
    use mod_variable_in_variable
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: sigmoidal_base
    contains
        procedure :: forward  => forward_sigmoidal
        procedure :: backward => backward_sigmoidal
    end type sigmoidal_base
    type(sigmoidal_base) :: sigmoidal
        
    interface sigmoid
        module procedure :: sigmoid_var
    end interface sigmoid
contains

    function forward_sigmoidal(this, input_var) result(output_var)
        implicit none
        class(sigmoidal_base) :: this
        type(variable) :: input_var
        type(variable) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("sigmoidal")

        ! Operation
        output_var%var = tanh(input_var%var * 0.5d0) * 0.5d0 + 0.5d0
        
        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var=input_var, output_var=output_var, dim=-1_8)
    end function forward_sigmoidal
    
    subroutine backward_sigmoidal(this, elm)
        implicit none
        class(sigmoidal_base) :: this
        type(element)      :: elm

        type(variable), pointer :: input_var_ptr
        type(variable), pointer :: output_var_ptr
        type(variable) :: out_var
        call get_input_variable_pointer(elm, input_var_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        call debug_print(__FILE__, __LINE__, &
                elm, input_var_ptr, output_var_ptr, t_)

        out_var%var = tanh(input_var_ptr%var * 0.5d0) * 0.5d0 + 0.5d0
        if (input_var_ptr%grd%dtype==-1) then
            input_var_ptr%grd = out_var%var * output_var_ptr%grd
        else
            input_var_ptr%grd = input_var_ptr%grd + out_var%var * output_var_ptr%grd
        end if

        call debug_print(__FILE__, __LINE__, &
                elm, input_var_ptr, output_var_ptr, f_)        
    end subroutine backward_sigmoidal

    function sigmoid_var(input_var) result(output_var)
        implicit none
        type(variable), intent(in) :: input_var
        type(variable) :: output_var
        output_var = sigmoidal%forward(input_var)
    end function sigmoid_var

end module mod_sigmoid