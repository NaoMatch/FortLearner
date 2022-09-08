module mod_log_natural
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: log_natural_base
    contains
        procedure :: forward  => forward_log_natural
        procedure :: backward => backward_log_natural
    end type log_natural_base
    type(log_natural_base) :: log_natural
    
contains

    function forward_log_natural(this, input_var) result(output_var)
        implicit none
        class(log_natural_base) :: this
        type(variable_) :: input_var
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("log_natural")

        ! Operation
        if (.not. allocated(output_var%v)) allocate(output_var%v(1,1))
        output_var%v = log(input_var%v)
        output_var%var = log(input_var%var)

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_vars=input_var, output_var=output_var, dim=-1_8)
    end function forward_log_natural
    
    subroutine backward_log_natural(this, elm)
        implicit none
        class(log_natural_base) :: this
        type(element)      :: elm

        type(variable_), pointer :: input_var_ptr
        type(variable_), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        ! print*, '*********************************************************************************************'
        ! print*, " ---- Square Backward"
        ! print*, "      var ids in/out  : ", input_var_ptr%var_id, output_var_ptr%var_id
        ! print*, "      input_var_ptr%v : ", allocated(input_var_ptr%v)
        ! print*, "      input_var_ptr%g : ", allocated(input_var_ptr%g)
        ! print*, "      output_var_ptr%g: ", allocated(output_var_ptr%g)
        if (allocated(input_var_ptr%g)) then
            input_var_ptr%g = input_var_ptr%g + output_var_ptr%g / input_var_ptr%v
        else
            input_var_ptr%g = output_var_ptr%g / input_var_ptr%v
        end if
    end subroutine backward_log_natural
end module mod_log_natural