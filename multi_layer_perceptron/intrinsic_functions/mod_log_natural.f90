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
            
    interface log
        module procedure :: log_var
    end interface log

contains

    function forward_log_natural(this, input_var) result(output_var)
        implicit none
        class(log_natural_base) :: this
        type(variable) :: input_var
        type(variable) :: output_var
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
            input_var=input_var, output_var=output_var, dim=-1_8)
    end function forward_log_natural
    
    subroutine backward_log_natural(this, elm)
        implicit none
        class(log_natural_base) :: this
        type(element)      :: elm

        type(variable), pointer :: input_var_ptr
        type(variable), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        if (allocated(input_var_ptr%g)) then
            input_var_ptr%g = input_var_ptr%g + output_var_ptr%g / input_var_ptr%v
        else
            input_var_ptr%g = output_var_ptr%g / input_var_ptr%v
        end if
        if (input_var_ptr%grd%dtype==-1) then
            input_var_ptr%grd = output_var_ptr%grd / input_var_ptr%var
        else
            input_var_ptr%grd = input_var_ptr%grd + output_var_ptr%grd / input_var_ptr%var
        end if        
    end subroutine backward_log_natural

    function log_var(input_var) result(output_var)
        implicit none
        type(variable), intent(in) :: input_var
        type(variable) :: output_var
        output_var = log_natural%forward(input_var)
    end function log_var    
end module mod_log_natural