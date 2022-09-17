module mod_sinusoid
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: sinusoid_base
    contains
        procedure :: forward  => forward_sinusoid
        procedure :: backward => backward_sinusoid
    end type sinusoid_base
    type(sinusoid_base) :: sinusoid
    
contains

    function forward_sinusoid(this, input_var) result(output_var)
        implicit none
        class(sinusoid_base) :: this
        type(variable_) :: input_var
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("sinusoid")

        ! Operation
        output_var%v = sin(input_var%v)
        output_var%var = sin(input_var%var)

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var=input_var, output_var=output_var, dim=-1_8)
    end function forward_sinusoid

    subroutine backward_sinusoid(this, elm)
        implicit none
        class(sinusoid_base) :: this
        type(element)      :: elm

        type(variable_), pointer :: input_var_ptr
        type(variable_), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        if (allocated(input_var_ptr%g)) then
            input_var_ptr%g = input_var_ptr%g + cos(input_var_ptr%v) * output_var_ptr%g
        else
            input_var_ptr%g =                   cos(input_var_ptr%v) * output_var_ptr%g
        end if
        ! print*, input_var_ptr%g        
    end subroutine backward_sinusoid


end module mod_sinusoid