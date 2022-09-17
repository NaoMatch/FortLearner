module mod_exponential
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: exponential_base
    contains
        procedure :: forward  => forward_exponential
        procedure :: backward => backward_exponential
    end type exponential_base
    type(exponential_base) :: exponential
        
    interface exp
        module procedure :: exp_var
    end interface exp
contains

    function forward_exponential(this, input_var) result(output_var)
        implicit none
        class(exponential_base) :: this
        type(variable_) :: input_var
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("exponential")

        ! Operation
        allocate(output_var%v, source=input_var%v)
        output_var%v = exp(input_var%v)
        output_var%var = exp(input_var%var)

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var=input_var, output_var=output_var, dim=-1_8)
    end function forward_exponential
    
    subroutine backward_exponential(this, elm)
        implicit none
        class(exponential_base) :: this
        type(element)      :: elm

        type(variable_), pointer :: input_var_ptr
        type(variable_), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        if (input_var_ptr%grd%dtype==-1) then
            input_var_ptr%grd = output_var_ptr%grd * exp(input_var_ptr%var)
        else
            input_var_ptr%grd = input_var_ptr%grd + output_var_ptr%grd * exp(input_var_ptr%var)
        end if        
    end subroutine backward_exponential

    function exp_var(input_var) result(output_var)
        implicit none
        type(variable_), intent(in) :: input_var
        type(variable_) :: output_var
        output_var = exponential%forward(input_var)
    end function exp_var
end module mod_exponential