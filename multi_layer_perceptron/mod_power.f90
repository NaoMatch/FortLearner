module mod_power
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: power_base
    contains
        procedure :: forward  => forward_power
        procedure :: backward => backward_power
    end type power_base
    type(power_base) :: power

    interface operator(**)
        module procedure power_var_var
        module procedure power_var_scl
    end interface operator(**)
    
contains

    function forward_power(this, input_var1, input_var2) result(output_var)
        implicit none
        class(power_base) :: this
        type(variable_) :: input_var1, input_var2, input_vars(2)
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("power")

        ! Operation
        output_var%v = input_var1%v ** input_var2%v

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var1=input_var1, input_var2=input_var2, output_var=output_var)
    end function forward_power


    subroutine backward_power(this, elm)
        implicit none
        class(power_base) :: this
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
            input_var1_ptr%g = input_var1_ptr%g + input_var2_ptr%v * (input_var1_ptr%v**(input_var2_ptr%v-1)) * output_var_ptr%g
        else
            input_var1_ptr%g =                    input_var2_ptr%v * (input_var1_ptr%v**(input_var2_ptr%v-1)) * output_var_ptr%g
        end if
        ! print*, input_var1_ptr%g        
        ! print*, input_var2_ptr%g        
    end subroutine backward_power    


    function power_var_var(input_var1, input_var2) result(output_var)
        implicit none
        type(variable_), intent(in) :: input_var1, input_var2
        type(variable_) :: output_var
        output_var = power%forward(input_var1, input_var2)
    end function power_var_var


    function power_var_scl(input_var, input_scl) result(output_var)
        implicit none
        type(variable_), intent(in) :: input_var
        real(kind=8), intent(in)    :: input_scl
        type(variable_) :: input_scl_var
        type(variable_) :: output_var
        input_scl_var = variable_(input_scl, stack_id=input_var%stack_id)
        output_var = power%forward(input_var, input_scl_var)
    end function power_var_scl

end module mod_power