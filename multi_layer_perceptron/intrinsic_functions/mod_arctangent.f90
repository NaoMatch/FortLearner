module mod_arctangent
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: arctangent_base
    contains
        procedure :: forward  => forward_arctangent
        procedure :: backward => backward_arctangent
    end type arctangent_base
    type(arctangent_base) :: arctangent

    interface atan
        module procedure :: atan_var
    end interface atan    
contains

    function forward_arctangent(this, input_var) result(output_var)
        implicit none
        class(arctangent_base) :: this
        type(variable) :: input_var
        type(variable) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("arctangent")

        ! Operation
        output_var%v = atan(input_var%v)
        output_var%var = atan(input_var%var)

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var=input_var, output_var=output_var, dim=-1_8)
    end function forward_arctangent
    
    subroutine backward_arctangent(this, elm)
        implicit none
        class(arctangent_base) :: this
        type(element)      :: elm

        type(variable), pointer :: input_var_ptr
        type(variable), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        if (input_var_ptr%grd%dtype==-1) then
            input_var_ptr%grd = output_var_ptr%grd / (1d0 + input_var_ptr%var**2_8)
        else
            input_var_ptr%grd = input_var_ptr%grd + output_var_ptr%grd / (1d0 + input_var_ptr%var**2_8)
        end if        
    end subroutine backward_arctangent

    function atan_var(input_var) result(output_var)
        implicit none
        type(variable) :: input_var
        type(variable) :: output_var
        output_var = arctangent%forward(input_var)
    end function atan_var       
end module mod_arctangent