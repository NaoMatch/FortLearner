module mod_tangent
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: tangent_base
    contains
        procedure :: forward  => forward_tangent
        procedure :: backward => backward_tangent
    end type tangent_base
    type(tangent_base) :: tangent

    interface tan
        module procedure :: tan_var
    end interface tan    
    
contains

    function forward_tangent(this, input_var) result(output_var)
        implicit none
        class(tangent_base) :: this
        type(variable) :: input_var
        type(variable) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("tangent")

        ! Operation
        output_var%var = tan(input_var%var)

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var=input_var, output_var=output_var, dim=-1_8)
    end function forward_tangent
    
    subroutine backward_tangent(this, elm)
        implicit none
        class(tangent_base) :: this
        type(element)      :: elm

        type(variable), pointer :: input_var_ptr
        type(variable), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        if (allocated(input_var_ptr%g)) then
            input_var_ptr%g = input_var_ptr%g + output_var_ptr%g / cos(input_var_ptr%v)**2d0
        else
            input_var_ptr%g = output_var_ptr%g / cos(input_var_ptr%v)**2d0
        end if
    end subroutine backward_tangent


    function tan_var(input_var) result(output_var)
        implicit none
        type(variable) :: input_var
        type(variable) :: output_var
        output_var = tangent%forward(input_var)
    end function tan_var

end module mod_tangent