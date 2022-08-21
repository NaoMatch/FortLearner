module mod_arcsinusoid
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: arcsinusoid_base
    contains
        procedure :: forward  => forward_arcsinusoid
        procedure :: backward => backward_arcsinusoid
    end type arcsinusoid_base
    type(arcsinusoid_base) :: arcsinusoid
    
contains

    function forward_arcsinusoid(this, input_var) result(output_var)
        implicit none
        class(arcsinusoid_base) :: this
        type(variable_) :: input_var
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("arcsinusoid")

        ! Operation
        output_var%v = asin(input_var%v)

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_vars=input_var, output_var=output_var)
    end function forward_arcsinusoid
    
    subroutine backward_arcsinusoid(this, elm)
        implicit none
        class(arcsinusoid_base) :: this
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
        input_var_ptr%g = output_var_ptr%g / sqrt(1d0-input_var_ptr%v**2d0)
    end subroutine backward_arcsinusoid
end module mod_arcsinusoid