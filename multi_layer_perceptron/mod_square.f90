module mod_square
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: square_base
    contains
        procedure :: forward  => forward_square
        procedure :: backward => backward_square
    end type square_base
    type(square_base) :: square
    
contains

    function forward_square(this, input_var) result(output_var)
        implicit none
        class(square_base) :: this
        type(variable_) :: input_var
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("square")

        ! Operation
        output_var%v = input_var%v**2d0
        output_var%var = input_var%var**2_8

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var=input_var, output_var=output_var, dim=-1_8)
    end function forward_square
    
    subroutine backward_square(this, elm)
        implicit none
        class(square_base) :: this
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
            input_var_ptr%g = input_var_ptr%g + 2d0 * input_var_ptr%v * output_var_ptr%g
        else
            input_var_ptr%g = 2d0 * input_var_ptr%v * output_var_ptr%g
        end if
    end subroutine backward_square


    
end module mod_square