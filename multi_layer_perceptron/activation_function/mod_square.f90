module mod_square
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: square_base
    contains
        procedure :: forward  => forward_square
        procedure :: backward => backward_square
    end type square_base
    type(square_base) :: square_func

    interface square
        module procedure :: square_var
    end interface square    
    
contains

    function forward_square(this, input_var) result(output_var)
        implicit none
        class(square_base) :: this
        type(variable) :: input_var
        type(variable) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("square")

        ! Operation
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

        type(variable), pointer :: input_var_ptr
        type(variable), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        if (input_var_ptr%grd%dtype==-1) then
            input_var_ptr%grd = output_var_ptr%grd * 2.0d0 * input_var_ptr%var
        else
            input_var_ptr%grd = input_var_ptr%grd + output_var_ptr%grd * 2.0d0 * input_var_ptr%var
        end if
    end subroutine backward_square

    function square_var(input_var) result(output_var)
        implicit none
        type(variable) :: input_var
        type(variable) :: output_var
        output_var = square_func%forward(input_var)
    end function square_var


    
end module mod_square