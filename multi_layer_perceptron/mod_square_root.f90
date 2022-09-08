module mod_square_root
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: square_root_base
    contains
        procedure :: forward  => forward_square_root
        procedure :: backward => backward_square_root
    end type square_root_base
    type(square_root_base) :: square_root

    interface sqrt
        module procedure :: sqrt_var
    end interface sqrt    
contains

    function forward_square_root(this, input_var) result(output_var)
        implicit none
        class(square_root_base) :: this
        type(variable_) :: input_var
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("square_root")

        ! Operation
        call allocate_var(output_var, var_shape=shape(input_var%v))
        output_var%v = sqrt(input_var%v)

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_vars=input_var, output_var=output_var, dim=-1_8)
    end function forward_square_root
    
    subroutine backward_square_root(this, elm)
        implicit none
        class(square_root_base) :: this
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
            input_var_ptr%g = input_var_ptr%g + .5d0 / sqrt(input_var_ptr%v) * output_var_ptr%g
        else
            input_var_ptr%g =                 + .5d0 / sqrt(input_var_ptr%v) * output_var_ptr%g
        end if
    end subroutine backward_square_root

    function sqrt_var(input_var) result(output_var)
        implicit none
        type(variable_) :: input_var
        type(variable_) :: output_var
        output_var = square_root%forward(input_var)
    end function sqrt_var

end module mod_square_root