module mod_arccosine
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: arccosine_base
    contains
        procedure :: forward  => forward_arccosine
        procedure :: backward => backward_arccosine
    end type arccosine_base
    type(arccosine_base) :: arccosine

    interface acos
        module procedure :: acos_var
    end interface acos
    
contains

    function forward_arccosine(this, input_var) result(output_var)
        implicit none
        class(arccosine_base) :: this
        type(variable_) :: input_var
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("arccosine")

        ! Operation
        output_var%v = acos(input_var%v)
        output_var%var = acos(input_var%var)

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var=input_var, output_var=output_var, dim=-1_8)
    end function forward_arccosine
    
    subroutine backward_arccosine(this, elm)
        implicit none
        class(arccosine_base) :: this
        type(element)      :: elm

        type(variable_), pointer :: input_var_ptr
        type(variable_), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        if (input_var_ptr%grd%dtype==-1) then
            input_var_ptr%grd = 0d0 - output_var_ptr%grd / sqrt(1d0 - input_var_ptr%var**2_8)
        else
            input_var_ptr%grd = input_var_ptr%grd - output_var_ptr%grd / sqrt(1d0 - input_var_ptr%var**2_8)
        end if
    end subroutine backward_arccosine

    function acos_var(input_var) result(output_var)
        implicit none
        type(variable_) :: input_var
        type(variable_) :: output_var
        output_var = arccosine%forward(input_var)
    end function acos_var

end module mod_arccosine