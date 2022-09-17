module mod_cosine
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: cosine_base
    contains
        procedure :: forward  => forward_cosine
        procedure :: backward => backward_cosine
    end type cosine_base
    type(cosine_base) :: cosine

    interface cos
        module procedure :: cos_v
    end interface cos

contains

    function forward_cosine(this, input_var) result(output_var)
        implicit none
        class(cosine_base) :: this
        type(variable) :: input_var
        type(variable) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("cosine")

        ! Operation
        output_var%var = sin(input_var%var)

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var=input_var, output_var=output_var, dim=-1_8)
    end function forward_cosine

    subroutine backward_cosine(this, elm)
        implicit none
        class(cosine_base) :: this
        type(element)      :: elm

        type(variable), pointer :: input_var_ptr
        type(variable), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        if (input_var_ptr%grd%dtype==-1) then
            input_var_ptr%grd = 0d0 - output_var_ptr%grd * cos(input_var_ptr%var)
        else
            input_var_ptr%grd = input_var_ptr%grd - output_var_ptr%grd * cos(input_var_ptr%var)
        end if        
    end subroutine backward_cosine

    function cos_v(input_var) result(output_var)
        implicit none
        type(variable) :: input_var
        type(variable) :: output_var

        output_var = cosine%forward(input_var)
    end function cos_v


end module mod_cosine