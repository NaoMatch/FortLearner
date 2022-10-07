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

    interface asin
        module procedure :: asin_var
    end interface asin
    
contains

    function forward_arcsinusoid(this, input_var) result(output_var)
        implicit none
        class(arcsinusoid_base) :: this
        type(variable) :: input_var
        type(variable) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("arcsinusoid")

        ! Operation
        output_var%v = asin(input_var%v)
        output_var%var = asin(input_var%var)

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var=input_var, output_var=output_var, dim=-1_8)
    end function forward_arcsinusoid
    
    subroutine backward_arcsinusoid(this, elm)
        implicit none
        class(arcsinusoid_base) :: this
        type(element)      :: elm

        type(variable), pointer :: input_var_ptr
        type(variable), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        if (input_var_ptr%grd%dtype==-1) then
            input_var_ptr%grd = output_var_ptr%grd / sqrt(1d0 - input_var_ptr%var**2_8)
        else
            input_var_ptr%grd = input_var_ptr%grd + output_var_ptr%grd / sqrt(1d0 - input_var_ptr%var**2_8)
        end if        
    end subroutine backward_arcsinusoid

    function asin_var(input_var) result(output_var)
        implicit none
        type(variable) :: input_var
        type(variable) :: output_var
        output_var = arcsinusoid%forward(input_var)
    end function asin_var    
end module mod_arcsinusoid