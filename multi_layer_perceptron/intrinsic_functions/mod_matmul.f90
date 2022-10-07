module mod_matmul
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: matmul_base
    contains
        procedure :: forward  => forward_matmul
        procedure :: backward => backward_matmul
    end type matmul_base
    type(matmul_base) :: matmul_function

    interface matmul
        module procedure matmul_var_var
    end interface matmul
    
contains

    function forward_matmul(this, input_var1, input_var2) result(output_var)
        implicit none
        class(matmul_base) :: this
        type(variable) :: input_var1, input_var2
        type(variable) :: output_var
        integer(kind=8) :: stack_id
        real(kind=8) :: val
        ! Set up
        call this%set_activation_type_name("matmul")

        ! Operation
        output_var%var = matmul(input_var1%var, input_var2%var)
        
        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var1=input_var1, input_var2=input_var2, output_var=output_var, dim=-1_8)
    end function forward_matmul


    subroutine backward_matmul(this, elm)
        implicit none
        class(matmul_base) :: this
        type(element)      :: elm

        type(variable), pointer :: input_var1_ptr, input_var2_ptr
        type(variable), pointer :: output_var_ptr

        type(variable) :: var1_T
        call get_input_variable_pointer(elm, input_var1_ptr, input_var2_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        call debug_print(__FILE__, __LINE__, elm, &
            input_var1_ptr, input_var2_ptr, output_var_ptr, t_)
            
        if (input_var1_ptr%require_grad) then
            if (input_var1_ptr%grd%dtype==-1) then
                input_var1_ptr%grd = matmul(output_var_ptr%grd, input_var2_ptr%var, f_, t_)
            else
                input_var1_ptr%grd = input_var1_ptr%grd + matmul(output_var_ptr%grd, input_var2_ptr%var, f_, t_)
            end if
        end if

        if (input_var2_ptr%require_grad) then
            if (input_var2_ptr%grd%dtype==-1) then
                input_var2_ptr%grd = matmul(input_var1_ptr%var, output_var_ptr%grd, t_, f_)
            else
                input_var2_ptr%grd = input_var2_ptr%grd + matmul(input_var1_ptr%var, output_var_ptr%grd, t_, f_)
            end if
        end if
        call debug_print(__FILE__, __LINE__, elm, &
            input_var1_ptr, input_var2_ptr, output_var_ptr, f_)
    end subroutine backward_matmul    


    function matmul_var_var(input_var1, input_var2) result(output_var)
        implicit none
        type(variable), intent(in) :: input_var1, input_var2
        type(variable) :: output_var
        output_var = matmul_function%forward(input_var1, input_var2)
    end function matmul_var_var

end module mod_matmul