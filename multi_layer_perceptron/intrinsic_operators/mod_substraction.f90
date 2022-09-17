module mod_substraction
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: substraction_base
    contains
        procedure :: forward  => forward_substraction
        procedure :: backward => backward_substraction
    end type substraction_base
    type(substraction_base) :: substraction

    interface operator(-)
        module procedure substraction_var_var
        module procedure substraction_var_scl
        module procedure substraction_scl_var
    end interface operator(-)

contains
    function forward_substraction(this, input_var1, input_var2) result(output_var)
        implicit none
        class(substraction_base) :: this
        type(variable) :: input_var1, input_var2, input_vars(2)
        type(variable) :: output_var
        real(kind=8) :: val
        ! Set up
        call this%set_activation_type_name("substraction")

        ! Operation
        output_var%var = input_var1%var - input_var2%var
                
        ! Append 'variables' to Stack
        input_vars = [input_var1, input_var2]
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var1=input_var1, input_var2=input_var2, output_var=output_var, dim=-1_8)
    end function forward_substraction

    subroutine backward_substraction(this, elm)
        implicit none
        class(substraction_base) :: this
        type(element)      :: elm

        type(variable), pointer :: input_var1_ptr, input_var2_ptr
        type(variable), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var1_ptr, input_var2_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        call debug_print(__FILE__, __LINE__, elm, &
            input_var1_ptr, input_var2_ptr, output_var_ptr, t_)


        if (input_var1_ptr%require_grad) then
            if (input_var1_ptr%grd%dtype==-1) then
                if (input_var1_ptr%var%dtype==0) then
                    if (output_var_ptr%grd%dtype==2) then
                        input_var1_ptr%grd = sum(output_var_ptr%grd)
                    end if
                elseif (input_var1_ptr%var%dtype==1) then
                    if (output_var_ptr%grd%dtype==2) then
                        input_var1_ptr%grd = sum(output_var_ptr%grd, dim=1_8)
                    end if
                elseif (input_var1_ptr%var%dtype==2) then
                    if (output_var_ptr%grd%dtype==2) then
                        input_var1_ptr%grd = output_var_ptr%grd
                    end if
                end if
            else
                if (input_var1_ptr%var%dtype==0) then
                    if (output_var_ptr%grd%dtype==2) then
                        input_var1_ptr%grd = input_var1_ptr%grd + sum(output_var_ptr%grd)
                    end if
                elseif (input_var1_ptr%var%dtype==1) then
                    if (output_var_ptr%grd%dtype==2) then
                        input_var1_ptr%grd = input_var1_ptr%grd + sum(output_var_ptr%grd, dim=1_8)
                    end if
                elseif (input_var1_ptr%var%dtype==2) then
                    if (output_var_ptr%grd%dtype==2) then
                        input_var1_ptr%grd = input_var1_ptr%grd + output_var_ptr%grd
                    end if
                end if
            end if
        end if

        if (input_var2_ptr%require_grad) then
            if (input_var2_ptr%grd%dtype==-1) then
                if (input_var2_ptr%var%dtype==0) then
                    if (output_var_ptr%grd%dtype==2) then
                        input_var2_ptr%grd = 0d0 - sum(output_var_ptr%grd)
                    end if
                elseif (input_var2_ptr%var%dtype==1) then
                    if (output_var_ptr%grd%dtype==2) then
                        input_var2_ptr%grd = 0d0 - sum(output_var_ptr%grd, dim=1_8)
                    end if
                elseif (input_var2_ptr%var%dtype==2) then
                    if (output_var_ptr%grd%dtype==2) then
                        input_var2_ptr%grd = 0d0 - output_var_ptr%grd
                    end if
                end if
            else
                if (input_var2_ptr%var%dtype==0) then
                    if (output_var_ptr%grd%dtype==2) then
                        input_var2_ptr%grd = input_var2_ptr%grd - sum(output_var_ptr%grd)
                    end if
                elseif (input_var2_ptr%var%dtype==1) then
                    if (output_var_ptr%grd%dtype==2) then
                        input_var2_ptr%grd = input_var2_ptr%grd - sum(output_var_ptr%grd, dim=1_8)
                    end if
                elseif (input_var2_ptr%var%dtype==2) then
                    if (output_var_ptr%grd%dtype==2) then
                        input_var2_ptr%grd = input_var2_ptr%grd - output_var_ptr%grd
                    end if
                end if
            end if
        end if
            
        call debug_print(__FILE__, __LINE__, elm, &
            input_var1_ptr, input_var2_ptr, output_var_ptr, f_)
    end subroutine backward_substraction    




    function substraction_var_var(input_var1, input_var2) result(output_var)
        implicit none
        type(variable), intent(in) :: input_var1, input_var2
        type(variable) :: output_var
        output_var = substraction%forward(input_var1, input_var2)
    end function substraction_var_var    


    function substraction_var_scl(input_var, input_scl) result(output_var)
        implicit none
        type(variable), intent(in) :: input_var
        real(kind=8), intent(in) :: input_scl
        type(variable) :: output_var
        type(variable) :: input_var_new
        input_var_new = variable(input_scl, stack_id=input_var%stack_id)
        output_var = substraction%forward(input_var, input_var_new)
    end function substraction_var_scl    


    function substraction_scl_var(input_scl, input_var) result(output_var)
        implicit none
        real(kind=8), intent(in) :: input_scl
        type(variable), intent(in) :: input_var
        type(variable) :: output_var
        type(variable) :: input_var_new
        input_var_new = variable(input_scl, stack_id=input_var%stack_id)
        output_var = substraction%forward(input_var_new, input_var)
    end function substraction_scl_var    
end module mod_substraction