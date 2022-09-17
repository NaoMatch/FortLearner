module mod_addition
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: addition_base
    contains
        procedure :: forward  => forward_addition
        procedure :: backward => backward_addition
    end type addition_base
    type(addition_base) :: addition

    interface operator(+)
        module procedure addition_var_var
        module procedure addition_var_scl
        module procedure addition_scl_val
    end interface operator(+)
    
contains

    function forward_addition(this, input_var1, input_var2) result(output_var)
        implicit none
        class(addition_base) :: this
        type(variable_) :: input_var1, input_var2
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        real(kind=8) :: val
        ! Set up
        call this%set_activation_type_name("addition")

        ! Operation
        output_var%var = input_var1%var + input_var2%var

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var1=input_var1, input_var2=input_var2, output_var=output_var, dim=-1_8)
    end function forward_addition


    subroutine backward_addition(this, elm)
        implicit none
        class(addition_base) :: this
        type(element)      :: elm

        type(variable_), pointer :: input_var1_ptr, input_var2_ptr
        type(variable_), pointer :: output_var_ptr
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
                        input_var2_ptr%grd = sum(output_var_ptr%grd)
                    end if
                elseif (input_var2_ptr%var%dtype==1) then
                    if (output_var_ptr%grd%dtype==2) then
                        input_var2_ptr%grd = sum(output_var_ptr%grd, dim=1_8)
                    end if
                elseif (input_var2_ptr%var%dtype==2) then
                    if (output_var_ptr%grd%dtype==2) then
                        input_var2_ptr%grd = output_var_ptr%grd
                    end if
                end if
            else
                if (input_var2_ptr%var%dtype==0) then
                    if (output_var_ptr%grd%dtype==2) then
                        input_var2_ptr%grd = input_var2_ptr%grd + sum(output_var_ptr%grd)
                    end if
                elseif (input_var2_ptr%var%dtype==1) then
                    if (output_var_ptr%grd%dtype==2) then
                        input_var2_ptr%grd = input_var2_ptr%grd + sum(output_var_ptr%grd, dim=1_8)
                    end if
                elseif (input_var2_ptr%var%dtype==2) then
                    if (output_var_ptr%grd%dtype==2) then
                        input_var2_ptr%grd = input_var2_ptr%grd + output_var_ptr%grd
                    end if
                end if
            end if
        end if

        call debug_print(__FILE__, __LINE__, elm, &
            input_var1_ptr, input_var2_ptr, output_var_ptr, f_)
    end subroutine backward_addition    


    function addition_var_var(input_var1, input_var2) result(output_var)
        implicit none
        type(variable_), intent(in) :: input_var1, input_var2
        type(variable_) :: output_var
        output_var = addition%forward(input_var1, input_var2)
    end function addition_var_var


    function addition_var_scl(input_var, input_scl) result(output_var)
        implicit none
        type(variable_), intent(in) :: input_var
        real(kind=8), intent(in)    :: input_scl
        type(variable_) :: input_scl_var
        type(variable_) :: output_var
        input_scl_var = variable_(input_scl, stack_id=input_var%stack_id, require_grad=.false.)
        output_var = addition%forward(input_var, input_scl_var)
    end function addition_var_scl

    
    function addition_scl_val(input_scl, input_var) result(output_var)
        implicit none
        real(kind=8), intent(in)    :: input_scl
        type(variable_), intent(in) :: input_var
        type(variable_) :: input_scl_var
        type(variable_) :: output_var
        input_scl_var = variable_(input_scl, stack_id=input_var%stack_id, require_grad=.false.)
        output_var = addition%forward(input_var, input_scl_var)
    end function addition_scl_val


end module mod_addition