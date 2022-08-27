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
        type(variable_) :: input_var1, input_var2, input_vars(2)
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("addition")

        ! Operation
        output_var%v = input_var1%v + input_var2%v

        ! Append 'variables' to Stack
        input_vars = [input_var1, input_var2]
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var1=input_var1, input_var2=input_var2, output_var=output_var)
    end function forward_addition


    subroutine backward_addition(this, elm)
        implicit none
        class(addition_base) :: this
        type(element)      :: elm

        type(variable_), pointer :: input_var1_ptr, input_var2_ptr
        type(variable_), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var1_ptr, input_var2_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        ! print*, '*********************************************************************************************'
        ! print*, " ---- Addition Backward"
        ! print*, "      input_var1_ptr%g: ", allocated(input_var1_ptr%g)
        ! print*, "      input_var2_ptr%g: ", allocated(input_var2_ptr%g)

        if (allocated(input_var1_ptr%g)) then
            input_var1_ptr%g = input_var1_ptr%g + output_var_ptr%g
        else
            input_var1_ptr%g =                    output_var_ptr%g
        end if

        if (.not. associated(input_var2_ptr)) return
        if (allocated(input_var2_ptr%g)) then
            input_var2_ptr%g = input_var2_ptr%g + output_var_ptr%g
        else
            input_var2_ptr%g =                    output_var_ptr%g
        end if
        ! print*, input_var1_ptr%g        
        ! print*, input_var2_ptr%g        
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
        input_scl_var = variable_(input_scl, stack_id=input_var%stack_id)
        output_var = addition%forward(input_var, input_scl_var)
    end function addition_var_scl

    
    function addition_scl_val(input_scl, input_var) result(output_var)
        implicit none
        real(kind=8), intent(in)    :: input_scl
        type(variable_), intent(in) :: input_var
        type(variable_) :: input_scl_var
        type(variable_) :: output_var
        input_scl_var = variable_(input_scl, stack_id=input_var%stack_id)
        output_var = addition%forward(input_var, input_scl_var)
    end function addition_scl_val


end module mod_addition