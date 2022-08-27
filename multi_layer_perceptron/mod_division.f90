module mod_division
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: division_base
    contains
        procedure :: forward  => forward_division
        procedure :: backward => backward_division
    end type division_base
    type(division_base) :: division

    interface operator(/)
        module procedure division_var_var
        module procedure division_var_scl
        module procedure division_scl_var
    end interface operator(/)

contains
    function forward_division(this, input_var1, input_var2) result(output_var)
        implicit none
        class(division_base) :: this
        type(variable_) :: input_var1, input_var2, input_vars(2)
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("division")

        ! Operation
        output_var%v = input_var1%v / input_var2%v

        ! Append 'variables' to Stack
        input_vars = [input_var1, input_var2]
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var1=input_var1, input_var2=input_var2, output_var=output_var)
    end function forward_division


    subroutine backward_division(this, elm)
        implicit none
        class(division_base) :: this
        type(element)      :: elm

        type(variable_), pointer :: input_var1_ptr, input_var2_ptr
        type(variable_), pointer :: output_var_ptr
        call get_input_variable_pointer(elm, input_var1_ptr, input_var2_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        ! print*, '*********************************************************************************************'
        ! print*, " ---- Division Backward"
        ! print*, "      input_var1_ptr%g: ", allocated(input_var1_ptr%g)
        ! print*, "      input_var2_ptr%g: ", allocated(input_var2_ptr%g)

        if (allocated(input_var1_ptr%g)) then
            input_var1_ptr%g = input_var1_ptr%g + 1d0/input_var2_ptr%v      * output_var_ptr%g
        else
            input_var1_ptr%g =                    1d0/input_var2_ptr%v      * output_var_ptr%g
        end if

        if (allocated(input_var2_ptr%g)) then
            input_var2_ptr%g = input_var2_ptr%g - input_var1_ptr%v/input_var2_ptr%v**2d0 * output_var_ptr%g
        else
            input_var2_ptr%g =                  - input_var1_ptr%v/input_var2_ptr%v**2d0 * output_var_ptr%g
        end if
        ! print*, input_var1_ptr%g        
        ! print*, input_var2_ptr%g        
    end subroutine backward_division    


    function division_var_var(input_var1, input_var2) result(output_var)
        implicit none
        type(variable_), intent(in) :: input_var1, input_var2
        type(variable_) :: output_var
        output_var = division%forward(input_var1, input_var2)
    end function division_var_var    


    function division_var_scl(input_var, input_scl) result(output_var)
        implicit none
        type(variable_), intent(in) :: input_var
        real(kind=8), intent(in) :: input_scl
        type(variable_) :: output_var
        type(variable_) :: input_var_new
        input_var_new = variable_(input_scl, stack_id=input_var%stack_id)
        output_var = division%forward(input_var, input_var_new)
    end function division_var_scl    


    function division_scl_var(input_scl, input_var) result(output_var)
        implicit none
        real(kind=8), intent(in) :: input_scl
        type(variable_), intent(in) :: input_var
        type(variable_) :: output_var
        type(variable_) :: input_var_new
        input_var_new = variable_(input_scl, stack_id=input_var%stack_id)
        output_var = division%forward(input_var_new, input_var)
    end function division_scl_var    

end module mod_division