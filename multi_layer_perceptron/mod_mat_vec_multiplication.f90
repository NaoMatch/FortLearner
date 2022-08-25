module mod_mat_vec_multiplication
    use mod_wengert_list
    use mod_activation_function
    implicit none
    

    type, extends(activation_function_) :: mvmul_base
    contains
        procedure :: forward  => forward_mvmul
        procedure :: backward => backward_mvmul
    end type mvmul_base
    type(mvmul_base) :: mvmul

contains


    function forward_mvmul(this, input_var1, input_var2) result(output_var)
        implicit none
        class(exponential_base) :: this
        type(variable_) :: input_var1, input_var2
        type(variable_) :: output_var
        integer(kind=8) :: stack_id
        ! Set up
        call this%set_activation_type_name("mvmul")

        ! Operation
        output_var%v = exp(input_var%v)

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_vars=input_var, output_var=output_var)
    end function forward_mvmul















    
end module mod_mat_vec_multiplication