module mod_summation
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: summation_base
    contains
        procedure :: forward  => forward_summation
        procedure :: backward => backward_summation
    end type summation_base
    type(summation_base) :: summation
    
    interface sum
        module procedure :: sum_var
    end interface sum

contains

    function forward_summation(this, input_var, dim) result(output_var)
        implicit none
        class(summation_base) :: this
        type(variable_) :: input_var
        type(variable_) :: output_var
        integer(kind=8), optional :: dim
        integer(kind=8) :: stack_id, dim_set
        integer(kind=4) :: output_shape(2)
        ! Set up
        call this%set_activation_type_name("summation")

        ! Operation
        if (present(dim)) then
            dim_set = dim
            output_var%var = sum(input_var%var, dim=dim)
        else
            dim_set = 0
            output_var%var = sum(input_var%var)
        end if

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var=input_var, output_var=output_var, dim=dim_set)
    end function forward_summation

    subroutine backward_summation(this, elm)
        implicit none
        class(summation_base) :: this
        type(element)      :: elm

        type(variable_), pointer  :: input_var_ptr
        type(variable_), pointer  :: output_var_ptr

        call get_input_variable_pointer(elm, input_var_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        call debug_print(__FILE__, __LINE__, &
                elm, input_var_ptr, output_var_ptr, before=t_)
        if (input_var_ptr%grd%dtype==-1) then
            ! Grad is not allocated.
            if (input_var_ptr%var%dtype==2) then
                if (elm%dim==0) then
                    ! print*, "                    : Scalar -> Matrix"
                    input_var_ptr%grd = spread(output_var_ptr%grd, output_shape=shape(input_var_ptr%var))
                elseif (elm%dim==1) then
                    ! print*, "                    : Vector -> Maxtrix dim=1"
                    input_var_ptr%grd = spread(output_var_ptr%grd, dim=elm%dim, ncopies=input_var_ptr%var%batch_sizes())
                elseif (elm%dim==2) then
                    ! print*, "                    : Vector -> Maxtrix dim=2"
                    input_var_ptr%grd = spread(output_var_ptr%grd, dim=elm%dim, ncopies=input_var_ptr%var%batch_sizes())
                end if
            elseif (input_var_ptr%var%dtype==1) then
                if (elm%dim==0) then
                    ! print*, "                    : Scalar -> Vector"
                    input_var_ptr%grd = spread(output_var_ptr%grd, dim=1_8, ncopies=input_var_ptr%var%batch_sizes())
                elseif (elm%dim==1) then
                    ! print*, "                    : Scalar -> Vector dim=1"
                    input_var_ptr%grd = spread(output_var_ptr%grd, dim=elm%dim, ncopies=input_var_ptr%var%batch_sizes())
                end if
            end if
        end if
        call debug_print(__FILE__, __LINE__, &
                elm, input_var_ptr, output_var_ptr, before=f_)
    end subroutine backward_summation

    function sum_var(input_var, dim) result(output_var)
        implicit none
        type(variable_), intent(in) :: input_var
        integer(kind=8), intent(in), optional :: dim
        type(variable_) :: output_var
        output_var = summation%forward(input_var, dim)
    end function sum_var


end module mod_summation
