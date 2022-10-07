module mod_spread
    use mod_wengert_list
    use mod_activation_function
    implicit none

    type, extends(activation_function_) :: spread_base
    contains
        procedure :: forward => forward_spread
        procedure :: backward => backward_spread
    end type spread_base
    type(spread_base) :: spread_func
    
    interface spread
        module procedure :: spread_var
    end interface spread
contains
    
    function forward_spread(this, input_var, dim, ncopies, output_shape) result(output_var)
        implicit none
        class(spread_base) :: this
        type(variable) :: input_var
        type(variable) :: output_var
        integer(kind=8), optional :: dim
        integer(kind=8), optional :: ncopies
        integer(kind=8), optional :: output_shape(2)
        integer(kind=8) :: stack_id, dim_set
        ! Set up
        call this%set_activation_type_name("spread")

        ! Operation
        if     (present(output_shape)) then
            dim_set = -1
            output_var%var = spread(input_var%var, output_shape)
        elseif (present(dim) .and. present(ncopies)) then
            dim_set = dim
            output_var%var = spread(input_var%var, dim, ncopies)
        end if

        ! Append 'variables' to Stack
        call set_operation(&
            this, &
            operation_name=this%act_name,   &
            input_var=input_var, output_var=output_var, dim=dim_set)
    end function forward_spread

    subroutine backward_spread(this, elm)
        implicit none
        class(spread_base) :: this
        type(element)      :: elm

        type(variable), pointer  :: input_var_ptr
        type(variable), pointer  :: output_var_ptr

        call get_input_variable_pointer(elm, input_var_ptr)
        call get_output_variable_pointer(elm, output_var_ptr)

        call debug_print(__FILE__, __LINE__, &
                elm, input_var_ptr, output_var_ptr, before=t_)
        if (input_var_ptr%grd%dtype==-1) then
            if     (input_var_ptr%var%dtype==0_8 .and. output_var_ptr%var%dtype==1_8) then
                input_var_ptr%grd = sum(output_var_ptr%grd)
            elseif     (input_var_ptr%var%dtype==0_8 .and. output_var_ptr%var%dtype==2_8) then
                input_var_ptr%grd = sum(output_var_ptr%grd)
            elseif (input_var_ptr%var%dtype==1_8 .and. output_var_ptr%var%dtype==2_8) then
                input_var_ptr%grd = sum(output_var_ptr%grd, dim=elm%dim)
            end if
        else
            if     (input_var_ptr%var%dtype==0_8 .and. output_var_ptr%var%dtype==1_8) then
                input_var_ptr%grd = input_var_ptr%grd + sum(output_var_ptr%grd)
            elseif     (input_var_ptr%var%dtype==0_8 .and. output_var_ptr%var%dtype==2_8) then
                input_var_ptr%grd = input_var_ptr%grd + sum(output_var_ptr%grd)
            elseif (input_var_ptr%var%dtype==1_8 .and. output_var_ptr%var%dtype==2_8) then
                input_var_ptr%grd = input_var_ptr%grd + sum(output_var_ptr%grd, dim=elm%dim)
            end if
        end if
        call debug_print(__FILE__, __LINE__, &
                elm, input_var_ptr, output_var_ptr, before=f_)
    end subroutine backward_spread

    function spread_var(input_var, dim, ncopies, output_shape) result(output_var)
        implicit none
        type(variable), intent(in) :: input_var
        integer(kind=8), intent(in), optional :: dim, ncopies, output_shape(2)
        type(variable) :: output_var
        if (present(output_shape) .and. (present(dim) .or. present(ncopies))) then
            print*, "'output_shape' and 'dim/ncopies' cannnot be set simultaneously."
            stop
        elseif     (present(output_shape)) then
            output_var = spread_func%forward(input_var, output_shape=output_shape)
        elseif (present(dim) .and. present(ncopies)) then
            output_var = spread_func%forward(input_var, dim=dim, ncopies=ncopies)
        end if
    end function spread_var

end module mod_spread