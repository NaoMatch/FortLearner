module mod_backwards
    use mod_sort
    use mod_wengert_list
    include "./inc_use_activation_functions.f90"
    implicit none
    
contains

    subroutine set_initial_gradient(var)
        implicit none
        type(variable) :: var

        integer(kind=8) :: stack_id, var_id

        stack_id = var%stack_id
        ! var_id = var%var_id
        var_id = get_list_index(var%var_id, stacks(stack_id)%idxs)
        
        if (.not. allocated(stacks(stack_id)%vars(var_id)%g)) allocate(stacks(stack_id)%vars(var_id)%g(1,1))
        stacks(stack_id)%vars(var_id)%g = 1d0
        stacks(stack_id)%vars(var_id)%grd = 1d0
    end subroutine set_initial_gradient

    subroutine backward(var)
        implicit none
        type(variable) :: var
        integer(kind=8) :: n_vars

        call set_initial_gradient(var)
        call backward_variable_(var)
    end subroutine backward

    recursive subroutine backward_variable_(var)
        implicit none
        type(variable) :: var
        type(element)   :: elm
        type(element), allocatable :: elms(:)
        type(variable), allocatable :: vars(:)
        class(*), pointer :: opr
        integer(kind=8) :: n_elms, e
        integer(kind=8), allocatable :: generations(:), indices(:)

        allocate(elms(0))

        ! print*, '*********************************************************************************************'
        ! print*, '*********************************************************************************************'
        elm = get_list_element(var%var_id, var%stack_id)
        call backward_activation_functions(elm)

        call get_previous_layer_elements(elm, elms)
        do while (size(elms)>0)
            ! print*, '*********************************************************************************************'
            ! print*, '*********************************************************************************************'
            allocate(generations(0))
            allocate(indices(0))

            ! Backward Last Element
            n_elms = size(elms)
            do e=1, n_elms, 1
                generations = [generations, elms(e)%generation]
                indices = [indices, e]
            end do
            call quick_argsort(generations, indices, n_elms)
            elms = elms(indices)
            ! print*, "    operation name ::: ", trim(elms(n_elms)%opr_name)
            call backward_activation_functions(elms(n_elms))

            ! Get & Remove Last Element
            elm = elms(n_elms)
            elms = elms(1:n_elms-1)

            ! Get Previous Layer
            call get_previous_layer_elements(elm, elms)

            deallocate(generations)
            deallocate(indices)    
        end do
    end subroutine backward_variable_

    subroutine get_previous_layer_elements(current_elm, previous_elms)
        implicit none
        type(element) :: current_elm
        type(element), allocatable :: previous_elms(:)

        integer(kind=8) :: i, j, id, stack_id, n_elm, p
        integer(kind=8), allocatable :: input_var_ids(:)
        logical(kind=4) :: to_be_append

        stack_id = current_elm%stack_id
        input_var_ids = current_elm%var_ids_in(:)
        do i=1, size(input_var_ids), 1
            id = input_var_ids(i)
            do j=1, size(stacks(stack_id)%list), 1
                if (stacks(stack_id)%list(j)%var_id_out == id) then
                    to_be_append = .true.
                    do p=1, size(previous_elms), 1
                        if ( stacks(stack_id)%list(j)%elm_id == previous_elms(p)%elm_id ) then
                            to_be_append = .false.
                            exit
                        end if
                    end do
                    if (to_be_append) previous_elms = [previous_elms, stacks(stack_id)%list(j)]
                end if
            end do
        end do
    end subroutine get_previous_layer_elements


    subroutine backward_activation_functions(elm)
        implicit none
        type(element)   :: elm

        if     (elm%opr_name == "square") then
            call square_func%backward(elm)
        elseif (elm%opr_name == "square_root") then
            call square_root%backward(elm)
        elseif (elm%opr_name == "sinusoid") then
            call sinusoid%backward(elm)
        elseif (elm%opr_name == "cosine") then
            call cosine%backward(elm)
        elseif (elm%opr_name == "tangent") then
            call tangent%backward(elm)
        elseif (elm%opr_name == "arcsinusoid") then
            call arcsinusoid%backward(elm)
        elseif (elm%opr_name == "arccosine") then
            call arccosine%backward(elm)
        elseif (elm%opr_name == "arctangent") then
            call arctangent%backward(elm)
        elseif (elm%opr_name == "addition") then
            call addition%backward(elm)
        elseif (elm%opr_name == "substraction") then
            call substraction%backward(elm)
        elseif (elm%opr_name == "multiplication") then
            call multiplication%backward(elm)
        elseif (elm%opr_name == "division") then
            call division%backward(elm)
        elseif (elm%opr_name == "exponential") then
            call exponential%backward(elm)
        elseif (elm%opr_name == "log_natural") then
            call log_natural%backward(elm)
        elseif (elm%opr_name == "absolute_value") then
            call absolute_value%backward(elm)
        elseif (elm%opr_name == "power") then
            call power%backward(elm)
        elseif (elm%opr_name == "summation") then
            call summation%backward(elm)
        elseif (elm%opr_name == "sigmoidal") then
            call sigmoidal%backward(elm)
        elseif (elm%opr_name == "matmul") then
            call matmul_function%backward(elm)
        elseif (elm%opr_name == "relu") then
            call relu_function%backward(elm)
        elseif (elm%opr_name == "spread") then
            call spread_func%backward(elm)
        else
            print*, trim(elm%opr_name)
            stop "NotImplementedError!"
        end if
    end subroutine backward_activation_functions

    subroutine get_input_var_ptr(elm, input_var_ptr)
        implicit none
        type(element) :: elm
        type(variable), pointer :: input_var_ptr

        integer(kind=8) :: var_id

        var_id = elm%var_ids_in(1)
        input_var_ptr => stacks(elm%stack_id)%vars(var_id)
    end subroutine get_input_var_ptr

    function get_input_var(elm) result(input_var)
        implicit none
        type(element) :: elm
        type(variable) :: input_var

        integer(kind=8) :: var_id

        var_id = elm%var_ids_in(1)
        input_var = stacks(elm%stack_id)%vars(var_id)
    end function get_input_var

    subroutine get_output_vars(elm, output_var)
        implicit none
        type(element) :: elm
        type(variable) :: output_var
        
        integer(kind=8) :: var_id

        var_id = elm%var_id_out
        output_var = stacks(elm%stack_id)%vars(var_id)
    end subroutine get_output_vars

    function get_list_element(var_id, stack_id) result(elm)
        implicit none
        type(element)   :: elm
        integer(kind=8) :: var_id, stack_id
        integer(kind=8) :: id, e

        id = stack_id

        do e=1, size(stacks(id)%list(:)), 1
            if (stacks(id)%list(e)%var_id_out == var_id) then
                elm = stacks(id)%list(e)
                return
            end if
        end do
        elm%var_id_out = -1
    end function get_list_element

    subroutine clear_grad(var)
        implicit none
        type(variable) :: var

        integer(kind=8) :: stack_id, v
        
        stack_id = var%stack_id
        stack_id = 1
        
        do v=1, size(stacks(stack_id)%vars), 1
            if (.not. allocated(stacks(stack_id)%vars(v)%g)) cycle
            stacks(stack_id)%vars(v)%g = 0d0
            call stacks(stack_id)%vars(v)%var%clear_all()
            call stacks(stack_id)%vars(v)%grd%clear_all()
            print*, stacks(stack_id)%vars(v)%grd%s
        end do
    end subroutine clear_grad

    
end module mod_backwards