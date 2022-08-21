module mod_backwards
    use mod_wengert_list
    use mod_square
    use mod_sinusoid
    implicit none
    
contains

    subroutine set_initial_gradient(var)
        implicit none
        type(variable_) :: var

        integer(kind=8) :: stack_id, var_id

        stack_id = var%stack_id
        var_id = var%var_id + 1
        allocate(stacks(stack_id)%vars(var_id)%g(1,1))
        stacks(stack_id)%vars(var_id)%g = 1d0
    end subroutine set_initial_gradient

    subroutine backward(vars)
        implicit none
        type(variable_) :: vars(:)
        integer(kind=8) :: n_vars

        n_vars = size(vars)
        if (n_vars /= 1) then
            stop "Backward must be 1."
        end if
        call set_initial_gradient(vars(1))
        call backward_variable_(vars(1))
    end subroutine backward

    recursive subroutine backward_variable_(var)
        implicit none
        type(variable_) :: var
        type(element)   :: elm
        type(element), allocatable :: elms(:)
        type(variable_), allocatable :: vars(:)
        class(*), pointer :: opr
        integer(kind=8) :: n_elms

        allocate(elms(0))

        elm = get_list_element(var%var_id, var%stack_id)
        call backward_activation_functions(elm)

        call get_previous_layer_elements(elm, elms)

        do while (size(elms)>0)
            call backward_activation_functions(elms(1))
            n_elms = size(elms)
            elms = elms(1:n_elms-1)
        end do
    end subroutine backward_variable_

    subroutine get_previous_layer_elements(current_elm, previous_elms)
        implicit none
        type(element) :: current_elm
        type(element), allocatable :: previous_elms(:)

        integer(kind=8) :: i, id, stack_id, n_elm
        integer(kind=8), allocatable :: input_var_ids(:)

        stack_id = current_elm%stack_id
        input_var_ids = current_elm%var_ids_in(:)
        do i=1, size(input_var_ids), 1
            id = input_var_ids(i)

            previous_elms = [previous_elms, stacks(stack_id)%list(id)]
        end do
    end subroutine get_previous_layer_elements


    subroutine backward_activation_functions(elm)
        implicit none
        type(element)   :: elm
        type(variable_) :: output_var
        type(variable_) :: input_var1, input_var2

        call get_output_vars(elm, output_var)

        if (elm%opr_name == "square") then
            call square%backward(elm)
        elseif (elm%opr_name == "sinusoid") then
            call sinusoid%backward(elm)
        end if
    end subroutine backward_activation_functions

    subroutine get_input_var_ptr(elm, input_var_ptr)
        implicit none
        type(element) :: elm
        type(variable_), pointer :: input_var_ptr

        integer(kind=8) :: var_id

        var_id = elm%var_ids_in(1)+1
        input_var_ptr => stacks(elm%stack_id)%vars(var_id)
    end subroutine get_input_var_ptr

    function get_input_var(elm) result(input_var)
        implicit none
        type(element) :: elm
        type(variable_) :: input_var

        integer(kind=8) :: var_id

        var_id = elm%var_ids_in(1)+1
        input_var = stacks(elm%stack_id)%vars(var_id)
    end function get_input_var

    subroutine get_output_vars(elm, output_var)
        implicit none
        type(element) :: elm
        type(variable_) :: output_var
        
        integer(kind=8) :: var_id

        var_id = elm%var_id_out+1
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

    
end module mod_backwards