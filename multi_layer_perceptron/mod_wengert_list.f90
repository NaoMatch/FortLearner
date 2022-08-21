module mod_wengert_list
    use mod_var
    use mod_const
    implicit none

    type variable_
        real(kind=8), allocatable  :: v(:,:)
        real(kind=8), allocatable  :: g(:,:)
        logical(kind=4)            :: is_input=.false.
        character(len=256)         :: var_name="None"
        integer(kind=8)            :: var_id
        integer(kind=8)            :: stack_id
    contains
    end type variable_

    interface variable_
        module procedure :: new_variable_s_
        module procedure :: new_variable_m_
    end interface variable_
    
    !> Element Type of Wengert List
    type element
        integer(kind=8)                 :: elm_id
        integer(kind=8), allocatable    :: prev_elm_ids(:)
        integer(kind=8)                 :: generation
        character(len=256)              :: var_name_out    ! output variable name
        integer(kind=8)                 :: var_id_out=-1   ! output variable name
        character(len=256)              :: opr_name        ! operation name
        character(len=256), allocatable :: var_names_in(:) ! input variable name(s)
        integer(kind=8), allocatable    :: var_ids_in(:)   ! input variable name(s)
        integer(kind=8)                 :: stack_id        ! output variable name
        logical(kind=4)                 :: is_output = .false.
        class(*), pointer               :: opr => null()
    contains
        procedure :: print => print_element
    end type element

    type optimizer_
        real(kind=8) :: learning_rate
        real(kind=8) :: alpha
    contains
    end type optimizer_

    interface optimizer_
        module procedure :: new_optimizer_
    end interface optimizer_

    type neural_network
        type(optimizer_) :: opt
        integer(kind=8) :: stack_id=-1
    contains
        procedure :: select_stack
        procedure :: set_stack_id_single_input
        procedure :: set_stack_id_multi_inputs
        procedure, pass :: init_single_input
        procedure, pass :: init_multi_inputs
        generic :: init => init_single_input, init_multi_inputs
        procedure :: build => build_neural_network
        procedure :: print => print_architecture
        procedure :: compile => compile_neural_network
    end type neural_network
        
    type stack
        integer(kind=8) :: n_ids=1
        type(element), allocatable :: list(:)
        type(variable_), allocatable :: vars(:)
        integer(kind=8), allocatable :: idxs(:)
    contains
        procedure :: print => print_elements
        procedure, pass :: assign_var_names
        procedure, pass :: assign_var_name
        generic   :: assign_name => assign_var_names, assign_var_name
    end type stack

    interface set_operation
        module procedure set_operation_1in_1out
        module procedure set_operation_2in_1out
    end interface set_operation

    interface get_input_variable_pointer
        module procedure get_input_variable_pointer_single
        module procedure get_input_variable_pointer_multi
    end interface get_input_variable_pointer

    type(stack), target :: stacks(MAX_STACK_SIZE)
    logical(kind=4) :: is_used_stacks(MAX_STACK_SIZE) = .false.

contains

    function new_optimizer_(learning_rate, alpha)
        implicit none
        type(optimizer_) :: new_optimizer_
        real(kind=8) :: learning_rate
        real(kind=8) :: alpha
        new_optimizer_%learning_rate = learning_rate
        new_optimizer_%alpha = alpha
    end function new_optimizer_

    subroutine print_element(this)
        implicit none
        class(element) :: this
        integer(kind=8)       :: i
        print*, '*********************************************************************************************'
        print*, "var_name_out : ", trim(this%var_name_out)    ! output variable name
        print*, "var_id_out   : ", int(this%var_id_out)      ! output variable name
        print*, "opr_name     : ", trim(this%opr_name)       ! operation name
        if (allocated(this%var_names_in)) then
            do i=1, size(this%var_names_in), 1
                print*, "var_names_in : ", trim(this%var_names_in(i)) ! input variable name(s)
            end do
        end if
        if (allocated(this%var_ids_in)) then
            do i=1, size(this%var_ids_in), 1
                print*, "var_ids_in   : ", int(this%var_ids_in(i)) ! input variable name(s)
            end do
        end if
        print*, "is_output    : ", this%is_output
    end subroutine print_element

    subroutine print_elements(this)
        implicit none
        class(stack) :: this
        integer(kind=8) :: e
        if ( allocated(this%list) ) then
            do e=1, size(this%list), 1
                call this%list(e)%print()
            end do
        else
            print*, "Not yet Generated."
        end if
    end subroutine print_elements

    subroutine print_architecture(this)
        implicit none
        class(neural_network) :: this
        call stacks(this%stack_id)%print()
    end subroutine print_architecture


    subroutine assign_var_names(this, vars)
        implicit none
        class(stack) :: this
        type(variable_), intent(inout) :: vars(:)
        integer(kind=8) :: v

        do v=1, size(vars), 1
            if (vars(v)%var_name == "None") then
                vars(v)%var_id = this%n_ids
                write(vars(v)%var_name, '("x_", I10.10)') this%n_ids
                this%n_ids = this%n_ids + 1
            end if
        end do
    end subroutine assign_var_names

    subroutine assign_var_name(this, var)
        implicit none
        class(stack) :: this
        type(variable_), intent(inout) :: var
        integer(kind=8) :: v

        if (var%var_name == "None") then
            var%var_id = this%n_ids
            write(var%var_name, '("x_", I10.10)') this%n_ids
            this%n_ids = this%n_ids + 1
        end if
    end subroutine assign_var_name
        

    subroutine set_operation_1in_1out(opr, operation_name, input_vars, output_var)
        implicit none
        class(*), target :: opr
        character(len=*) :: operation_name
        type(variable_)  :: input_vars
        type(variable_)  :: output_var

        type(element)    :: elm
        integer(kind=8)  :: i, id

        id = input_vars%stack_id
        call stacks(id)%assign_name(input_vars)
        call stacks(id)%assign_name(output_var)

        allocate(elm%opr, mold=opr)
        elm%opr => opr
        elm%var_name_out = output_var%var_name
        elm%var_id_out   = output_var%var_id
        elm%opr_name     = operation_name
        elm%var_names_in = [input_vars%var_name]
        elm%var_ids_in   = [input_vars%var_id]
        elm%stack_id     = id
        
        stacks(id)%list = [stacks(id)%list, elm]
        if (.not. any(input_vars%var_id == stacks(id)%idxs)) then
            stacks(id)%vars = [stacks(id)%vars, input_vars]
            stacks(id)%idxs = [stacks(id)%idxs, input_vars%var_id]
            ! print*, "append input", size(stacks(id)%vars)
        end if

        output_var%stack_id = id
        if (.not. any(output_var%var_id == stacks(id)%idxs)) then
            stacks(id)%vars = [stacks(id)%vars, output_var]
            stacks(id)%idxs = [stacks(id)%idxs, output_var%var_id]
            ! print*, "append output", size(stacks(id)%vars)
        end if
    end subroutine set_operation_1in_1out

    subroutine set_operation_2in_1out(opr, operation_name, input_var1, input_var2, output_var)
        implicit none
        class(*), target :: opr
        character(len=*) :: operation_name
        type(variable_)  :: input_var1, input_var2
        type(variable_)  :: output_var

        type(element)    :: elm
        integer(kind=8)  :: i, id

        id = input_var1%stack_id
        call stacks(id)%assign_name(input_var1)
        call stacks(id)%assign_name(input_var2)
        call stacks(id)%assign_name(output_var)

        allocate(elm%opr, mold=opr)
        elm%opr => opr
        elm%var_name_out = output_var%var_name
        elm%var_id_out   = output_var%var_id
        elm%opr_name     = operation_name
        elm%var_names_in = [input_var1%var_name, input_var2%var_name]
        elm%var_ids_in   = [input_var1%var_id, input_var2%var_id]
        elm%stack_id     = id
        
        stacks(id)%list = [stacks(id)%list, elm]
        if (.not. any(input_var1%var_id == stacks(id)%idxs)) then
            stacks(id)%vars = [stacks(id)%vars, input_var1]
            stacks(id)%idxs = [stacks(id)%idxs, input_var1%var_id]
            ! print*, "append input", size(stacks(id)%vars)
        end if
        if (.not. any(input_var2%var_id == stacks(id)%idxs)) then
            stacks(id)%vars = [stacks(id)%vars, input_var2]
            stacks(id)%idxs = [stacks(id)%idxs, input_var2%var_id]
            ! print*, "append input", size(stacks(id)%vars)
        end if

        output_var%stack_id = id
        if (.not. any(output_var%var_id == stacks(id)%idxs)) then
            stacks(id)%vars = [stacks(id)%vars, output_var]
            stacks(id)%idxs = [stacks(id)%idxs, output_var%var_id]
            ! print*, "append output", size(stacks(id)%vars)
        end if
    end subroutine set_operation_2in_1out

    function new_variable_s_(sclr)
        implicit none
        type(variable_) :: new_variable_s_
        real(kind=8), intent(in) :: sclr
        new_variable_s_%v = reshape( (/sclr/), shape = [1,1] )
    end function new_variable_s_
    
    function new_variable_m_(mtrx)
        implicit none
        type(variable_) :: new_variable_m_
        real(kind=8), intent(in) :: mtrx(:,:)
        new_variable_m_%v = mtrx(:,:)
    end function new_variable_m_

    function new_variable_()
        type(variable_) :: new_variable_
    end function new_variable_

    function forward_neural_network(this, input_vars) result(output_vars)
        implicit none
        class(neural_network) :: this
        type(variable_) :: input_vars(:)
        type(variable_), allocatable :: output_vars(:)
        stop "NotImplementedError"
    end function forward_neural_network 

    subroutine build_neural_network(this)
        implicit none
        class(neural_network) :: this
        stop "NotImplementedError"
    end subroutine build_neural_network 

    subroutine select_stack(this)
        implicit none
        class(neural_network) :: this
        integer(kind=8) :: s
        if (this%stack_id /= -1) then
            is_used_stacks(this%stack_id) = f_
            if (allocated(stacks(this%stack_id)%list)) deallocate(stacks(this%stack_id)%list)
            if (allocated(stacks(this%stack_id)%vars)) deallocate(stacks(this%stack_id)%vars)
            if (allocated(stacks(this%stack_id)%idxs)) deallocate(stacks(this%stack_id)%idxs)
            this%stack_id = -1
        end if

        do s=1, MAX_STACK_SIZE, 1
            if (.not. is_used_stacks(s)) exit 
        end do
        if (s == MAX_STACK_SIZE+1) then
            stop "There is no space. Increase stack size 'MAX_STACK_SIZE'."
        end if
        this%stack_id = s
        is_used_stacks(this%stack_id) = t_
        allocate(stacks(this%stack_id)%list(0))
        allocate(stacks(this%stack_id)%vars(0))
        allocate(stacks(this%stack_id)%idxs(0))
    end subroutine select_stack

    subroutine set_stack_id_multi_inputs(this, input_vars)
        implicit none
        class(neural_network) :: this
        type(variable_) :: input_vars(:)
        integer(kind=8) :: n

        do n=1, size(input_vars), 1
            input_vars(n)%stack_id = this%stack_id
        end do
    end subroutine set_stack_id_multi_inputs

    subroutine set_stack_id_single_input(this, input_var)
        implicit none
        class(neural_network) :: this
        type(variable_) :: input_var
        integer(kind=8) :: n

        input_var%stack_id = this%stack_id
    end subroutine set_stack_id_single_input

    subroutine init_single_input(this, input_var)
        implicit none
        class(neural_network) :: this
        type(variable_) :: input_var
        call this%select_stack()
        call this%set_stack_id_single_input(input_var)
    end subroutine init_single_input

    subroutine init_multi_inputs(this, input_vars)
        implicit none
        class(neural_network) :: this
        type(variable_) :: input_vars(:)
        call this%select_stack()
        call this%set_stack_id_multi_inputs(input_vars)
    end subroutine init_multi_inputs

    subroutine compile_neural_network(this)
        implicit none
        class(neural_network) :: this
        stop "NotImplementedError."
    end subroutine compile_neural_network

    subroutine get_output_variable_pointer(elm, output_var_ptr)
        implicit none
        type(element)            :: elm
        type(variable_), pointer :: output_var_ptr

        integer(kind=8)          :: output_var_id, stack_id

        nullify(output_var_ptr)
        stack_id = elm%stack_id
        output_var_id = elm%var_id_out
        output_var_ptr => stacks(stack_id)%vars(output_var_id)
    end subroutine get_output_variable_pointer

    subroutine get_input_variable_pointer_single(elm, input_var_ptr)
        implicit none
        type(element)            :: elm
        type(variable_), pointer :: input_var_ptr

        integer(kind=8)          :: input_var_id, stack_id

        nullify(input_var_ptr)
        stack_id = elm%stack_id
        input_var_id = elm%var_ids_in(1)
        input_var_ptr => stacks(stack_id)%vars(input_var_id)
    end subroutine get_input_variable_pointer_single

    subroutine get_input_variable_pointer_multi(elm, input_var1_ptr, input_var2_ptr)
        implicit none
        type(element)            :: elm
        type(variable_), pointer :: input_var1_ptr, input_var2_ptr

        integer(kind=8)          :: input_var_ids(2), stack_id

        nullify(input_var1_ptr)
        nullify(input_var2_ptr)
        stack_id = elm%stack_id
        input_var_ids(:) = elm%var_ids_in(:)
        input_var1_ptr => stacks(stack_id)%vars(input_var_ids(1))
        input_var2_ptr => stacks(stack_id)%vars(input_var_ids(2))
    end subroutine get_input_variable_pointer_multi



end module mod_wengert_list 