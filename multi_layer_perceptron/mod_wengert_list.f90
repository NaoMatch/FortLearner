module mod_wengert_list
    ! use mod_var
    use mod_const
    use mod_variable_in_variable
    implicit none

    type variable
        type(variable_in_variable) :: var
        type(variable_in_variable) :: grd
        real(kind=8), allocatable  :: v(:,:)
        real(kind=8), allocatable  :: g(:,:)
        logical(kind=4)            :: is_input=f_
        character(len=256)         :: var_name="None"
        integer(kind=8)            :: var_id
        integer(kind=8)            :: stack_id=-1
        integer(kind=8)            :: generation=0
        logical(kind=4)            :: require_grad=t_
        type(variable), allocatable :: input_vars(:)
        type(variable), pointer     :: var_ptr
        logical(kind=4)              :: is_learnable=f_
        type(optimizer), pointer    :: opt_ptr
    contains
        procedure :: sizes
    end type variable

    interface variable
        module procedure :: new_variable_s
        module procedure :: new_variable_v
        module procedure :: new_variable_m
    end interface variable
    
    !> Element Type of Wengert List
    type element
        integer(kind=8)                 :: elm_id
        integer(kind=8), allocatable    :: prev_elm_ids(:)
        integer(kind=8)                 :: generation
        character(len=256)              :: var_name_out    ! output variable name
        integer(kind=8)                 :: var_id_out=-1   ! output variable name
        integer(kind=8), allocatable    :: dim_out(:)   ! input variable name(s)
        character(len=256)              :: opr_name        ! operation name
        character(len=256), allocatable :: var_names_in(:) ! input variable name(s)
        integer(kind=8), allocatable    :: var_ids_in(:)   ! input variable name(s)
        integer(kind=8), allocatable    :: dim_in1(:)   ! input variable name(s)
        integer(kind=8), allocatable    :: dim_in2(:)   ! input variable name(s)
        integer(kind=8)                 :: stack_id=-1     ! output variable name
        logical(kind=4)                 :: is_output = f_
        class(*), pointer               :: opr => null()
        integer(kind=8)                 :: dim
    contains
        procedure :: print => print_element
    end type element

    type neural_network
        type(optimizer), pointer :: opt_ptr
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
        integer(kind=8) :: n_ids=1 !< number of used indices 
        type(element), allocatable   :: list(:) ! elements of wengert list
        type(variable), allocatable :: vars(:) ! input/intermediate/output variables
        type(variable), allocatable :: prms(:) ! parameters eg. weights and bias of dense layer
        integer(kind=8), allocatable :: idxs(:) ! variable index
    contains
        procedure :: print => print_all_elements
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
    logical(kind=4) :: is_used_stacks(MAX_STACK_SIZE) = f_

    interface operator (.eq.)
        module procedure compare_element
    end interface operator (.eq.)    

    interface allocate_var
        module procedure :: allocate_var_rank2
    end interface allocate_var

    interface shape
        module procedure shape_var
    end interface shape

    interface debug_print
        module procedure debug_print_1in_1out
        module procedure debug_print_2in_1out
    end interface debug_print


    type optimizer
        type(stack), pointer :: stack_ptr
        real(kind=8) :: learning_rate
        real(kind=8) :: momentum
    contains
        procedure :: sgd => sgd_optimizer
        procedure :: update => update_optimizer
    end type optimizer

    interface optimizer
        module procedure :: new_optimizer
    end interface optimizer

contains

    subroutine update_optimizer(this)
        implicit none
        class(optimizer) :: this
        integer(kind=8) :: i

        do i=1, size(this%stack_ptr%vars), 1
            if (this%stack_ptr%vars(i)%is_learnable) then
                this%stack_ptr%vars(i)%var_ptr%var = &
                        this%stack_ptr%vars(i)%var_ptr%var & 
                        - this%learning_rate*this%stack_ptr%vars(i)%grd
            end if
        end do
    end subroutine update_optimizer

    function new_optimizer()
        implicit none
        type(optimizer) :: new_optimizer
    end function new_optimizer

    subroutine sgd_optimizer(this, learning_rate, momentum)
        implicit none
        class(optimizer) :: this
        real(kind=8), intent(in) :: learning_rate, momentum
        this%learning_rate = learning_rate
        this%momentum = momentum
    end subroutine sgd_optimizer

    subroutine debug_print_1in_1out(file_name, line_num, elm, input_var_ptr, output_var_ptr, before)
        implicit none
        character(len=*), intent(in) :: file_name
        integer(kind=4), intent(in) :: line_num
        type(element), intent(in) :: elm
        type(variable), pointer :: input_var_ptr
        type(variable), pointer :: output_var_ptr
        type(variable_in_variable) :: hoge, fuga
        logical(kind=4) :: before
#ifdef _debug
        print*, "                ", file_name, line_num
        if (before) then
            print*, "                     before", elm%dim, elm%opr_name
            print*, "in:  ", input_var_ptr%var%dtype,  input_var_ptr%grd%dtype,  input_var_ptr%require_grad
            print*, "out: ", output_var_ptr%var%dtype, output_var_ptr%grd%dtype, output_var_ptr%require_grad
        else
            print*, "                      after", elm%dim, elm%opr_name
            hoge = input_var_ptr%grd
            fuga = output_var_ptr%grd
            if (input_var_ptr%grd%dtype>=1) hoge = sum(input_var_ptr%grd)
            if (output_var_ptr%grd%dtype>=1) fuga = sum(output_var_ptr%grd)
            print*, "in:  ", input_var_ptr%var%dtype,  input_var_ptr%grd%dtype,  input_var_ptr%require_grad, hoge%s
            print*, "out: ", output_var_ptr%var%dtype, output_var_ptr%grd%dtype, output_var_ptr%require_grad, fuga%s
        end if
        if (input_var_ptr%var%dtype/=input_var_ptr%grd%dtype .and. input_var_ptr%require_grad .and. .not. before) then
            print*, __FILE__, __LINE__
            stop
        end if
#endif
    end subroutine debug_print_1in_1out

    subroutine debug_print_2in_1out(file_name, line_num, elm, input_var1_ptr, input_var2_ptr, output_var_ptr, before)
        implicit none
        character(len=*), intent(in) :: file_name
        integer(kind=4), intent(in) :: line_num
        type(element), intent(in) :: elm
        type(variable), pointer :: input_var1_ptr
        type(variable), pointer :: input_var2_ptr
        type(variable), pointer :: output_var_ptr
        type(variable_in_variable) :: var1, var2, var3
        type(variable_in_variable) :: hoge
        logical(kind=4) :: before
#ifdef _debug
        print*, "                ", file_name, line_num
        if (before) then
            print*, "                     before", elm%dim, elm%opr_name
            print*, "in1: ", input_var1_ptr%var%dtype, input_var1_ptr%grd%dtype, input_var1_ptr%require_grad
            print*, "in2: ", input_var2_ptr%var%dtype, input_var2_ptr%grd%dtype, input_var2_ptr%require_grad
            print*, "out: ", output_var_ptr%var%dtype, output_var_ptr%grd%dtype, output_var_ptr%require_grad
        else
            print*, "                      after", elm%dim, elm%opr_name
            var1 = input_var1_ptr%grd
            var2 = input_var2_ptr%grd
            var3 = output_var_ptr%grd
            if (input_var1_ptr%grd%dtype>=1) var1 = sum(input_var1_ptr%grd)
            if (input_var2_ptr%grd%dtype>=1) var2 = sum(input_var2_ptr%grd)
            if (output_var_ptr%grd%dtype>=1) var3 = sum(output_var_ptr%grd)
            print*, "in1: ", input_var1_ptr%var%dtype, input_var1_ptr%grd%dtype, input_var1_ptr%require_grad, var1%s
            print*, "in2: ", input_var2_ptr%var%dtype, input_var2_ptr%grd%dtype, input_var2_ptr%require_grad, var2%s
            print*, "out: ", output_var_ptr%var%dtype, output_var_ptr%grd%dtype, output_var_ptr%require_grad, var3%s
        end if
        if (input_var1_ptr%var%dtype/=input_var1_ptr%grd%dtype .and. input_var1_ptr%require_grad .and. .not. before) then
            print*, __FILE__, __LINE__
            stop
        end if
        if (input_var2_ptr%var%dtype/=input_var2_ptr%grd%dtype .and. input_var2_ptr%require_grad .and. .not. before) then
            print*, __FILE__, __LINE__
            stop
        end if
#endif
    end subroutine debug_print_2in_1out

    function shape_var(var) result(res)
        implicit none
        type(variable), intent(in) :: var
        integer(kind=8), allocatable :: res(:)
        res = shape(var%var)
    end function shape_var
        
    function sizes(this) result(sample_size)
        implicit none
        class(variable) :: this
        integer(kind=8) :: sample_size
        sample_size = this%var%batch_sizes()
    end function sizes

    function compare_element(elm1, elm2) result(ret)
        implicit none
        type(element), intent(in) :: elm1
        type(element), intent(in) :: elm2
        logical(kind=4) :: ret
    
        ret = f_
        if (elm1%elm_id == elm2%elm_id) ret = t_
    end function compare_element

    subroutine print_element(this)
        implicit none
        class(element) :: this
        integer(kind=8) :: i
        print*, '*********************************************************************************************'
        print*, "elm_id :       ", int(this%elm_id)    ! output variable name
        print*, "var_name_out : ", trim(this%var_name_out), " -> ", int(this%dim_out)    ! output variable name
        print*, "var_id_out   : ", int(this%var_id_out)      ! output variable name
        print*, "opr_name     : ", trim(this%opr_name)       ! operation name
        if (allocated(this%var_names_in)) then
            print*, "var_names_in : ", trim(this%var_names_in(1)), " -> ", int(this%dim_in1)  ! input variable name(s)
            if (size(this%var_names_in)>1) print*, "var_names_in : ", trim(this%var_names_in(2)), " -> ", int(this%dim_in2)  ! input variable name(s)
        end if
        if (allocated(this%var_ids_in)) then
            do i=1, size(this%var_ids_in), 1
                print*, "var_ids_in   : ", int(this%var_ids_in(i)) ! input variable name(s)
            end do
        end if
        print*, "is_output    : ", this%is_output
    end subroutine print_element

    subroutine print_all_elements(this)
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
    end subroutine print_all_elements

    subroutine print_architecture(this)
        implicit none
        class(neural_network) :: this
        call stacks(this%stack_id)%print()
    end subroutine print_architecture


    subroutine assign_var_names(this, vars)
        implicit none
        class(stack) :: this
        type(variable), intent(inout) :: vars(:)
        integer(kind=8) :: v

        do v=1, size(vars), 1
            call this%assign_var_name(vars(v))
        end do
    end subroutine assign_var_names

    subroutine assign_var_name(this, var)
        implicit none
        class(stack) :: this
        type(variable), intent(inout) :: var
        integer(kind=8) :: v

        if (var%var_name == "None") then
            var%var_id = this%n_ids
            do while (any(var%var_id == this%idxs))
                var%var_id = var%var_id + 1
            end do
            write(var%var_name, '("x_", I10.10)') var%var_id
        end if
        this%n_ids = this%n_ids + 1
    end subroutine assign_var_name
        

    subroutine set_operation_1in_1out(opr, operation_name, input_var, output_var, dim)
        implicit none
        class(*), target :: opr
        character(len=*) :: operation_name
        type(variable), target  :: input_var
        type(variable)  :: output_var
        integer(kind=8)  :: dim

        type(element)    :: elm
        integer(kind=8)  :: i, id

        id = input_var%stack_id
        call stacks(id)%assign_name(input_var)
        call stacks(id)%assign_name(output_var)

        allocate(elm%opr, mold=opr)
        elm%opr => opr
        elm%elm_id       = size(stacks(id)%list)+1
        elm%generation   = input_var%generation
        elm%var_name_out = output_var%var_name
        elm%var_id_out   = output_var%var_id
        elm%dim_out      = shape(output_var%var)
        elm%opr_name     = operation_name
        elm%var_names_in = [input_var%var_name]
        elm%var_ids_in   = [input_var%var_id]
        elm%dim_in1      = shape(input_var%var)
        elm%stack_id     = id
        elm%dim          = dim
        
        stacks(id)%list = [stacks(id)%list, elm]
        if (.not. any(input_var%var_id == stacks(id)%idxs)) then
            stacks(id)%vars = [stacks(id)%vars, input_var]
            stacks(id)%idxs = [stacks(id)%idxs, input_var%var_id]
            if (input_var%is_learnable) then
                allocate(stacks(id)%vars(size(stacks(id)%vars))%var_ptr)
                stacks(id)%vars(size(stacks(id)%vars))%var_ptr => input_var
            end if
        end if

        output_var%stack_id = id
        output_var%generation = elm%generation + 1
        if (.not. any(output_var%var_id == stacks(id)%idxs)) then
            stacks(id)%vars = [stacks(id)%vars, output_var]
            stacks(id)%idxs = [stacks(id)%idxs, output_var%var_id]
        end if
    end subroutine set_operation_1in_1out

    subroutine set_operation_2in_1out(opr, operation_name, input_var1, input_var2, output_var, dim)
        implicit none
        class(*), target :: opr
        character(len=*) :: operation_name
        type(variable), target  :: input_var1, input_var2
        type(variable)  :: output_var
        integer(kind=8)  :: dim

        type(element)    :: elm
        integer(kind=8)  :: i, id, dim_set

        id = maxval([input_var1%stack_id, input_var2%stack_id])
        ! print*, '*********************************************************************************************'
        ! print*, "operation:  ", trim(operation_name)
        ! print*, "input_var1: ", input_var1%v
        ! print*, "input_var2: ", input_var2%stack_id
        ! print*, "output_var: ", output_var%stack_id
        call stacks(id)%assign_name(input_var1)
        call stacks(id)%assign_name(input_var2)
        call stacks(id)%assign_name(output_var)

        allocate(elm%opr, mold=opr)
        elm%opr => opr
        elm%elm_id       = size(stacks(id)%list)+1
        elm%generation   = maxval([input_var1%generation, input_var2%generation])
        elm%var_name_out = output_var%var_name
        elm%var_id_out   = output_var%var_id
        elm%dim_out      = shape(output_var%var)
        elm%opr_name     = operation_name
        elm%var_names_in = [input_var1%var_name, input_var2%var_name]
        elm%var_ids_in   = [input_var1%var_id, input_var2%var_id]
        elm%dim_in1      = shape(input_var1%var)
        elm%dim_in2      = shape(input_var2%var)
        elm%stack_id     = id
        elm%dim          = dim
        
        stacks(id)%list = [stacks(id)%list, elm]
        if (.not. any(input_var1%var_id == stacks(id)%idxs)) then
            stacks(id)%vars = [stacks(id)%vars, input_var1]
            stacks(id)%idxs = [stacks(id)%idxs, input_var1%var_id]
            ! print*, "append input", size(stacks(id)%vars)
            if (input_var1%is_learnable) then
                allocate(stacks(id)%vars(size(stacks(id)%vars))%var_ptr)
                stacks(id)%vars(size(stacks(id)%vars))%var_ptr => input_var1
            end if
        end if
        if (.not. any(input_var2%var_id == stacks(id)%idxs)) then
            stacks(id)%vars = [stacks(id)%vars, input_var2]
            stacks(id)%idxs = [stacks(id)%idxs, input_var2%var_id]
            ! print*, "append input", size(stacks(id)%vars)
            if (input_var2%is_learnable) then
                allocate(stacks(id)%vars(size(stacks(id)%vars))%var_ptr)
                stacks(id)%vars(size(stacks(id)%vars))%var_ptr => input_var2
            end if
        end if

        output_var%stack_id = id
        output_var%generation = elm%generation + 1
        if (.not. any(output_var%var_id == stacks(id)%idxs)) then
            stacks(id)%vars = [stacks(id)%vars, output_var]
            stacks(id)%idxs = [stacks(id)%idxs, output_var%var_id]
            ! print*, "append output", size(stacks(id)%vars)
        end if
    end subroutine set_operation_2in_1out

    
    function new_variable_s(sclr, stack_id, require_grad, is_learnable)
        implicit none
        type(variable) :: new_variable_s
        integer(kind=8), optional :: stack_id
        logical(kind=4), optional :: require_grad
        logical(kind=4), optional :: is_learnable
        real(kind=8), intent(in) :: sclr
        new_variable_s%var = variable_in_variable(sclr)
        if (present(stack_id)) new_variable_s%stack_id = stack_id
        if (present(require_grad)) new_variable_s%require_grad = require_grad
        if (present(is_learnable)) new_variable_s%is_learnable = is_learnable
    end function new_variable_s
    
    function new_variable_v(vctr, stack_id, require_grad, is_learnable)
        implicit none
        type(variable) :: new_variable_v
        integer(kind=8), optional :: stack_id
        logical(kind=4), optional :: require_grad
        logical(kind=4), optional :: is_learnable
        real(kind=8), intent(in) :: vctr(:)
        new_variable_v%var = variable_in_variable(vctr)
        if (present(stack_id)) new_variable_v%stack_id = stack_id
        if (present(require_grad)) new_variable_v%require_grad = require_grad
        if (present(is_learnable)) new_variable_v%is_learnable = is_learnable
    end function new_variable_v
    
    function new_variable_m(mtrx, stack_id, require_grad, is_learnable)
        implicit none
        type(variable) :: new_variable_m
        integer(kind=8), optional :: stack_id
        logical(kind=4), optional :: require_grad
        logical(kind=4), optional :: is_learnable
        real(kind=8), intent(in) :: mtrx(:,:)
        new_variable_m%var = variable_in_variable(mtrx)
        if (present(stack_id)) new_variable_m%stack_id = stack_id
        if (present(require_grad)) new_variable_m%require_grad = require_grad
        if (present(is_learnable)) new_variable_m%is_learnable = is_learnable
    end function new_variable_m

    function new_variable()
        type(variable) :: new_variable
    end function new_variable

    function forward_neural_network(this, input_vars) result(output_vars)
        implicit none
        class(neural_network) :: this
        type(variable) :: input_vars(:)
        type(variable), allocatable :: output_vars(:)
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
            stacks(this%stack_id)%n_ids = 1
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
        type(variable) :: input_vars(:)
        integer(kind=8) :: n

        do n=1, size(input_vars), 1
            input_vars(n)%stack_id = this%stack_id
        end do
    end subroutine set_stack_id_multi_inputs

    subroutine set_stack_id_single_input(this, input_var)
        implicit none
        class(neural_network) :: this
        type(variable) :: input_var
        integer(kind=8) :: n

        input_var%stack_id = this%stack_id
    end subroutine set_stack_id_single_input

    subroutine init_single_input(this, input_var)
        implicit none
        class(neural_network) :: this
        type(variable) :: input_var
        call this%select_stack()
        call this%set_stack_id_single_input(input_var)
        this%opt_ptr%stack_ptr => stacks(this%stack_id)
    end subroutine init_single_input

    subroutine init_multi_inputs(this, input_vars)
        implicit none
        class(neural_network) :: this
        type(variable) :: input_vars(:)
        call this%select_stack()
        call this%set_stack_id_multi_inputs(input_vars)
        this%opt_ptr%stack_ptr => stacks(this%stack_id)
    end subroutine init_multi_inputs

    subroutine compile_neural_network(this)
        implicit none
        class(neural_network) :: this
        stop "NotImplementedError."
    end subroutine compile_neural_network

    subroutine get_output_variable_pointer(elm, output_var_ptr)
        implicit none
        type(element)            :: elm
        type(variable), pointer :: output_var_ptr

        integer(kind=8)          :: output_var_id, stack_id

        nullify(output_var_ptr)
        stack_id = elm%stack_id
        output_var_id = get_list_index(elm%var_id_out, stacks(stack_id)%idxs)
        output_var_ptr => stacks(stack_id)%vars(output_var_id)
    end subroutine get_output_variable_pointer

    subroutine get_input_variable_pointer_single(elm, input_var_ptr)
        implicit none
        type(element)            :: elm
        type(variable), pointer :: input_var_ptr

        integer(kind=8)          :: input_var_id, stack_id

        nullify(input_var_ptr)
        stack_id = elm%stack_id
        input_var_id = elm%var_ids_in(1)
        input_var_id = get_list_index(elm%var_ids_in(1), stacks(stack_id)%idxs)
        input_var_ptr => stacks(stack_id)%vars(input_var_id)
    end subroutine get_input_variable_pointer_single

    subroutine get_input_variable_pointer_multi(elm, input_var1_ptr, input_var2_ptr)
        implicit none
        type(element)            :: elm
        type(variable), pointer :: input_var1_ptr, input_var2_ptr

        integer(kind=8)          :: input_var_ids(2), stack_id

        nullify(input_var1_ptr)
        nullify(input_var2_ptr)
        stack_id = elm%stack_id
        input_var_ids(:size(elm%var_ids_in(:))) = elm%var_ids_in(:)
        input_var_ids(1) = get_list_index(input_var_ids(1), stacks(stack_id)%idxs)
        input_var1_ptr => stacks(stack_id)%vars(input_var_ids(1))
        if (size(elm%var_ids_in(:))>1) then
            input_var_ids(2) = get_list_index(input_var_ids(2), stacks(stack_id)%idxs)
            input_var2_ptr => stacks(stack_id)%vars(input_var_ids(2))
        end if
    end subroutine get_input_variable_pointer_multi

    subroutine allocate_var_rank2(var, var_shape)
        implicit none
        type(variable) :: var
        integer(kind=4) :: var_shape(2)
        allocate(var%v(var_shape(1), var_shape(2)))
    end subroutine allocate_var_rank2

    function get_list_index(var_id, var_indices) result(list_id)
        implicit none
        integer(kind=8) :: var_id
        integer(kind=8) :: var_indices(:)
        integer(kind=8) :: list_id, i

        do i=1, size(var_indices), 1
            if (var_id == var_indices(i)) exit
        end do
        list_id = i
    end function get_list_index

end module mod_wengert_list 