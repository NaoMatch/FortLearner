module mod_wengert_list
    ! use mod_var
    use mod_const
    use mod_random
    use mod_sort
    use mod_variable_in_variable
    implicit none

    type variable
        type(variable_in_variable) :: var
        type(variable_in_variable) :: grd
        type(variable_in_variable) :: grd_delta
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
        logical(kind=4)             :: is_learnable=f_
        type(optimizer), pointer    :: opt_ptr
        logical(kind=4)             :: create_list=t_
    contains
        procedure :: sizes
        procedure :: ncolumns
    end type variable

    interface variable
        module procedure :: new_variable_s_r8
        module procedure :: new_variable_v_r8
        module procedure :: new_variable_m_r8
        module procedure :: new_variable_s_i8
        module procedure :: new_variable_v_i8
        module procedure :: new_variable_m_i8
    end interface variable
    
    !> Element Type of Wengert List
    type element
        integer(kind=8)                 :: elm_id
        integer(kind=8), allocatable    :: prev_elm_ids(:)
        integer(kind=8)                 :: generation
        character(len=256)              :: var_name_out    ! output variable name
        integer(kind=8)                 :: var_id_out=-1   ! output variable name
        integer(kind=8)                 :: stack_id_out=-1   ! output variable name
        integer(kind=8), allocatable    :: dim_out(:)   ! input variable name(s)
        character(len=256)              :: opr_name        ! operation name
        character(len=256), allocatable :: var_names_in(:) ! input variable name(s)
        integer(kind=8), allocatable    :: var_ids_in(:)   ! input variable name(s)
        integer(kind=8), allocatable    :: stack_ids_in(:)   ! input variable name(s)
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
        logical(kind=4) :: create_list=t_
    contains
        procedure :: set_stack_id_single_input
        procedure :: set_stack_id_multi_inputs
        procedure, pass :: preprocess_single_input
        procedure, pass :: preprocess_multi_inputs
        generic :: preprocess => preprocess_single_input, preprocess_multi_inputs
        procedure, pass :: postprocess_single_input
        procedure, pass :: postprocess_multi_inputs
        generic :: postprocess => postprocess_single_input, postprocess_multi_inputs
        procedure :: no_list
        procedure :: build => build_neural_network
        procedure :: print => print_architecture
    end type neural_network
        
    type stack
        integer(kind=8) :: stack_id=-1
        integer(kind=8) :: n_ids=1 !< number of used indices 
        type(element), allocatable   :: list(:) ! elements of wengert list
        type(variable), allocatable :: vars(:) ! input/intermediate/output variables
        integer(kind=8), allocatable :: idxs(:) ! variable index
        integer(kind=8), allocatable :: free_ids(:) ! to free temporary stack, store concatenated stack ids
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
    type(stack), target :: stacks_tmp(MAX_STACK_SIZE_TMP)
    logical(kind=4) :: is_used_stacks(MAX_STACK_SIZE) = f_

    interface operator (.eq.)
        module procedure compare_element
    end interface operator (.eq.)    

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
        real(kind=8) :: top_k
    contains
        procedure :: sgd => sgd_optimizer
        procedure :: update => update_optimizer
    end type optimizer

    interface optimizer
        module procedure :: new_optimizer
    end interface optimizer

    type batch_idxs_generator
        integer(kind=8) :: n_samples
        integer(kind=8) :: n_mini_batch
        integer(kind=8) :: n_loop
        logical(kind=4) :: shuffle=t_
        integer(kind=8), allocatable :: batch_idxs(:)
    contains
        procedure :: get_batch_idxs
    end type batch_idxs_generator

    interface batch_idxs_generator
        module procedure new_batch_idxs_generator
    end interface batch_idxs_generator        


contains

    function get_batch_idxs(this, loop_index) result(batch_idxs)
        implicit none
        class(batch_idxs_generator) :: this
        integer(kind=8) :: loop_index
        integer(kind=8), allocatable :: batch_idxs(:)
        integer(kind=8) :: i, ini, fin

        ini = 1 + this%n_mini_batch*(loop_index-1)
        fin = 1 + this%n_mini_batch*loop_index-1
        allocate(batch_idxs(this%n_mini_batch))

        batch_idxs = this%batch_idxs(ini:fin)
    end function get_batch_idxs

    function new_batch_idxs_generator(n_samples, n_mini_batch, shuffle) result(res)
        implicit none
        integer(kind=8), intent(in) :: n_samples
        integer(kind=8), intent(in) :: n_mini_batch
        logical(kind=4), optional   :: shuffle
        type(batch_idxs_generator)  :: res
        integer(kind=8) :: n
        res%n_samples = n_samples
        res%n_mini_batch = n_mini_batch
        if (present(shuffle)) res%shuffle = shuffle
        res%n_loop = n_samples / n_mini_batch

        allocate(res%batch_idxs(n_samples))
        do n=1, n_samples, 1
            res%batch_idxs(n) = n
        end do
        call permutation(res%batch_idxs, n_samples)
    end function new_batch_idxs_generator
        



    subroutine no_list(this)
        implicit none
        class(neural_network) :: this
        this%create_list = f_
    end subroutine no_list
    
    subroutine minimal_effort(var_i, k)
        implicit none
        type(variable_in_variable) :: var_i
        real(kind=8), intent(in) :: k

        real(kind=8), allocatable :: grd_vals(:)
        real(kind=8), allocatable :: mask_m(:), mask_v(:)
        real(kind=8) :: threshold_val, zero
        integer(kind=8) :: n_params, n_params_k, n_rows, n_cols, i, j, factor

        if (k==0d0) return
        if (var_i%dtype<=0) return

        if     (var_i%dtype==2) then
            n_params = size(var_i%m)
            allocate(grd_vals(n_params))
            grd_vals(:) = abs(reshape(var_i%m, shape=[n_params]))
        elseif (var_i%dtype==1) then
            n_params = size(var_i%v)
            allocate(grd_vals(n_params))
            grd_vals(:) = abs(var_i%v)
        end if

        call pbucket_sort(grd_vals, n_params)
        n_params_k = maxval([n_params * (1d0 - k), 1d0])
        threshold_val = grd_vals(n_params_k)

        if     (var_i%dtype==2) then
            n_rows = size(var_i%m, dim=1)
            n_cols = size(var_i%m, dim=2)
            do j=1, n_cols, 1
                do i=1, n_rows, 1
                    factor = abs(var_i%m(i,j)) >= threshold_val
                    var_i%m(i,j) = var_i%m(i,j) * factor
                end do
            end do
        elseif (var_i%dtype==1) then
            n_params = size(var_i%v)
            do i=1, n_params, 1
                factor = abs(var_i%v(i)) >= threshold_val
                var_i%v(i) = var_i%v(i) * factor
            end do
        end if

    end subroutine minimal_effort

    subroutine update_optimizer(this)
        implicit none
        class(optimizer) :: this
        integer(kind=8) :: i
        type(variable) :: sum_var
        type(variable), pointer :: param_ptr

        do i=1, size(this%stack_ptr%vars), 1
            if (this%stack_ptr%vars(i)%is_learnable) then
                param_ptr => this%stack_ptr%vars(i)%var_ptr

                call minimal_effort(this%stack_ptr%vars(i)%grd, k=this%top_k)
                if (this%momentum==0d0) then
                    param_ptr%var = param_ptr%var - this%learning_rate * this%stack_ptr%vars(i)%grd
                else
                    if (param_ptr%grd_delta%dtype==-1) then
                        call param_ptr%grd_delta%zeros(source=this%stack_ptr%vars(i)%var)
                        param_ptr%grd_delta = param_ptr%grd_delta * 0d0
                    end if
                    param_ptr%grd_delta = & 
                          this%momentum      * param_ptr%grd_delta &
                        - this%learning_rate * this%stack_ptr%vars(i)%grd
                    param_ptr%var = param_ptr%var + param_ptr%grd_delta
                end if
            end if
        end do
    end subroutine update_optimizer

    function new_optimizer(top_k)
        implicit none
        type(optimizer) :: new_optimizer
        real(kind=8), optional :: top_k
        if (present(top_k)) new_optimizer%top_k = top_k
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
        
    function ncolumns(this) result(sample_size)
        implicit none
        class(variable) :: this
        integer(kind=8) :: sample_size
        sample_size = this%var%ncolumns_viv()
    end function ncolumns

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
        print*, "elm_id :       ", int(this%elm_id, kind=8)    ! output variable name
        print*, "var_name_out : ", trim(this%var_name_out), " -> ", int(this%dim_out)    ! output variable name
        print*, "var_id_out   : ", int(this%var_id_out, kind=8)      ! output variable name
        print*, "stack_id_out : ", int(this%stack_id_out, kind=8)      ! output variable name
        print*, "opr_name     : ", trim(this%opr_name)       ! operation name
        if (allocated(this%var_names_in)) then
            print*, "var_names_in : ", trim(this%var_names_in(1)), " -> ", int(this%dim_in1)  ! input variable name(s)
            if (size(this%var_names_in)>1) print*, "var_names_in : ", trim(this%var_names_in(2)), " -> ", int(this%dim_in2)  ! input variable name(s)
        end if
        if (allocated(this%var_ids_in)) then
            do i=1, size(this%var_ids_in), 1
                print*, "var_ids_in   : ", int(this%var_ids_in(i), kind=8) ! input variable name(s)
                print*, "stack_id_in  : ", int(this%stack_ids_in(i), kind=8)      ! output variable name
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
            var%var_id = this%n_ids + this%stack_id
            do while (any(var%var_id == this%idxs))
                var%var_id = var%var_id + 1
            end do
            write(var%var_name, '("x_", I19.19)') var%var_id
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
        output_var%create_list = input_var%create_list
        if (.not. input_var%create_list) return
        call stacks(id)%assign_name(input_var)
        call stacks(id)%assign_name(output_var)

        allocate(elm%opr, mold=opr)
        elm%opr => opr
        elm%elm_id       = size(stacks(id)%list)+1+(id * 10000000000_8)
        elm%generation   = input_var%generation
        elm%var_name_out = output_var%var_name
        elm%stack_id_out = output_var%stack_id
        elm%var_id_out   = output_var%var_id
        elm%dim_out      = shape(output_var%var)
        elm%opr_name     = operation_name
        elm%var_names_in = [input_var%var_name]
        elm%var_ids_in   = [input_var%var_id]
        elm%stack_ids_in = [input_var%stack_id]
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

    subroutine concat_stacks(id, id_tmp)
        implicit none
        integer(kind=8), intent(in) :: id, id_tmp
        integer(kind=8) :: len_vars, len_vars_tmp, v

        stacks(id)%list = [stacks(id)%list, stacks(id_tmp)%list]
        len_vars = size(stacks(id)%vars)
        len_vars_tmp = size(stacks(id_tmp)%vars)
        stacks(id)%vars = [stacks(id)%vars, stacks(id_tmp)%vars]
        do v=len_vars+1, len_vars+len_vars_tmp, 1
            if (associated(stacks(id)%vars(v)%var_ptr)) then
                stacks(id)%vars(v)%var_ptr => stacks(id_tmp)%vars(v-len_vars)%var_ptr
            end if
        end do
        stacks(id)%idxs = [stacks(id)%idxs, stacks(id_tmp)%idxs]
        stacks(id)%free_ids = [stacks(id)%free_ids, id, id_tmp]
        stacks(id_tmp)%free_ids = [stacks(id_tmp)%free_ids, id, id_tmp]
        ! print*, '*********************************************************************************************'
        ! print*, '*********************************************************************************************'
        ! print*, "concating"
        ! print*, "    -----: id =         ", id
        ! print*, "    -----: id_tmp =     ", id_tmp
        ! print*, "    -----: count() =    ", count(is_used_stacks)
        ! print*, "    -----: to_be_free = ", stacks(id)%free_ids
    end subroutine concat_stacks

    subroutine set_operation_2in_1out(opr, operation_name, input_var1, input_var2, output_var, dim)
        implicit none
        class(*), target :: opr
        character(len=*) :: operation_name
        type(variable), target  :: input_var1, input_var2
        type(variable)  :: output_var
        integer(kind=8)  :: dim

        type(element)    :: elm
        integer(kind=8)  :: i, id, id_min, dim_set

        id = maxval([input_var1%stack_id, input_var2%stack_id])
        id_min = minval([input_var1%stack_id, input_var2%stack_id])
        if (id==-1) id = select_stack_id(-1_8)
        if (id /= id_min .and. id_min/=-1) call concat_stacks(id, id_min)
        
        output_var%create_list = all([input_var1%create_list, input_var2%create_list])
        if (.not. input_var1%create_list) return
        if (.not. input_var2%create_list) return
        call stacks(id)%assign_name(input_var1)
        call stacks(id)%assign_name(input_var2)
        call stacks(id)%assign_name(output_var)

        allocate(elm%opr, mold=opr)
        elm%opr => opr
        elm%elm_id       = size(stacks(id)%list)+1 + (id * 10000000000_8)
        elm%generation   = maxval([input_var1%generation, input_var2%generation])
        elm%var_name_out = output_var%var_name
        elm%stack_id_out = output_var%stack_id
        elm%var_id_out   = output_var%var_id
        elm%dim_out      = shape(output_var%var)
        elm%opr_name     = operation_name
        elm%var_names_in = [input_var1%var_name, input_var2%var_name]
        elm%var_ids_in   = [input_var1%var_id, input_var2%var_id]
        elm%stack_ids_in = [input_var1%stack_id, input_var2%stack_id]
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

    
    function new_variable_s_r8(sclr, stack_id, require_grad, is_learnable)
        implicit none
        type(variable) :: new_variable_s_r8
        integer(kind=8), optional :: stack_id
        logical(kind=4), optional :: require_grad
        logical(kind=4), optional :: is_learnable
        real(kind=8), intent(in) :: sclr
        new_variable_s_r8%var = variable_in_variable(sclr)
        if (present(stack_id)) new_variable_s_r8%stack_id = stack_id
        if (present(require_grad)) new_variable_s_r8%require_grad = require_grad
        if (present(is_learnable)) new_variable_s_r8%is_learnable = is_learnable
    end function new_variable_s_r8
    
    function new_variable_v_r8(vctr, stack_id, require_grad, is_learnable)
        implicit none
        type(variable) :: new_variable_v_r8
        integer(kind=8), optional :: stack_id
        logical(kind=4), optional :: require_grad
        logical(kind=4), optional :: is_learnable
        real(kind=8), intent(in) :: vctr(:)
        new_variable_v_r8%var = variable_in_variable(vctr)
        if (present(stack_id)) new_variable_v_r8%stack_id = stack_id
        if (present(require_grad)) new_variable_v_r8%require_grad = require_grad
        if (present(is_learnable)) new_variable_v_r8%is_learnable = is_learnable
    end function new_variable_v_r8
    
    function new_variable_m_r8(mtrx, stack_id, require_grad, is_learnable)
        implicit none
        type(variable) :: new_variable_m_r8
        integer(kind=8), optional :: stack_id
        logical(kind=4), optional :: require_grad
        logical(kind=4), optional :: is_learnable
        real(kind=8), intent(in) :: mtrx(:,:)
        new_variable_m_r8%var = variable_in_variable(mtrx)
        if (present(stack_id)) new_variable_m_r8%stack_id = stack_id
        if (present(require_grad)) new_variable_m_r8%require_grad = require_grad
        if (present(is_learnable)) new_variable_m_r8%is_learnable = is_learnable
    end function new_variable_m_r8

    
    function new_variable_s_i8(sclr, stack_id, require_grad, is_learnable)
        implicit none
        type(variable) :: new_variable_s_i8
        integer(kind=8), optional :: stack_id
        logical(kind=4), optional :: require_grad
        logical(kind=4), optional :: is_learnable
        integer(kind=8), intent(in) :: sclr
        new_variable_s_i8%var = variable_in_variable(sclr)
        if (present(stack_id)) new_variable_s_i8%stack_id = stack_id
        if (present(require_grad)) new_variable_s_i8%require_grad = require_grad
        if (present(is_learnable)) new_variable_s_i8%is_learnable = is_learnable
    end function new_variable_s_i8
    
    function new_variable_v_i8(vctr, stack_id, require_grad, is_learnable)
        implicit none
        type(variable) :: new_variable_v_i8
        integer(kind=8), optional :: stack_id
        logical(kind=4), optional :: require_grad
        logical(kind=4), optional :: is_learnable
        integer(kind=8), intent(in) :: vctr(:)
        new_variable_v_i8%var = variable_in_variable(vctr)
        if (present(stack_id)) new_variable_v_i8%stack_id = stack_id
        if (present(require_grad)) new_variable_v_i8%require_grad = require_grad
        if (present(is_learnable)) new_variable_v_i8%is_learnable = is_learnable
    end function new_variable_v_i8
    
    function new_variable_m_i8(mtrx, stack_id, require_grad, is_learnable)
        implicit none
        type(variable) :: new_variable_m_i8
        integer(kind=8), optional :: stack_id
        logical(kind=4), optional :: require_grad
        logical(kind=4), optional :: is_learnable
        integer(kind=8), intent(in) :: mtrx(:,:)
        new_variable_m_i8%var = variable_in_variable(mtrx)
        if (present(stack_id)) new_variable_m_i8%stack_id = stack_id
        if (present(require_grad)) new_variable_m_i8%require_grad = require_grad
        if (present(is_learnable)) new_variable_m_i8%is_learnable = is_learnable
    end function new_variable_m_i8

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

    recursive subroutine collect_concatinated_stack_ids(stack_ids, start_stack_id)
        implicit none
        integer(kind=8), allocatable, intent(inout) :: stack_ids(:)
        integer(kind=8), intent(in) :: start_stack_id
        integer(kind=8) :: s
        integer(kind=8), allocatable :: tmp_stack_ids(:)

        if (allocated(stacks(start_stack_id)%free_ids)) then
            stack_ids = [stack_ids, stacks(start_stack_id)%free_ids(:)]
            tmp_stack_ids = stacks(start_stack_id)%free_ids(:)
            deallocate(stacks(start_stack_id)%free_ids)
            do s=1, size(tmp_stack_ids), 1
                call collect_concatinated_stack_ids(stack_ids, tmp_stack_ids(s))
            end do
        end if
    end subroutine collect_concatinated_stack_ids

    subroutine free_stack(stack_id)
        implicit none
        integer(kind=8), intent(in) :: stack_id
        integer(kind=8) :: s, s_id
        integer(kind=8), allocatable :: stack_ids(:)

        if (stack_id /= -1) then
            allocate(stack_ids(0))
            call collect_concatinated_stack_ids(stack_ids, stack_id)
            do s=1, size(stack_ids), 1
                s_id = stack_ids(s)
                ! print*, '*********************************************************************************************'
                ! print*, '*********************************************************************************************'
                ! print*, "freeing:   "
                ! print*, "     ----: stack_id =    ", s_id
                ! print*, "     ----: count() =     ", count(is_used_stacks)
                stacks(s_id)%n_ids = 1
                is_used_stacks(s_id) = f_
                if (allocated(stacks(s_id)%list)) deallocate(stacks(s_id)%list)
                if (allocated(stacks(s_id)%vars)) deallocate(stacks(s_id)%vars)
                if (allocated(stacks(s_id)%idxs)) deallocate(stacks(s_id)%idxs)
                if (allocated(stacks(s_id)%free_ids)) deallocate(stacks(s_id)%free_ids)
            end do
        end if
    end subroutine free_stack

    function select_stack_id(current_stack_id) result(new_stack_id)
        implicit none
        integer(kind=8), intent(in) :: current_stack_id
        integer(kind=8) :: new_stack_id
        
        integer(kind=8) :: i, s

        call free_stack(current_stack_id)
        
        do s=1, MAX_STACK_SIZE, 1
            if (.not. is_used_stacks(s)) exit 
        end do
        if (s == MAX_STACK_SIZE+1) then
            stop "There is no space. Increase stack size 'MAX_STACK_SIZE'."
        end if

        new_stack_id = s
        is_used_stacks(new_stack_id) = t_
        stacks(new_stack_id)%stack_id = new_stack_id * 10000000000_8
        allocate(stacks(new_stack_id)%list(0))
        allocate(stacks(new_stack_id)%vars(0))
        allocate(stacks(new_stack_id)%idxs(0))
        allocate(stacks(new_stack_id)%free_ids(0))
    end function select_stack_id

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

    subroutine preprocess_single_input(this, input_var)
        implicit none
        class(neural_network) :: this
        type(variable) :: input_var
        this%stack_id = select_stack_id(this%stack_id)
        call this%set_stack_id_single_input(input_var)
        this%opt_ptr%stack_ptr => stacks(this%stack_id)
    end subroutine preprocess_single_input

    subroutine preprocess_multi_inputs(this, input_vars)
        implicit none
        class(neural_network) :: this
        type(variable) :: input_vars(:)
        this%stack_id = select_stack_id(this%stack_id)
        call this%set_stack_id_multi_inputs(input_vars)
        this%opt_ptr%stack_ptr => stacks(this%stack_id)
    end subroutine preprocess_multi_inputs


    subroutine postprocess_single_input(this, output_var)
        implicit none
        class(neural_network) :: this
        type(variable) :: output_var
        integer(kind=8) :: e
        this%create_list = t_
        this%opt_ptr%stack_ptr => stacks(output_var%stack_id)
        this%stack_id = output_var%stack_id
        do e=1, size(stacks(this%stack_id)%list), 1
            stacks(this%stack_id)%list(e)%stack_id = this%stack_id
        end do
    end subroutine postprocess_single_input

    subroutine postprocess_multi_inputs(this, output_vars)
        implicit none
        class(neural_network) :: this
        type(variable) :: output_vars(:)
        integer(kind=8) :: v
        do v=1, size(output_vars), 1
            call this%postprocess_single_input(output_vars(v))
        end do
    end subroutine postprocess_multi_inputs


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