module mod_variable
    use mod_csr
    use mod_config
    implicit none

    type jagged_matrix
        real(kind=8), allocatable :: g(:,:)
        type(csr_matrix), allocatable :: csr_g
    end type jagged_matrix

    type velement
        real(kind=8), allocatable :: v(:,:)
        real(kind=8), allocatable :: g(:,:)
        real(kind=8), allocatable :: c(:,:)
        type(csr_matrix), allocatable :: csr_v
        type(csr_matrix), allocatable :: csr_g
        type(csr_matrix), allocatable :: csr_c
        integer(kind=8) :: fid=-1
        integer(kind=8) :: lid=-1
        character(len=:), allocatable :: name
        logical(kind=4) :: is_parameter = .false.
        logical(kind=4) :: is_train = .true.
    end type velement

    type variable
        integer(kind=8) :: id = -1
        integer(kind=8) :: generation = 0
        integer(kind=8), allocatable :: param_ids(:)
        integer(kind=8), allocatable :: non_param_ids(:)
    contains
        procedure :: backward
        procedure :: set_name
        procedure :: get_grad
        procedure :: get_data
        procedure :: get_shape
        procedure :: clear_grad
        procedure :: is_alloc
    end type variable 

    interface variable
        module procedure new_variable_r8_scl
        module procedure new_variable_r8_vec
        module procedure new_variable_r8_mat
        module procedure new_variable_r8_csr
    end interface variable    

    type felement
        integer(kind=8) :: generation
        class(*), allocatable :: func
    end type felement

    type base_function
        ! Common Part
        character(len=32) :: fname = "None"
        integer(kind=8)   :: n_in=-1, n_out=-1
        integer(kind=8)   :: id_in_1=-1, id_in_2=-1
        integer(kind=8)   :: id_out_1=-1, id_out_2=-1
        integer(kind=8)   :: shape_in_1(2) = [-1,-1], shape_in_2(2) = [-1,-1]
        integer(kind=8)   :: shape_out_1(2) = [-1,-1], shape_out_2(2) = [-1,-1]

        ! Summation & Spreading
        integer(kind=8)   :: dim=-1

        ! Power
        real(kind=8)      :: pow=-1

        ! Reshaping
        integer(kind=8)      :: shape(2) = [-1,-1]

        ! Spreading
        integer(kind=8)      :: ncopies = -1

        ! GetItem
        integer(kind=8), allocatable :: indices(:)

        ! Dropout
        real(kind=8) :: drop_ratio = 0d0
        real(kind=8), allocatable :: mask(:,:)

        ! Binary Cross Entropy
        character(len=4) :: reduction="mean"
        integer(kind=8) :: n_classes = -1

        ! CSR2DENSE
        integer(kind=8) :: top_k = -1
        character(len=16) :: how = "filter"
    contains
        procedure :: act_1in_1out, act_2in_1out
        generic   :: act => act_1in_1out, act_2in_1out

        procedure :: forward_1in_1out, forward_2in_1out
        procedure :: forward_1in_csr_1out, forward_1in_1out_csr, forward_2in_1out_csr
        generic   :: forward => forward_1in_1out, forward_2in_1out

        procedure :: backward_1in_2out, backward_1in_2out_csr
        generic   :: backward => backward_1in_2out

        procedure :: append_new_element_to_stack
    end type base_function

    type(velement), allocatable, target :: vstack(:)
    type(felement), allocatable, target :: fstack(:)

    type layer_parameter
        type(variable), pointer :: ptr
    end type layer_parameter

    type base_layer
        character(len=32) :: lname = "None"
        integer(kind=8), allocatable :: param_ids(:)
        ! Linear
        integer(kind=8) :: in_size=-1, out_size=-1
        logical(kind=4) :: no_bias=.false.
    contains
        procedure :: act
        procedure :: forward
        procedure :: set_params
    end type base_layer

    type, extends(base_layer) :: base_model
    end type base_model

    type :: optimizer
        real(kind=8) :: lr
    contains
        procedure :: update
        procedure :: update_one_param
        procedure :: clear_grads
    end type optimizer


    interface set_mode_to_variable
        procedure :: set_mode_to_variable_id
        procedure :: set_mode_to_variable_var
    end interface set_mode_to_variable

contains

    subroutine clear_stack(var)
        implicit none
        integer(kind=8) :: p, id
        type(variable) :: var

        integer(kind=8), allocatable :: var_ids(:)

        do p=1, size(var%param_ids), 1
            id = var%param_ids(p)
            if (allocated(vstack(id)%g)) deallocate(vstack(id)%g)
        end do

        do p=1, size(var%non_param_ids), 1
            id = var%non_param_ids(p)
            if (allocated(vstack(id)%v)) deallocate(vstack(id)%v)
            if (allocated(vstack(id)%g)) deallocate(vstack(id)%g)
        end do

        ! do p=1, size(vstack), 1
        !     id = p
        !     if (.not. vstack(id)%is_train) then
        !         if (allocated(vstack(id)%v)) deallocate(vstack(id)%v)
        !         if (allocated(vstack(id)%g)) deallocate(vstack(id)%g)            
        !     end if
        ! end do

        deallocate(fstack)
        allocate(fstack(0))
    end subroutine clear_stack


    function unused_stack_id() result(id)
        implicit none
        integer(kind=8) :: id

        integer(kind=8) :: n_stacks, s

        id = -1
        n_stacks = size(vstack)
        ! print*, '*********************************************************************************************'
        do s=1, n_stacks, 1
            ! print*, "stack id: ", s, allocated(vstack(s)%v)
            if ((.not. allocated(vstack(s)%v)) .and. (.not. allocated(vstack(s)%csr_v))) then
                id = s
                exit
            end if
        end do
        ! print*, '*********************************************************************************************'
    end function unused_stack_id

    function act(this, var_in) result(var_out)
        implicit none
        class(base_layer) :: this
        type(variable) :: var_in
        type(variable) :: var_out

        var_out = this%forward(var_in)
    end function act

    function forward(this, var_in) result(var_out)
        implicit none
        class(base_layer) :: this
        type(variable) :: var_in
        type(variable) :: var_out
        stop "NotImplementedError"
    end function forward

    subroutine set_params(this, v_param) 
        implicit none
        class(base_layer), intent(inout) :: this
        type(variable) :: v_param

        if (.not. allocated(this%param_ids)) allocate(this%param_ids(0))

        this%param_ids = [this%param_ids, v_param%id]
    end subroutine set_params


    subroutine pop_by_index(flist, glist, pop_index)
        implicit none
        integer(kind=8), allocatable, intent(inout) :: flist(:)
        integer(kind=8), allocatable, intent(inout) :: glist(:)
        integer(kind=8), intent(in) :: pop_index

        if (size(flist) == 1) then
            deallocate(flist)
            allocate(flist(0))
            deallocate(glist)
            allocate(glist(0))
        else
            flist = [flist(1:pop_index-1), flist(pop_index+1:)]
            glist = [glist(1:pop_index-1), glist(pop_index+1:)]
        end if
    end subroutine pop_by_index

    subroutine init_stack()
        implicit none
        integer(kind=8) :: i, n_hold
        integer(kind=8) :: last_hold_idx

        if (allocated(vstack)) deallocate(vstack)
        allocate(vstack(0))

        if (allocated(fstack)) deallocate(fstack)
        allocate(fstack(0))
    end subroutine init_stack

    subroutine append_new_element_to_stack(this, fid, vid)
        implicit none
        class(base_function) :: this
        integer(kind=8), intent(inout) :: fid
        integer(kind=8), intent(inout) :: vid

        type(felement) :: func
        type(velement) :: elm

        integer(kind=8) :: id

        id = unused_stack_id()
        if (id == -1) then
            vstack = [vstack, elm]
            vid = size(vstack)
        else
            vid = id
        end if

        fstack = [fstack, func]
        fid = size(fstack)
    end subroutine append_new_element_to_stack

    subroutine set_mode_to_variable_id(id)
        implicit none
        integer(kind=8) :: id

        vstack(id)%is_train = is_train
    end subroutine set_mode_to_variable_id

    subroutine set_mode_to_variable_var(var)
        implicit none
        type(variable) :: var
        vstack(var%id)%is_train = is_train
    end subroutine set_mode_to_variable_var

    subroutine train_mode()
        implicit none

        integer(kind=8) :: id, p
        is_train = .true.

        do p=1, size(vstack), 1
            id = p
            if (.not. vstack(id)%is_train) then
                if (allocated(vstack(id)%v)) deallocate(vstack(id)%v)
                if (allocated(vstack(id)%g)) deallocate(vstack(id)%g)            
            end if
        end do
    end subroutine train_mode
    
    subroutine test_mode()
        implicit none
        is_train = .false.
    end subroutine test_mode


    subroutine set_name(this, name)
        implicit none
        class(variable) :: this
        character(len=*) :: name

        vstack(this%id)%name = name
    end subroutine set_name

    subroutine clear_grad(this)
        implicit none
        class(variable) :: this

        if (allocated(vstack(this%id)%g)) then
            vstack(this%id)%g = 0d0
        end if
    end subroutine clear_grad

    function get_grad(this) result(grad)
        implicit none
        class(variable) :: this
        real(kind=8), allocatable :: grad(:,:)

        if (this%id == -1) then
            stop "'variable' is not assigned."
        end if
        allocate(grad, source=vstack(this%id)%g)
    end function get_grad

    function get_data(this) result(grad)
        implicit none
        class(variable) :: this
        real(kind=8), allocatable :: grad(:,:)

        if (this%id == -1) then
            stop "'variable' is not assigned."
        end if
        allocate(grad, source=vstack(this%id)%v)
    end function get_data

    function get_shape(this) result(dshape)
        implicit none
        class(variable) :: this
        integer(kind=8) :: dshape(2)

        if (allocated(vstack(this%id)%csr_v)) then
            dshape(1) = vstack(this%id)%csr_v%n_rows
            dshape(2) = vstack(this%id)%csr_v%n_cols
        else
            dshape = shape(vstack(this%id)%v)
        end if
    end function get_shape

    function is_alloc(this)
        implicit none
        class(variable) :: this
        logical(kind=4) :: is_alloc
        
        is_alloc = allocated(vstack(this%id)%v)
    end function is_alloc

    subroutine backward(this, retain_grad)
        implicit none
        class(variable) :: this
        logical(kind=4), optional :: retain_grad

        integer(kind=8) :: vid, fid
        integer(kind=8), allocatable :: func_ids(:)
        integer(kind=8), allocatable :: func_gs(:)
        integer(kind=8), allocatable :: func_seen(:)

        integer(kind=8) :: id_in_1, id_in_2
        integer(kind=8) :: id_out_1, id_out_2
        integer(kind=8) :: max_g_idx

        type(jagged_matrix), allocatable :: g_outs(:)

        if (allocated(this%param_ids)) deallocate(this%param_ids)
        if (allocated(this%non_param_ids)) deallocate(this%non_param_ids)
        allocate(this%param_ids(0))
        allocate(this%non_param_ids(0))
        if (vstack(this%id)%is_parameter) then
            this%param_ids = [this%param_ids, this%id]
        else
            this%non_param_ids = [this%non_param_ids, this%id]
        end if

        vid = this%id
        allocate(func_ids(0))
        allocate(func_gs(0))
        allocate(func_seen(0))

        if (.not. allocated(vstack(vid)%g)) then
            if (allocated(vstack(vid)%v)) then
                allocate(vstack(vid)%g, source=vstack(vid)%v)
                vstack(vid)%g(:,:) = 1d0
            elseif (allocated(vstack(vid)%csr_v)) then
                vstack(vid)%csr_g = vstack(vid)%csr_v
                vstack(vid)%csr_g%vals = 1d0
            end if
        end if
        func_ids = [func_ids, vstack(vid)%fid]
        func_gs = [func_gs, fstack(vstack(vid)%fid)%generation]

        do while (size(func_ids)>0)
            max_g_idx = maxloc(func_gs, dim=1)
            fid = func_ids(max_g_idx)
            select type (creator => fstack(fid)%func)
                class is (base_function)
                    ! print*, fid, creator%fname
                    ! Get Variable IDs ------------------------------------------------------------
                    id_in_1  = creator%id_in_1
                    id_in_2  = creator%id_in_2
                    id_out_1 = creator%id_out_1
                    id_out_2 = creator%id_out_2

                    ! Compute Grads ---------------------------------------------------------------
                    if (creator%fname == "dense2csr") then
                        g_outs = creator%backward_1in_2out_csr(vstack(id_out_1)%csr_g)
                    else
                        g_outs = creator%backward(vstack(id_out_1)%g)
                    end if

                    ! Get Parameter IDs -----------------------------------------------------------
                    if (id_in_1>0) then
                        if (vstack(id_in_1)%is_parameter) then
                            this%param_ids = [this%param_ids, id_in_1]
                        else
                            this%non_param_ids = [this%non_param_ids, id_in_1]
                        end if
                    end if
                    if (id_in_2>0) then
                        if (vstack(id_in_2)%is_parameter) then
                            this%param_ids = [this%param_ids, id_in_2]
                        else
                            this%non_param_ids = [this%non_param_ids, id_in_2]
                        end if
                    end if

                    ! Add Grads -------------------------------------------------------------------
                    if (id_in_1>0) then
                        if (allocated(vstack(id_in_1)%csr_v)) then
                            if (allocated(vstack(id_in_1)%csr_g)) then
                                vstack(id_in_1)%csr_g%vals(:) = vstack(id_in_1)%csr_g%vals(:) + g_outs(1)%csr_g%vals(:)
                            else
                                vstack(id_in_1)%csr_g = g_outs(1)%csr_g
                            end if
                        else
                            if (allocated(vstack(id_in_1)%g)) then
                                vstack(id_in_1)%g = vstack(id_in_1)%g + g_outs(1)%g
                            else
                                vstack(id_in_1)%g = g_outs(1)%g
                            end if
                        end if
                    end if
                    if (id_in_2>0) then
                        if (allocated(vstack(id_in_2)%g)) then
                            vstack(id_in_2)%g = vstack(id_in_2)%g + g_outs(2)%g
                        else
                            vstack(id_in_2)%g = g_outs(2)%g
                        end if
                    end if

                    ! Append Next Function IDs ----------------------------------------------------
                    if (id_in_1>0) then
                        if (vstack(id_in_1)%fid>0) then
                            if (.not. any(vstack(id_in_1)%fid == func_seen)) then
                                func_ids = [func_ids, vstack(id_in_1)%fid]
                                func_gs = [func_gs, fstack(vstack(id_in_1)%fid)%generation]
                                func_seen = [func_seen, vstack(id_in_1)%fid]
                            end if
                        end if
                    end if

                    if (id_in_2>0) then
                        if (vstack(id_in_2)%fid>0) then
                            if (.not. any(vstack(id_in_2)%fid == func_seen)) then
                                func_ids = [func_ids, vstack(id_in_2)%fid]
                                func_gs = [func_gs, fstack(vstack(id_in_2)%fid)%generation]
                                func_seen = [func_seen, vstack(id_in_2)%fid]
                            end if
                        end if
                    end if

                    ! Retain grad or not ----------------------------------------------------------
                    if (present(retain_grad)) then
                        if (retain_grad) then
                            if (id_out_1>0) then
                                deallocate(vstack(id_out_1)%g)
                            end if
                            if (id_out_2>0) then
                                deallocate(vstack(id_out_2)%g)
                            end if
                        end if
                    end if
                end select 
            call pop_by_index(func_ids, func_gs, max_g_idx)
        end do
    end subroutine backward


    function new_variable_r8_scl(x, is_parameter) result(var)
        implicit none
        type(variable) :: var
        real(kind=8), intent(in) :: x
        logical(kind=4), optional :: is_parameter
        type(velement) :: elm

        elm%v = reshape([x], [1,1])
        if (present(is_parameter)) elm%is_parameter = is_parameter
        vstack = [vstack, elm]
        var%id = size(vstack)
        call set_mode_to_variable(var)
    end function new_variable_r8_scl


    function new_variable_r8_vec(x, is_parameter) result(var)
        implicit none
        type(variable) :: var
        real(kind=8), intent(in) :: x(:)
        logical(kind=4), optional :: is_parameter
        type(velement) :: elm

        elm%v = reshape(x, [1, size(x)])
        if (present(is_parameter)) elm%is_parameter = is_parameter
        vstack = [vstack, elm]
        var%id = size(vstack)
        call set_mode_to_variable(var)
    end function new_variable_r8_vec


    function new_variable_r8_mat(x, is_parameter) result(var)
        implicit none
        type(variable) :: var
        real(kind=8), intent(in) :: x(:,:)
        logical(kind=4), optional :: is_parameter
        type(velement) :: elm
        integer(kind=8) :: id

        elm%v = x
        if (present(is_parameter)) elm%is_parameter = is_parameter
        id = unused_stack_id()
        if (id==-1_8) then
            vstack = [vstack, elm]
            var%id = size(vstack)
        else
            vstack(id) = elm
            var%id = id
        end if
        call set_mode_to_variable(var)
    end function new_variable_r8_mat


    function new_variable_r8_csr(x, is_parameter) result(var)
        implicit none
        type(variable) :: var
        type(csr_matrix), intent(in) :: x
        logical(kind=4), optional :: is_parameter
        type(velement) :: elm
        integer(kind=8) :: id

        elm%csr_v = x
        if (present(is_parameter)) elm%is_parameter = is_parameter
        id = unused_stack_id()
        if (id==-1_8) then
            vstack = [vstack, elm]
            var%id = size(vstack)
        else
            vstack(id) = elm
            var%id = id
        end if
        call set_mode_to_variable(var)
    end function new_variable_r8_csr

    function act_1in_1out(this, var_in) result(var_out)
        implicit none
        class(base_function)      :: this
        type(variable)            :: var_in
        type(variable)            :: var_out

        integer(kind=8) :: fid, vid

        call this%append_new_element_to_stack(fid, vid)
        var_out%id = vid
        
        fstack(fid)%generation = maxval([var_in%generation])
        var_out%generation = fstack(fid)%generation+1

        ! print*, "ids: ", var_in_1%id, var_in_2%id, " -->  ", vid
        this%id_in_1 = var_in%id
        this%id_out_1 = vid
        this%shape_in_1 = var_in%get_shape()
        allocate(fstack(fid)%func, source=this)
        
        if (this%fname == "dense2csr") then
            vstack(vid)%csr_v = this%forward_1in_1out_csr(vstack(var_in%id)%v)
        elseif (this%fname == "csr2dense") then
            vstack(vid)%v = this%forward_1in_csr_1out(vstack(var_in%id)%csr_v)
        else
            vstack(vid)%v = this%forward(vstack(var_in%id)%v)
        end if
        vstack(vid)%fid = fid
        this%shape_out_1 = var_out%get_shape()

        call set_mode_to_variable(var_out)
    end function act_1in_1out

    function act_2in_1out(this, var_in_1, var_in_2) result(var_out)
        implicit none
        class(base_function)      :: this
        type(variable)            :: var_in_1, var_in_2
        type(variable)            :: var_out

        integer(kind=8) :: fid, vid

        call this%append_new_element_to_stack(fid, vid)
        var_out%id = vid
        fstack(fid)%generation = maxval([var_in_1%generation, var_in_2%generation])
        var_out%generation = fstack(fid)%generation+1

        ! print*, "ids: ", var_in_1%id, var_in_2%id, " -->  ", vid
        this%id_in_1 = var_in_1%id
        this%id_in_2 = var_in_2%id        
        this%id_out_1 = vid
        this%shape_in_1 = var_in_1%get_shape()
        this%shape_in_2 = var_in_2%get_shape()
        allocate(fstack(fid)%func, source=this)
        
        if (this%fname == "sparse_matrix_multiplication") then
            vstack(vid)%v = this%forward_2in_1out_csr(vstack(var_in_1%id)%csr_v, vstack(var_in_2%id)%v)
        else
            vstack(vid)%v = this%forward(vstack(var_in_1%id)%v, vstack(var_in_2%id)%v)
        end if
        vstack(vid)%fid = fid
        this%shape_out_1 = var_out%get_shape()
        call set_mode_to_variable(var_out)
    end function act_2in_1out

    function forward_2in_1out(this, v_in_1, v_in_2) result(v_out)
        implicit none
        class(base_function) :: this
        real(kind=8), intent(in) :: v_in_1(:,:), v_in_2(:,:)
        real(kind=8), allocatable :: v_out(:,:)
        stop "NotImplemented Error, " // trim(this%fname) // "." 
    end function forward_2in_1out

    function forward_1in_1out(this, v_in) result(v_out)
        implicit none
        class(base_function) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        real(kind=8), allocatable :: v_out(:,:)
        stop "NotImplemented Error, " // trim(this%fname) // "."
    end function forward_1in_1out
    
    function forward_1in_1out_csr(this, v_in) result(v_out)
        implicit none
        class(base_function) :: this
        real(kind=8), intent(in) :: v_in(:,:)
        type(csr_matrix) :: v_out
        stop "NotImplemented Error, " // trim(this%fname) // "."
    end function forward_1in_1out_csr
    
    function forward_1in_csr_1out(this, v_in) result(v_out)
        implicit none
        class(base_function) :: this
        type(csr_matrix) :: v_in
        real(kind=8), allocatable :: v_out(:,:)
        stop "NotImplemented Error, " // trim(this%fname) // "."
    end function forward_1in_csr_1out
    
    function forward_2in_1out_csr(this, v_in_1, v_in_2) result(v_out)
        implicit none
        class(base_function) :: this
        type(csr_matrix), intent(in) :: v_in_1
        real(kind=8), intent(in) :: v_in_2(:,:)
        real(kind=8), allocatable :: v_out(:,:)
        stop "NotImplemented Error, " // trim(this%fname) // "."
    end function forward_2in_1out_csr

    function backward_1in_2out(this, g_in) result(g_outs)
        implicit none
        class(base_function) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        stop "NotImplemented Error, " // trim(this%fname) // "."
    end function backward_1in_2out

    function backward_1in_2out_csr(this, g_in) result(g_outs)
        implicit none
        class(base_function) :: this
        type(csr_matrix), intent(in) :: g_in
        type(jagged_matrix) :: g_outs(2)
        stop "NotImplemented Error, " // trim(this%fname) // "."
    end function backward_1in_2out_csr
    

    subroutine update(this, var)
        implicit none
        class(optimizer) :: this
        type(variable), intent(in) :: var

        integer(kind=8) :: n_params, p, id

        n_params = size(var%param_ids)

        do p=1, n_params, 1
            id = var%param_ids(p)
            call this%update_one_param(id)
        end do
    end subroutine update

    subroutine update_one_param(this, id)
        implicit none
        class(optimizer) :: this
        integer(kind=8) :: id
        stop "NotImplemented Error, " // __FILE__ // "."
    end subroutine update_one_param

    subroutine clear_grads(this, var)
        implicit none
        class(optimizer) :: this
        type(variable), intent(in) :: var

        integer(kind=8) :: n_params, p, id

        n_params = size(var%param_ids)

        do p=1, n_params, 1
            id = var%param_ids(p)
            deallocate(vstack(id)%g)
        end do
    end subroutine clear_grads

end module mod_variable