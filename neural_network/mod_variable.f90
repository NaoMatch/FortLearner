module mod_variable
    implicit none

    type jagged_matrix
        real(kind=8), allocatable :: g(:,:)
    end type jagged_matrix

    type velement
        real(kind=8), allocatable :: v(:,:)
        real(kind=8), allocatable :: g(:,:)
        real(kind=8), allocatable :: c(:,:)
        integer(kind=8) :: fid=-1
        integer(kind=8) :: lid=-1
        character(len=:), allocatable :: name
        logical(kind=4) :: is_parameter = .false.
    end type velement

    type variable
        integer(kind=8) :: id = -1
        integer(kind=8) :: generation = 0
        integer(kind=8), allocatable :: param_ids(:)
    contains
        procedure :: backward
        procedure :: set_name
        procedure :: get_grad
        procedure :: get_data
        procedure :: clear_grad
        procedure :: is_alloc
    end type variable 

    interface variable
        module procedure new_variable_r8_scl
        module procedure new_variable_r8_vec
        module procedure new_variable_r8_mat
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
    contains
        procedure :: act_1in_1out, act_2in_1out
        generic   :: act => act_1in_1out, act_2in_1out

        procedure :: forward_1in_1out, forward_2in_1out
        generic   :: forward => forward_1in_1out, forward_2in_1out

        procedure :: backward_1in_2out
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
    end type optimizer


contains

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

        if (.not. allocated(vstack)) then
            allocate(vstack(0))
        else
            ! print*, "zero clear"
            do i=1, size(vstack), 1
                if (vstack(i)%fid<0) then
                    last_hold_idx = i
                    if (allocated(vstack(i)%g)) vstack(i)%g = 0d0
                end if
            end do
            
            ! print*, "delete useless stacks"
            vstack = vstack(1:last_hold_idx)
            
            ! print*, "deallocate"
            do i=1, size(vstack), 1
                if (vstack(i)%fid>0) then
                    if (allocated(vstack(i)%v)) deallocate(vstack(i)%v)
                    if (allocated(vstack(i)%g)) deallocate(vstack(i)%g)
                end if
            end do
            ! print*, "done"
        end if

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

        fstack = [fstack, func]
        vstack = [vstack, elm]
        fid = size(fstack)
        vid = size(vstack)
    end subroutine append_new_element_to_stack


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

        allocate(grad, source=vstack(this%id)%g)
    end function get_grad

    function get_data(this) result(grad)
        implicit none
        class(variable) :: this
        real(kind=8), allocatable :: grad(:,:)

        allocate(grad, source=vstack(this%id)%v)
    end function get_data

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
        allocate(this%param_ids(0))

        vid = this%id
        allocate(func_ids(0))
        allocate(func_gs(0))
        allocate(func_seen(0))

        if (.not. allocated(vstack(vid)%g)) then
            allocate(vstack(vid)%g, source=vstack(vid)%v)
            vstack(vid)%g(:,:) = 1d0
        end if
        func_ids = [func_ids, vstack(vid)%fid]
        func_gs = [func_gs, fstack(vstack(vid)%fid)%generation]

        do while (size(func_ids)>0)
            max_g_idx = maxloc(func_gs, dim=1)
            fid = func_ids(max_g_idx)
            ! print*, fid, func_gs
            select type (creator => fstack(fid)%func)
                class is (base_function)
                    ! Get Variable IDs ------------------------------------------------------------
                    id_in_1  = creator%id_in_1
                    id_in_2  = creator%id_in_2
                    id_out_1 = creator%id_out_1
                    id_out_2 = creator%id_out_2

                    ! Compute Grads ---------------------------------------------------------------
                    g_outs = creator%backward(vstack(id_out_1)%g)

                    ! Get Parameter IDs -----------------------------------------------------------
                    if (id_in_1>0) then
                        if (vstack(id_in_1)%is_parameter) this%param_ids = [this%param_ids, id_in_1]
                    end if
                    if (id_in_2>0) then
                        if (vstack(id_in_2)%is_parameter) this%param_ids = [this%param_ids, id_in_2]
                    end if

                    ! Add Grads -------------------------------------------------------------------
                    if (id_in_1>0) then
                        if (allocated(vstack(id_in_1)%g)) then
                            vstack(id_in_1)%g = vstack(id_in_1)%g + g_outs(1)%g
                        else
                            vstack(id_in_1)%g = g_outs(1)%g
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
    end function new_variable_r8_vec


    function new_variable_r8_mat(x, is_parameter) result(var)
        implicit none
        type(variable) :: var
        real(kind=8), intent(in) :: x(:,:)
        logical(kind=4), optional :: is_parameter
        type(velement) :: elm

        elm%v = x
        if (present(is_parameter)) elm%is_parameter = is_parameter
        vstack = [vstack, elm]
        var%id = size(vstack)
    end function new_variable_r8_mat

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
        allocate(fstack(fid)%func, source=this)
        
        vstack(vid)%v = this%forward(vstack(var_in%id)%v)
        vstack(vid)%fid = fid
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
        allocate(fstack(fid)%func, source=this)
        
        vstack(vid)%v = this%forward(vstack(var_in_1%id)%v, vstack(var_in_2%id)%v)
        vstack(vid)%fid = fid
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

    function backward_1in_2out(this, g_in) result(g_outs)
        implicit none
        class(base_function) :: this
        real(kind=8), intent(in) :: g_in(:,:)
        type(jagged_matrix) :: g_outs(2)
        stop "NotImplemented Error, " // trim(this%fname) // "."
    end function backward_1in_2out
    

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

end module mod_variable