module mod_variable
    use mod_const
    implicit none
    
    type layer 
        type(variable), pointer :: var_in
        type(variable), pointer :: var_out
        type(variable), pointer :: vars_in(:)
        type(variable), pointer :: vars_out(:)

        logical(kind=4)         :: is_multi_input  = f_
        logical(kind=4)         :: is_multi_output = f_

        character(len=256)      :: layer_type
        integer(kind=8)         :: dim_in, dim_out
        integer(kind=4), allocatable :: in_shape(:), out_shape(:)
    contains
        procedure, pass :: set_input_var
        procedure, pass :: set_input_vars
        generic         :: set_input => set_input_var, set_input_vars
    end type layer

    type layer_ptr
        type(layer), pointer :: ptr
    contains
        procedure :: set_output_var
    end type layer_ptr

    type variable 
        integer(kind=8), allocatable :: x_shape(:)
        real(kind=8), allocatable :: x(:,:)
        type(layer), pointer :: creator
        type(layer), pointer :: creators
        type(layer_ptr), pointer :: creator_ptr
    contains
        procedure :: set_creator
    end type variable

    interface variable
        module procedure :: new_variable_2d
    end interface ! variable

    type variable_ptr
        type(variable), pointer :: ptr
    end type variable_ptr

contains

    function new_variable_2d(x) result(r)
        implicit none
        type(variable) :: r
        real(kind=8)   :: x(:,:)
        r%x_shape = shape(x)
        r%x = x
    end function new_variable_2d

    subroutine set_creator(this, layers, idx_new_lyr)
        implicit none
        class(variable), target              :: this
        type(layer_ptr), allocatable, target :: layers(:)
        integer(kind=8), intent(in)          :: idx_new_lyr

        allocate(this%creator_ptr)
        this%creator_ptr => layers(idx_new_lyr)
    end subroutine set_creator

    subroutine set_input_vars(this, vars_in)
        implicit none
        class(layer), target   :: this
        type(variable), target :: vars_in(:)
        allocate(this%vars_in(size(vars_in)))
        this%vars_in(1:)  => vars_in(1:)        
    end subroutine set_input_vars

    subroutine set_input_var(this, var_in)
        implicit none
        class(layer), target   :: this
        type(variable), target :: var_in
        allocate(this%var_in)
        this%var_in => var_in       
    end subroutine set_input_var

    subroutine set_output_var(this, var_out)
        implicit none
        class(layer_ptr), target :: this
        type(variable),   target :: var_out
        allocate(this%ptr)
        allocate(this%ptr%var_out)
        this%ptr%var_out => var_out
    end subroutine set_output_var

end module mod_variable
