module mod_var
    ! use mod_activation_functions2
    implicit none

    type variable
        real(kind=8), allocatable :: v(:,:)
        real(kind=8), allocatable :: g(:,:)
        type(activation_function), pointer :: creator_ptr
        class(*), pointer :: creator_tmp
    contains
        procedure :: backward => backward_variable
    end type variable

    interface variable
        module procedure :: new_variable_s
        module procedure :: new_variable_m
    end interface variable

    type activation_function
        character(len=256) :: act_name
        type(variable), pointer :: input_ptr
    contains
        ! Forward
        procedure :: square
        procedure :: exponential
        ! Backward
        procedure :: backward => backward_activation_funcion
    end type activation_function

contains

    function new_variable_s(sclr)
        implicit none
        type(variable) :: new_variable_s
        real(kind=8), intent(in) :: sclr
        new_variable_s%v = reshape( (/sclr/), shape = [1,1] )
        nullify(new_variable_s%creator_ptr)
    end function new_variable_s
    
    function new_variable_m(mtrx)
        implicit none
        type(variable) :: new_variable_m
        real(kind=8), intent(in) :: mtrx(:,:)
        new_variable_m%v = mtrx(:,:)
        nullify(new_variable_m%creator_ptr)
    end function new_variable_m

    recursive subroutine backward_variable(this)
        implicit none
        class(variable) :: this
        type(variable) :: var
        if (associated(this%creator_ptr)) then
            this%creator_ptr%input_ptr%g = this%creator_ptr%backward(this%g)
            call this%creator_ptr%input_ptr%backward()
        end if
    end subroutine backward_variable

    function square(this, input_var) result(output_var)
        implicit none
        class(activation_function), target :: this
        type(variable), target :: input_var
        type(variable) :: output_var
        this%act_name = "square"
        output_var%creator_ptr => this
        output_var%creator_ptr%input_ptr => input_var
        output_var%v = input_var%v**2d0
    end function square
    
    function exponential(this, input_var) result(output_var)
        implicit none
        class(activation_function), target :: this
        type(variable), target :: input_var
        type(variable) :: output_var
        this%act_name = "exponential"
        output_var%creator_ptr => this
        output_var%creator_ptr%input_ptr => input_var
        output_var%v = exp(input_var%v)
    end function exponential

    function backward_activation_funcion(this, grad_in) result(grad_out)
        implicit none
        class(activation_function) :: this
        real(kind=8), intent(in) :: grad_in(:,:)
        real(kind=8), allocatable :: grad_out(:,:)
        if (this%act_name == "square") then
            grad_out = 2d0 * this%input_ptr%v(:,:) * grad_in(:,:)
        elseif (this%act_name == "exponential") then
            grad_out = exp(this%input_ptr%v(:,:)) * grad_in(:,:)
        end if
    end function backward_activation_funcion

end module mod_var