module mod_my_mlp
    use mod_var
    use mod_wengert_list
    include "./inc_use_activation_functions.f90"
    implicit none

    type, extends(neural_network) :: my_mlp
    contains
        procedure :: forward => forward_my_mlp
    end type my_mlp

    interface my_mlp
        module procedure new_my_mlp
    end interface my_mlp
    
contains

    function new_my_mlp(opt)
        implicit none
        type(my_mlp) :: new_my_mlp
        type(optimizer_) :: opt
        new_my_mlp%opt = opt
    end function new_my_mlp

    function forward_my_mlp(this, input_vars) result(output_var)
        implicit none
        class(my_mlp) :: this
        type(variable_), target :: input_vars(5)
        type(variable_) :: output_var, x, w1, b1, h, w2, b2, z

        ! Preprocess
        call this%init(input_vars)
        
        ! 
        x = input_vars(1)
        w1 = input_vars(2)
        b1 = input_vars(3)
        w2 = input_vars(4)
        b2 = input_vars(5)

        ! Simple
        h = matmul(x, w1) + b1
        ! h = relu(h)
        h = sigmoid(h)
        output_var = matmul(h, w2) + b2
    end function forward_my_mlp

    ! function forward_my_mlp(this, input_vars) result(output_var)
    !     implicit none
    !     class(my_mlp) :: this
    !     type(variable_), target :: input_vars(5)
    !     type(variable_) :: output_var, x, w1, b1, h, w2, b2, z

    !     ! Preprocess
    !     call this%init(input_vars)
        
    !     ! 
    !     x = input_vars(1)
    !     w1 = input_vars(2)
    !     b1 = input_vars(3)
    !     w2 = input_vars(4)
    !     b2 = input_vars(5)
        
    !     ! Simple
    !     h = matmul(x, w1)
    !     output_var = h

    !     ! Complex
    !     ! h = matmul(x, w1) + b1
    !     ! h = sigmoid(h)
    !     ! h = matmul(h, w2) + b2
    !     ! output_var = h
    ! end function forward_my_mlp

    ! function forward_my_mlp(this, input_vars) result(output_var)
    !     implicit none
    !     class(my_mlp) :: this
    !     type(variable_), target :: input_vars(2)
    !     type(variable_) :: output_var, x, y, z
    !     call this%init(input_vars)

    !     x = input_vars(1)
    !     y = input_vars(2)

    !     z = ( & 
    !         1d0 + (x+y+1d0)**2d0 * (19d0 - 14d0*x + 3d0*x**2d0 - 14d0*y + 6d0*x*y + 3d0*y**2d0) &
    !     ) * &
    !     (   &
    !         (30d0 + (2d0*x - 3d-*y)**2d0 * (18d0 - 32d0/x + 12d0*x**2d0 + 48d0*y - 36d0*x*y + 27d0*y**2d0)) &
    !     )

    !     z = sum(z) / 1000000d0
    !     output_var = sigmoid(z)
    ! end function forward_my_mlp


end module mod_my_mlp