module mod_mnist_classifier_with_sp
    use mod_timer
    use mod_variable
    use mod_intrinsics
    use mod_functions
    use mod_layers
    implicit none

    type, extends(base_model) :: mnist_classifier_with_sp
        type(linear) :: linear1
        type(splinear) :: linear2
        ! type(splinear) :: linear3
        type(linear) :: linear3
    contains
        procedure :: forward => forward_mnist_classifier_with_sp
    end type mnist_classifier_with_sp

    interface mnist_classifier_with_sp
        module procedure :: new_mnist_classifier_with_sp
    end interface mnist_classifier_with_sp
    
contains

    function new_mnist_classifier_with_sp() result(model)
        implicit none
        type(mnist_classifier_with_sp) :: model

        model%linear1 = linear(out_size=800_8, no_bias=.false.)
        model%linear2 = splinear(out_size=800_8, no_bias=.false.)
        ! model%linear3 = splinear(out_size=10_8,  no_bias=.false.)
        model%linear3 = linear(out_size=10_8,  no_bias=.false.)
    end function new_mnist_classifier_with_sp


    function forward_mnist_classifier_with_sp(this, var_in) result(var_out)
        implicit none
        class(mnist_classifier_with_sp) :: this
        type(variable) :: var_in
        type(variable) :: var_out
        integer(kind=8)        :: date_value1(8), date_value2(8)
        integer(kind=8) :: time1, time2, time3

        time1=0
        time2=0
        time3=0

        ! var_out = this%linear1%act(var_in); print*, "this%linear1 :: ", var_out%get_shape()
        ! var_out = relu_(var_out); print*, "relu_ :: ", var_out%get_shape()
        ! var_out = transpose_(var_out); print*, "transpose_ :: ", var_out%get_shape()
        
        ! var_out = dense2csr_(var_out, top_k=32_8, how="filter"); print*, "dense2csr_ :: ", var_out%get_shape()
        ! var_out = this%linear2%act(var_out); print*, "this%linear2 :: ", var_out%get_shape()
        
        ! var_out = dense2csr_(var_out, top_k=16_8, how="filter"); print*, "dense2csr_ :: ", var_out%get_shape()
        ! var_out = this%linear3%act(var_out); print*, "this%linear3 :: ", var_out%get_shape()
        ! var_out = transpose_(var_out); print*, "transpose_ :: ", var_out%get_shape()

        call date_and_time(values=date_value1)
        var_out = this%linear1%act(var_in)
        var_out = relu_(var_out)!; print*, " @@@ relu_"
        var_out = transpose_(var_out)
        call date_and_time(values=date_value2)
        time1 = time1 + abs(date_value1(8) - date_value2(8))
        print*, "    LINEAR1: ", time1
        
        call date_and_time(values=date_value1)
        var_out = dense2csr_(var_out, top_k=32_8, how="filter")
        var_out = this%linear2%act(var_out)
        var_out = relu_(var_out)!; print*, " @@@ relu_"
        call date_and_time(values=date_value2)
        time2 = time2 + abs(date_value1(8) - date_value2(8))
        print*, "    LINEAR2: ", time2, var_out%get_shape()
        
        call date_and_time(values=date_value1)
        var_out = transpose_(var_out)
        var_out = this%linear3%act(var_out)
        call date_and_time(values=date_value2)
        time3 = time3 + abs(date_value1(8) - date_value2(8))
        print*, "    LINEAR3: ", time3
        ! call date_and_time(values=date_value1)
        ! var_out = dense2csr_(var_out, top_k=8_8, how="absolute")
        ! var_out = this%linear3%act(var_out)
        ! var_out = transpose_(var_out)
        ! call date_and_time(values=date_value2)
        ! time3 = time3 + abs(date_value1(8) - date_value2(8))
        ! print*, "    LINEAR3: ", time3
    end function forward_mnist_classifier_with_sp
    
end module mod_mnist_classifier_with_sp