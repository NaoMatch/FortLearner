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
        type(splinear) :: linear3
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

        model%linear1 = linear(out_size=512_8, no_bias=.false.)
        model%linear2 = splinear(out_size=256_8, no_bias=.false.)
        model%linear3 = splinear(out_size=10_8,  no_bias=.false.)
    end function new_mnist_classifier_with_sp


    function forward_mnist_classifier_with_sp(this, var_in) result(var_out)
        implicit none
        class(mnist_classifier_with_sp) :: this
        type(variable) :: var_in
        type(variable) :: var_out
        integer(kind=8)        :: date_value1(8), date_value2(8)
        var_out = this%linear1%act(var_in)
        var_out = relu_(var_out)
        
        var_out = dense2csr_(var_out, top_k=64_8, how="absolute")
        var_out = this%linear2%act(var_out)
        var_out = relu_(var_out)
        
        var_out = dense2csr_(var_out, top_k=64_8, how="absolute")
        var_out = this%linear3%act(var_out)
    end function forward_mnist_classifier_with_sp
    
end module mod_mnist_classifier_with_sp