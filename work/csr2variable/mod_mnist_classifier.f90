module mod_mnist_classifier
    use mod_variable
    use mod_intrinsics
    use mod_functions
    use mod_layers
    implicit none

    type, extends(base_model) :: mnist_classifier
        type(linear), allocatable :: linears(:)
    contains
        procedure :: forward => forward_mnist_classifier
    end type mnist_classifier

    interface mnist_classifier
        module procedure :: new_mnist_classifier
    end interface mnist_classifier
    
contains

    function new_mnist_classifier(hidden_dims) result(model)
        implicit none
        type(mnist_classifier) :: model
        integer(kind=8) :: hidden_dims(:)

        allocate(model%linears(3))
        model%linears(1) = linear(out_size=512_8, no_bias=.false.)
        model%linears(2) = linear(out_size=256_8, no_bias=.false.)
        model%linears(3) = linear(out_size=10_8,  no_bias=.false.)
    end function new_mnist_classifier


    function forward_mnist_classifier(this, var_in) result(var_out)
        implicit none
        class(mnist_classifier) :: this
        type(variable) :: var_in
        type(variable) :: var_out

        var_out = this%linears(1)%act(var_in)
        var_out = relu_(var_out)
        var_out = this%linears(2)%act(var_out)
        var_out = relu_(var_out)
        var_out = this%linears(3)%act(var_out)
    end function forward_mnist_classifier
    
end module mod_mnist_classifier