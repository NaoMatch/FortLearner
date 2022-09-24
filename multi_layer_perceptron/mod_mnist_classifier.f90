module mod_mnist_classifier
    use mod_random
    use mod_const
    use mod_wengert_list
    include "./inc_use_activation_functions.f90"
    implicit none

    type, extends(neural_network) :: mnist_classifier
        type(dense) :: dense1 = dense(784_8, 100_8, bias=t_)
        type(dense) :: dense2 = dense(100_8,  10_8, bias=t_)
    contains
        procedure :: forward => forward_mnist_classifier
    end type mnist_classifier

    interface mnist_classifier
        module procedure new_mnist_classifier
    end interface mnist_classifier
    
contains

    function new_mnist_classifier(opt)
        implicit none
        type(mnist_classifier) :: new_mnist_classifier
        type(optimizer), target :: opt
        new_mnist_classifier%opt_ptr => opt
        call new_mnist_classifier%dense1%init()
        call new_mnist_classifier%dense2%init()
    end function new_mnist_classifier

    function forward_mnist_classifier(this, input_var) result(output_var)
        implicit none
        class(mnist_classifier) :: this
        type(variable), target :: input_var
        type(variable) :: output_var, h

        ! Preprocess
        call this%preprocess(input_var)

        h = this%dense1%act(input_var)
        h = relu(h)
        h = this%dense2%act(h)
        output_var = sigmoid(h)

        ! Postprocess
        call this%postprocess()
    end function forward_mnist_classifier

end module mod_mnist_classifier