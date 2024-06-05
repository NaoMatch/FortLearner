module mod_mnist_classifier
    use mod_timer
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
        model%linears(1) = linear(out_size=800_8, no_bias=.false.)
        model%linears(2) = linear(out_size=800_8, no_bias=.false.)
        model%linears(3) = linear(out_size=10_8,  no_bias=.false.)
    end function new_mnist_classifier


    function forward_mnist_classifier(this, var_in) result(var_out)
        implicit none
        class(mnist_classifier) :: this
        type(variable) :: var_in
        type(variable) :: var_out
        integer(kind=8)        :: date_value1(8), date_value2(8)
        integer(kind=8) :: time1, time2, time3

        time1=0
        time2=0
        time3=0

        call date_and_time(values=date_value1)
        var_out = this%linears(1)%act(var_in)
        var_out = relu_(var_out)
        call date_and_time(values=date_value2)
        time1 = time1 + abs(date_value1(8) - date_value2(8))
        print*, "    LINEAR1: ", time1
        

        call date_and_time(values=date_value1)
        var_out = this%linears(2)%act(var_out)
        var_out = relu_(var_out)
        call date_and_time(values=date_value2)
        time2 = time2 + abs(date_value1(8) - date_value2(8))
        print*, "    LINEAR2: ", time2

        call date_and_time(values=date_value1)
        var_out = this%linears(3)%act(var_out)
        call date_and_time(values=date_value2)
        time3 = time3 + abs(date_value1(8) - date_value2(8))
        print*, "    LINEAR3: ", time3
    end function forward_mnist_classifier
    
end module mod_mnist_classifier