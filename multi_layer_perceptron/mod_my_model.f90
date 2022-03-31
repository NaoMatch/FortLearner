module mod_my_model
    use mod_multi_layer_perceptron
    implicit none

    type(layer_stacker) :: my_model_stacker

    type  my_model
        ! model
        type(model_builder) :: model

        ! Layer
        type(layer) :: lyr_dense00
        type(layer) :: lyr_relu00
        type(layer) :: lyr_dense_relu(2)

    contains
        procedure :: forward
    end type my_model

    interface my_model
        module procedure :: new_my_model
    end interface my_model
    
contains

    function new_my_model()
        type(my_model) :: new_my_model
    end function new_my_model


    function forward(this, x)
        implicit none
        class(my_model) :: this
        real(kind=8)    :: x(:,:)
        type(variable)  :: forward

        type(variable) :: var00
        type(variable) :: var01
        type(variable) :: var02
        type(variable) :: var03

        this%model = new_model_builder(model_name="test")

        var00 = variable(x)
        call var00%info()

        var01 = this%model%dense(var00, 784, 392, this%lyr_dense00)
        call var01%info()

        var02 = this%model%relu(var01, 392, 392, this%lyr_relu00)
        call var02%info()

        var03 = this%model%dense_relu(var02, 392, 392, this%lyr_dense_relu)
        call var03%creator%input_var0%info()
        call var03%creator%info()
        call var03%info()

    end function 

end module mod_my_model
