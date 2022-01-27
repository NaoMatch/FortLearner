program main
    use mod_multi_layer_perceptron
    implicit none

    type(model_builder) :: model
    type(variable_ptr), ALLOCATABLE :: input_ptrs(:)
    type(variable_ptr), ALLOCATABLE :: output_ptrs(:)
    type(layer_ptr), ALLOCATABLE :: layer_ptrs(:)

    type(variable), target  :: var0, var1, var2, var3
    type(variable), target  :: var4, var5, var6, var7

    type(layer), target :: layer_dense0

    var0 = variable()
    var1 = variable()
    var2 = variable()


    layer_dense0 = layer(layer_type="dense", input_dim=784, output_dim=392)


    model = new_model_builder(model_name="test")
    var1 = model%add_layer(input_var0=var0, layer_type=layer_dense0)


    print*, "================================================================="
    print*, "================================================================="
    print*, "================================================================="
    print*, "================================================================="
    print*, "================================================================="
    ! call model%input_array(1)%info()
    ! call model%output_array(1)%info()
    call var1%info()
    call var1%creator%info()
    call var1%creator%input_var0%info()
    ! call model%output_array(1)%creator%info()
    ! call model%layer_array(1)%info()
    ! call var1%info()


    ! Input -> Dense
    ! call model%dense(var0_ptr, var1_ptr, lyr0_ptr, input_dim=784, output_dim=392)

    ! ! Dense -> Dense, Dense
    ! call model%dense(var1_ptr, var2_ptr, lyr1_ptr, input_dim=392, output_dim=196)
    ! call model%dense(var1_ptr, var3_ptr, lyr2_ptr, input_dim=392, output_dim=196)

    ! ! Dense -> Dense
    ! call model%dense(var3_ptr, var4_ptr, lyr3_ptr, input_dim=196, output_dim=196)

    ! ! Dense, Dense -> Sum
    ! call model%sum(var2_ptr, var4_ptr, var5_ptr, lyr4_ptr, input_dim=196, output_dim=196)
    ! call var5_ptr%info()

    ! call var0_ptr%info()
    ! call var1_ptr%info()
    ! call var2_ptr%info()
    ! call var3_ptr%info()
    ! call var4_ptr%info()

    ! print*, "==================================================="
    ! print*, "==================================================="
    ! print*, "==================================================="
    ! print*, "==================================================="
    ! call var4_ptr%creator%info()


    ! call model%dense(var2_ptr, var_ptr, input_dim=196, output_dim=98)
    ! call model%dense(var3_ptr, var4_ptr, input_dim=98,  output_dim=49)
    ! call model%dense(var4_ptr, var5_ptr, input_dim=49,  output_dim=10)

    ! call var5_ptr%info()


    ! var2 = model%add_layer(var, input_dim=784, output_dim=392, layer_type="dense")
    ! call var2%info()
    
    ! vars = (/var1, var2/)
    ! var3 = model%add_layer((/var1, var2/), input_dim=392, output_dim=392, layer_type="sum")

    ! model = model_builder(model_name="test")

    ! dense_lyr_ptr => dense_lyr
    ! call model%add_layer(dense_lyr_ptr, input_var0=var0_ptr, output_var=var1_ptr, input_dim=784, output_dim=392)
    ! call var0_ptr%info()
    ! call var1_ptr%info()








end program main
