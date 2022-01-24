program main
    use mod_multi_layer_perceptron
    implicit none

    type(model_builder) :: model
    type(variable), pointer :: var0_ptr, var1_ptr, var2_ptr, var3_ptr
    type(variable), pointer :: var4_ptr, var5_ptr, var6_ptr, var7_ptr
    type(variable), target  :: var0, var1, var2, var3
    type(variable), target  :: var4, var5, var6, var7

    type(layer), pointer :: lyr0_ptr, lyr1_ptr, lyr2_ptr, lyr3_ptr
    type(layer), pointer :: lyr4_ptr, lyr5_ptr, lyr6_ptr, lyr7_ptr
    type(layer), target  :: lyr0, lyr1, lyr2, lyr3
    type(layer), target  :: lyr4, lyr5, lyr6, lyr7

    var0 = variable(); var0_ptr => var0
    var1 = variable(); var1_ptr => var1
    var2 = variable(); var2_ptr => var2
    var3 = variable(); var3_ptr => var3
    var4 = variable(); var4_ptr => var4
    var5 = variable(); var5_ptr => var5
    var6 = variable(); var6_ptr => var6

    lyr0 = layer(); lyr0_ptr => lyr0
    lyr1 = layer(); lyr1_ptr => lyr1
    lyr2 = layer(); lyr2_ptr => lyr2
    lyr3 = layer(); lyr3_ptr => lyr3
    lyr4 = layer(); lyr4_ptr => lyr4
    lyr5 = layer(); lyr5_ptr => lyr5
    lyr6 = layer(); lyr6_ptr => lyr6

    model = model_builder(model_name="test")

    var1 = model%dense_func(var0, input_dim=784, output_dim=392)


    print*, "================================================================="
    print*, "================================================================="
    print*, "================================================================="
    print*, "================================================================="
    print*, "================================================================="
    ! call var1%info()
    ! call var1%creator%info()
    call model%input_ptrs(1)%ptr%info()
    call model%output_ptrs(1)%ptr%info()
    call model%layer_ptrs(1)%ptr%info()


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
