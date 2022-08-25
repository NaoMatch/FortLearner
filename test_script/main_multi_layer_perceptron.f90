program main_multi_layer_perceptron
    use mod_timer
    use mod_var
    use mod_optimizer
    use mod_wengert_list
    use mod_backwards
    use mod_my_mlp
    implicit none

    type(variable_) :: input_var, output_vars(1)
    type(variable_) :: input_vars(2)
    type(variable_) :: loss_var(1)
    type(my_mlp) :: nn_model, nn_model2
    type(optimizer_) :: opt
    integer(kind=8)        :: date_value1(8), date_value2(8)

    opt = optimizer_(learning_rate=.1d0, alpha=0d0)

    input_var = variable_(2.0d0)

    nn_model = my_mlp(opt=opt)
    output_vars = nn_model%forward(input_var)
    
    call clear_grad(output_vars(1))
    call stacks(1)%print()
    call backward(output_vars)
    print*, "OUTPUT: ", output_vars(1)%v
    print*, stacks(1)%vars(1)%g





end program main_multi_layer_perceptron