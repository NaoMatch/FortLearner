program main_multi_layer_perceptron
    use mod_stats
    use mod_timer
    use mod_optimizer
    use mod_wengert_list
    use mod_backwards
    use mod_my_mlp
    use mod_loss_function
    use mod_random
    use mod_const
    implicit none

    type(variable) :: input_var, output_vars(1), output_var, weight
    type(variable) :: input_vars(5), y_true, y_pred
    type(variable) :: loss_var(1), loss
    type(my_mlp) :: nn_model, nn_model2
    type(optimizer) :: opt
    integer(kind=8)        :: date_value1(8), date_value2(8), i, j
    integer(kind=8)        :: var_id_w1, var_id_b1
    integer(kind=8)        :: var_id_w2, var_id_b2
    real(kind=8)        :: x(100,1), y(100,1)

    opt = optimizer()
    call opt%sgd(learning_rate=0.1d0, momentum=0.9d0)

    ! X
    call random_number(x)
    do i=1, 100, 1
        x(i,1) = 0.01*(i-1)
    end do    

    ! y
    y = sin(x*2d0*pi_)
    y_true = variable(y)

    nn_model = my_mlp(opt=opt)

    input_var = variable(x)
    do i=1, 10000, 1

        y_pred = nn_model%forward(input_var)
        loss = mean_squared_error(y_true, y_pred)
        if (mod(i,1000)==0) print*, "LOOP INDEX", i, "loss", loss%var%s

        call clear_grad(loss)
        call backward(loss)

        call opt%update()
    end do


end program main_multi_layer_perceptron