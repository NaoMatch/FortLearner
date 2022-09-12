program main_multi_layer_perceptron
    use mod_stats
    use mod_timer
    use mod_var
    use mod_optimizer
    use mod_wengert_list
    use mod_backwards
    use mod_my_mlp
    use mod_loss_function
    use mod_random
    use mod_const
    implicit none

    type(variable_) :: input_var, output_vars(1), output_var, weight
    type(variable_) :: input_vars(5), y_true, y_pred
    type(variable_) :: loss_var(1), loss
    type(my_mlp) :: nn_model, nn_model2
    type(optimizer_) :: opt
    integer(kind=8)        :: date_value1(8), date_value2(8), i, j
    integer(kind=8)        :: var_id_w1, var_id_b1
    integer(kind=8)        :: var_id_w2, var_id_b2
    real(kind=8)        :: x(100,1), w1(1,10), w2(10,1), y(100,1), delta(100,1), b1(10), b2(1)
    real(kind=8)        :: w1_up(1,10), w2_up(10,1), b1_up(10), b2_up
    real(kind=8)        :: xx(1000,1), dd(1000,1)

    opt = optimizer_(learning_rate=.1d0, alpha=0d0)

    ! X
    call random_number(x)
    do i=1, 100, 1
        x(i,1) = 0.01*(i-1)
    end do    

    ! y
    y = sin(x*2d0*pi_)
    y_true = variable_(y)

    nn_model = my_mlp(opt=opt)

    do i=1, 10000, 1

        input_var = variable_(x)

        y_pred = nn_model%forward(input_var)
        loss = mean_squared_error(y_true, y_pred)
        print*, "LOOP INDEX", i, "loss", loss%var%s
        ! call stacks(1)%print()

        call clear_grad(loss)
        call backward(loss)
    end do


end program main_multi_layer_perceptron