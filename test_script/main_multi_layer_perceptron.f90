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
    real(kind=8)        :: xx(10000,1)
    ! real(kind=8)        :: x(100,1), w1(1,1), b1, y(100,1)


    opt = optimizer_(learning_rate=.1d0, alpha=0d0)

    ! X
    call rand_normal_2d_r8(x, 100_8, 1_8)
    call rand_normal_2d_r8(delta, 100_8, 1_8)
    delta = delta * 0.01d0

    ! w1 and b1
    call rand_normal_2d_r8(w1, 1_8, 10_8)
    w1 = 0.01 * w1
    b1 = 0d0
    print*, "w1: ", minval(w1), maxval(w1)

    call rand_normal_2d_r8(w2, 10_8, 1_8)
    w2 = 0.01 * w2
    b2 = 0d0
    print*, "w2: ", minval(w2), maxval(w2)

    ! y
    y = sin(x*2d0*pi_) + delta
    ! y(:,1) = sum(x, dim=2)
    y_true = variable_(y)

    nn_model = my_mlp(opt=opt)



    w1_up=0d0
    w2_up=0d0
    b1_up=0d0
    b2_up=0d0
    do i=1, 10000, 1

        input_vars(1) = variable_(x)
        input_vars(2) = variable_(w1)
        input_vars(3) = variable_(b1)
        input_vars(4) = variable_(w2)
        input_vars(5) = variable_(b2)

        y_pred = nn_model%forward(input_vars)
        loss = mean_squared_error(y_true, y_pred)
        print*, "LOOP INDEX", i, "loss", loss%var%s
        ! call stacks(1)%print()

        ! var_id_w1 = get_list_index(2_8, stacks(1)%idxs)
        ! var_id_w2 = get_list_index(7_8, stacks(1)%idxs)
        var_id_w1 = get_list_index(2_8, stacks(1)%idxs)
        var_id_b1 = get_list_index(5_8, stacks(1)%idxs)
        var_id_w2 = get_list_index(10_8, stacks(1)%idxs)
        var_id_b2 = get_list_index(13_8, stacks(1)%idxs)

        call clear_grad(loss)
        call backward(loss)

        w1 = w1 - 0.2d0*stacks(1)%vars(var_id_w1)%grd%m
        b1 = b1 - 0.2d0*stacks(1)%vars(var_id_b1)%grd%v
        w2 = w2 - 0.2d0*stacks(1)%vars(var_id_w2)%grd%m
        b2 = b2 - 0.2d0*stacks(1)%vars(var_id_b2)%grd%v
        
    end do
    ! print*, i, w1, " :: ", loss%v
    ! print*, i, w2, " :: ", loss%v


    ! do i=1, 100, 1
    !     print*, x(i,1), y_true%v(i,1), y_pred%v(i,:)
    ! end do



end program main_multi_layer_perceptron