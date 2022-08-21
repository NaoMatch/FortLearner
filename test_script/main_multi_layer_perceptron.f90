program main_multi_layer_perceptron
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

    opt = optimizer_(learning_rate=.1d0, alpha=0d0)

    input_vars(1) = variable_(1.5d0)
    input_vars(2) = variable_(0.5d0)

    nn_model = my_mlp(opt=opt)
    output_vars = nn_model%forward(input_vars)

    call stacks(1)%print()
    call backward(output_vars)

    print*, stacks(1)%vars(1)%g
    ! print*, stacks(1)%vars(2)%g
    ! print*, stacks(1)%vars(3)%g








    ! type(variable), target :: var_x, var_y, var_a, var_b, var_c, var_d, var_e, var_f, var_g, var_h, var_i
    ! type(variable), target :: var_j, var_k
    ! type(variable), target :: var_v, var_w
    ! type(variable), target :: var_xx, var_yy, var_ww, var_bb
    ! real(kind=8) :: x(1,1), y(1,1), g(1,1)
    ! real(kind=8) :: v(1,5), w(1,5)
    ! real(kind=8) :: xx(100,2), yy(100,1), ww(1,2), bb(1,1)
    ! type(activation_function) :: act1, act2, act3, act4, act5, act6, act7, act8, act9
    ! integer(kind=8) :: iter, max_iter
    ! integer(kind=4) :: batch_size
    ! ! type(linear_regression) :: lin_reg
    ! ! type(data_holder), target :: dholder
    ! ! type(data_holder), pointer :: dholder_ptr
    ! type(optimizer) :: opt
    ! type(variable_ptr), allocatable :: var_sets(:)


    ! max_iter = 20
    ! x = .5d0
    ! v = 1d0
    ! g = 1d0
    
    ! x = .5d0
    ! max_iter = 100
    ! do iter=1, max_iter, 1
    !     var_x = variable(x)
    !     ! 
    !     ! var_c = act1%sq2ex(var_x)
    !     ! 
    !     var_a = act1%square(var_x)
    !     var_b = act2%exponential(var_a)
    !     var_c = act3%square(var_b)        

    !     var_c%g = g
    !     call var_c%backward()
    !     x = var_x%v - 0.1d0 * var_x%g
    ! end do
    ! print*, x


    ! x = .5d0
    ! max_iter = 1000
    ! do iter=1, max_iter, 1
    !     var_x = variable(x)
    !     ! 
    !     var_a = act1%square(var_x)
    !     var_b = act2%exponential(var_a)
    !     var_c = act3%square(var_b)   
    !     var_d = act4%exponential(var_c)  
    !     var_e = act5%summation(var_d)

    !     var_e%g = g
    !     call var_e%backward()
    !     ! nullify(var_d%creator_ptr)
    !     ! nullify(var_a%creator_ptr)
    !     x = var_x%v - 0.001d0 * var_x%g
    !     print*, iter, x
    ! end do
    ! print*, x

    ! v = .5d0
    ! max_iter = 100
    ! do iter=1, max_iter, 1
    !     var_v = variable(v)

    !     var_a = act1%square(var_v)
    !     var_b = act2%exponential(var_a)
    !     var_c = act3%summation(var_b, dim=1)
    !     var_d = act4%summation(var_c, dim=2)
        
    !     var_d%g = g
    !     call var_d%backward()
    !     v = var_v%v - 0.1d0 * var_v%g
    ! end do
    ! print*, v

    ! x = .5d0
    ! y = .5d0
    ! v = .5d0
    ! max_iter = 1000
    ! do iter=1, max_iter, 1
    !     var_x = variable(x)
    !     var_y = variable(y)

    !     var_a = act1%multiply(var_x, var_y)
    !     var_b = act2%square(var_a)
    !     var_c = act3%exponential(var_b)
        
    !     var_c%g = g
    !     call var_c%backward_variable_subroutine()
    !     x = var_x%v - 1d0 * var_x%g
    !     y = var_y%v - 1d0 * var_y%g

    !     ! print*, iter, x, y
    ! end do
    ! print*, x
    ! print*, y

    ! x = .5d0
    ! y = .5d0
    ! v = .5d0
    ! w = .5d0
    ! max_iter = 200
    ! do iter=1, max_iter, 1
    !     var_x = variable(x)
    !     var_y = variable(y)

    !     var_a = act1%square(var_x)
    !     var_b = act2%exponential(var_a)

    !     var_c = act3%square(var_y)
    !     var_d = act4%exponential(var_c)

    !     var_e = act5%multiply(var_b, var_d)
        
    !     var_e%g = g
    !     call var_e%backward_variable_subroutine()
    !     x = var_x%v - 0.1d0 * var_x%g
    !     y = var_y%v - 0.1d0 * var_y%g
    ! end do
    ! print*, x
    ! print*, y


    ! x = .5d0
    ! y = .5d0
    ! v = .5d0
    ! w = .5d0
    ! max_iter = 200
    ! do iter=1, max_iter, 1
    !     var_v = variable(v)
    !     var_w = variable(w)

    !     var_a = act1%square(var_v)
    !     var_b = act2%exponential(var_a)

    !     var_c = act3%square(var_w)
    !     var_d = act4%exponential(var_c)

    !     var_e = act5%multiply(var_b, var_d)
    !     var_f = act6%summation(var_e)
        
    !     var_f%g = g
    !     call var_f%backward_variable_subroutine()
    !     v = var_v%v - 0.1d0 * var_v%g
    !     w = var_w%v - 0.1d0 * var_w%g
    ! end do
    ! print*, v
    ! print*, w

    ! x = .5d0
    ! y = .5d0
    ! v = .5d0
    ! w = .5d0
    ! call random_number(xx(:,:))
    ! call random_number(bb(:,:))
    ! yy(:,1) = sum(xx, dim=2)
    ! var_xx = variable(xx)
    ! var_yy = variable(yy)
    ! max_iter = 10000
    ! batch_size = 100
    ! batch_size = 100
    ! var_j = variable(batch_size+0d0)

    ! call random_number(ww(:,:))
    ! print*, "Start: ", ww, bb
    ! do iter=1, max_iter, 1
    !     ! Set
    !     var_ww = variable(ww)
    !     var_bb = variable(bb)

    !     ! Broadcast
    !     var_a  = act1%broadcast(var_ww, dim=1, n_copies=batch_size)
    !     var_b  = act2%broadcast(var_bb, dim=1, n_copies=batch_size)

    !     ! y = w*x
    !     var_c  = act3%multiply(var_xx, var_a)
    !     var_d  = act4%summation(var_c, dim=2)
        
    !     ! y = y + b
    !     var_e = act5%addition(var_d, var_b)

    !     ! l = (y_pred-y_true)^2 / n
    !     var_f  = act6%substraction(var_e, var_yy) ! y_diff = (y_pred-y_true)
    !     var_g  = act7%square(var_f)               ! y_diff^2
    !     var_h  = act8%summation(var_g, dim=1)     ! sum(y_diff^2_i)
    !     var_i  = act9%division(var_h, var_j)      ! sum(y_diff^2_i) / n

    !     var_i%g = g
    !     call var_i%backward()

    !     ww = var_ww%v - 0.01d0 * var_ww%g
    !     bb = var_bb%v - 0.01d0 * var_bb%g
    ! end do
    ! print*, "Stop:  ", ww, bb, sum(xx), sum(yy)

    ! lin_reg = linear_regression()
    ! dholder = data_holder(xx, yy, is_trans_x=.false.)
    ! dholder_ptr => dholder
    ! call lin_reg%fit(dholder_ptr)
    ! print*, lin_reg%coefs_
    ! print*, lin_reg%intercept_

    ! max_iter = 10000
    ! batch_size = 100
    ! batch_size = 100
    ! x = .5d0
    ! y = .5d0
    ! v = .5d0
    ! w = .5d0
    ! call random_number(xx(:,:))
    ! call random_number(ww(:,:))
    ! call random_number(bb(:,:))
    ! allocate(var_sets(2))
    ! yy(:,1) = sum(xx, dim=2)
    ! print*, "Start: ", ww, bb

    ! ! Set Data
    ! var_xx = variable(xx)
    ! var_yy = variable(yy)

    ! ! Set Optimizer
    ! opt = optimizer(learning_rate=0.1d0, alpha=0d0)

    ! ! Set Parameters
    ! var_j  = variable(batch_size+0d0)
    ! var_ww = variable(ww)
    ! var_bb = variable(bb)
    ! do iter=1, max_iter, 1
    !     ! Broadcast
    !     var_a  = act1%broadcast(var_ww, dim=1, n_copies=batch_size)
    !     var_b  = act2%broadcast(var_bb, dim=1, n_copies=batch_size)

    !     ! y = w*x
    !     var_c  = act3%multiply(var_xx, var_a)
    !     var_d  = act4%summation(var_c, dim=2)
        
    !     ! y = y + b
    !     var_e = act5%addition(var_d, var_b)

    !     ! l = (y_pred-y_true)^2 / n
    !     var_f  = act6%substraction(var_e, var_yy) ! y_diff = (y_pred-y_true)
    !     var_g  = act7%square(var_f)               ! y_diff^2
    !     var_h  = act8%summation(var_g, dim=1)     ! sum(y_diff^2_i)
    !     var_i  = act9%division(var_h, var_j)      ! sum(y_diff^2_i) / n

    !     ! Initialize Gradient
    !     var_i%g = g
    !     call var_i%backward()

    !     ! update parameters
    !     ! ww = var_ww%v - 0.01d0 * var_ww%g
    !     ! bb = var_bb%v - 0.01d0 * var_bb%g
    !     ! var_sets = [var_ww, var_bb]
    !     var_sets(1)%ptr => var_ww
    !     var_sets(2)%ptr => var_bb
    !     call opt%update(var_sets)
    ! end do
    ! print*, "Stop:  ", var_ww%v, var_bb%v, sum(xx), sum(yy)

    ! lin_reg = linear_regression()
    ! dholder = data_holder(xx, yy, is_trans_x=.false.)
    ! dholder_ptr => dholder
    ! call lin_reg%fit(dholder_ptr)
    ! print*, lin_reg%coefs_
    ! print*, lin_reg%intercept_


    ! nn_model = my_nn_model()
    ! call nn_model%run(input_var)
    ! y_pred = nn_model%predict(input_var)
    ! var_x = variable()
    ! var_y = variable()
    ! 
    ! var = dense(var_x, in_dim=764, out_dim=100)
    ! 
    ! 
    ! 



end program main_multi_layer_perceptron