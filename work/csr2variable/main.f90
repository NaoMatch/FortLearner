program main
    use mod_timer
    use mod_csr
    use mod_variable
    use mod_dense2csr
    use mod_sparse_matrix_multiplication
    use mod_intrinsics
    use mod_functions
    use mod_optimizers
    use mod_losses
    use mod_random_number_regressor
    use mod_random_number_regressor_with_sp
    implicit none

    integer(kind=8)        :: date_value1(8), date_value2(8)
    integer(kind=8) :: top_k, i, max_iter
    integer(kind=8) :: m, k, l
    integer(kind=8) :: time_f, time_b
    type(csr_matrix) :: csr_a, csr_b
    type(variable) :: var_csr, var_a, var_b, var_b_t, var_c, var_d
    real(kind=8), allocatable :: a(:,:), b(:,:), b_t(:,:)
    type(random_number_regressor) :: my_model_
    type(random_number_regressor_with_sp) :: my_model
    type(variable) :: mse_loss, y_out
    type(variable) :: x_test_, x_train_, y_test_, y_test_out, y_train_
    type(sgd) :: opt
    type(momentum_sgd) :: opt_mom

    real(kind=8), allocatable :: x_train(:,:), y_train(:,:)
    real(kind=8), allocatable :: x_test(:,:), y_test(:,:)
    integer(kind=8) :: n_train, n_test, n_cols, n_outs
    integer(kind=8) :: batch, batch_size, epoch, n_epoch

    call init_stack()

    my_model = random_number_regressor_with_sp()
    my_model_ = random_number_regressor()

    n_cols = 960
    n_outs = 16

    n_train = 10000
    n_test = 10000
    allocate(x_train(n_train, n_cols))
    allocate(y_train(n_train, n_outs))
    allocate(x_test(n_test, n_cols))
    allocate(y_test(n_test, n_outs))
    

    call random_number(x_train)
    call random_number(y_train)
    call random_number(x_test)
    call random_number(y_test)

    x_test_ = variable(x_test(:,:))
    y_test_ = variable(y_test(:,:))

    print*, "Start: ", size(vstack), size(fstack)
    n_epoch = 100
    batch_size = 1000
    do epoch=1, n_epoch, 1
        call train_mode()
        call date_and_time(values=date_value1)
        do batch=1, n_train-batch_size+1, batch_size
            x_train_ = variable(x_train(batch:batch+batch_size-1,:))
            y_train_ = variable(y_train(batch:batch+batch_size-1,:))

            call date_and_time(values=date_value1)
            y_out = my_model%act(x_train_)
            call date_and_time(values=date_value2)
            time_f = time_diff(date_value1, date_value2)
            ! y_out = softmax_(y_out)
            ! bce_loss = cross_entropy_(y_train_, y_out, n_classes=10_8)
            call date_and_time(values=date_value1)
            mse_loss = mean_squared_error_(y_train_, y_out)
            call mse_loss%backward()
            call date_and_time(values=date_value2)
            time_b = time_diff(date_value1, date_value2)

            call opt_mom%update(mse_loss)
            print*, batch, "update", size(vstack), mse_loss%get_data(), time_f, time_b
            
            call clear_stack(mse_loss)
        end do
        call date_and_time(values=date_value2)

        call test_mode()

        y_test_out = my_model%act(x_test_)
        mse_loss = mean_squared_error_(y_test_, y_test_out)

        print*, "update", size(vstack), mse_loss%get_data(), time_diff(date_value1, date_value2)
    end do

















    ! max_iter = 1
    ! m = 1024 * 2
    ! k = 512
    ! l = 128
    ! top_k = 16

    ! allocate(a(m, k))
    ! allocate(b(k, l), b_t(l,k))
    ! call random_number(a)
    ! call random_number(b)
    ! a = 2d0 * a - 1d0
    ! b_t = transpose(b)

    ! print*, 1
    ! csr_a = dense2csr_weighted_sampling_mat(a, top_k, &
    !     dim=2_8, start_index=0_8, negative_weights="absolute")
    ! print*, 1,2
    ! a = csr_a%to_dense()
    ! print*, 1,3
    ! var_a = variable(a)
    ! print*, 1,4
    ! var_csr = variable(csr_a)
    ! var_b_t = variable(b_t)
    
    ! print*, "Forward"
    ! var_d = sp_matmul_(var_csr, var_b_t)
    ! print*, var_d%get_shape()
    
    ! print*, "Backward"
    ! call var_d%backward()

end program main
