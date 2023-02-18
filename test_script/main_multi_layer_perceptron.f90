program main_multi_layer_perceptron
    use mod_stats
    use mod_timer
    use mod_wengert_list, only: variable, optimizer 
    use mod_backwards
    use mod_my_mlp
    use mod_mnist_classifier
    use mod_loss_function
    use mod_random
    use mod_const
    use mod_encoder
    use mod_common
    implicit none

    type(variable)         :: input_var, output_vars(1), output_var, weight
    type(variable)         :: input_vars(5), y_true, y_pred
    type(variable)         :: loss_var(1), loss, losses(2), tmp
    type(my_mlp)           :: nn_model, nn_model2
    type(mnist_classifier) :: mnist_clf
    type(optimizer)        :: opt
    integer(kind=8)        :: date_value1(8), date_value2(8), i, j, e
    integer(kind=8)        :: var_id_w1, var_id_b1
    integer(kind=8)        :: var_id_w2, var_id_b2
    real(kind=8)           :: x(100,1), y(100,1), acc

    character(len=256)           :: fn_x_train, fn_y_train
    character(len=256)           :: fn_x_test, fn_y_test
    real(kind=8), allocatable    :: x_train(:,:), x_test(:,:)
    real(kind=8), allocatable    :: x_batch(:,:)
    integer(kind=8), allocatable :: y_train(:,:), y_test(:,:)
    integer(kind=8), allocatable :: y_train_ohe(:,:), y_test_ohe(:,:)
    real(kind=8), allocatable    :: y_batch(:,:)
    integer(kind=8)              :: ini, fin, max_loop, max_epoch, n_samples, n_mini_batch
    integer(kind=8), allocatable :: idxs(:)
    type(one_hot_encoder)        :: oh_enc
    type(batch_idxs_generator)   :: idx_gen
    real(kind=8) :: dummy

    ! Read data
    fn_x_train = "../sample_data/mnist_X_train.bin"
    fn_y_train = "../sample_data/mnist_y_train.bin"
    fn_x_test  = "../sample_data/mnist_X_test.bin"
    fn_y_test  = "../sample_data/mnist_y_test.bin"
    call read_bin_2d(fn_x_train, x_train)
    call read_bin_2d(fn_y_train, y_train)
    call read_bin_2d(fn_x_test, x_test)
    call read_bin_2d(fn_y_test, y_test)

    ! Optimizer setup
    opt = optimizer(top_k=0.1d0)
    call opt%sgd(learning_rate=0.2d0, momentum=0.9d0)

    ! Model
    mnist_clf = mnist_classifier(opt=opt)

    ! Normalize data
    x_train = x_train / 255d0
    x_test = x_test / 255d0

    ! One-Hot encoding
    oh_enc = one_hot_encoder()
    y_train_ohe = oh_enc%transform(y_train)

    ! Training setup
    max_epoch = 2
    n_samples = size(x_train, dim=1)
    n_mini_batch = 1000

    ! Allocate Minibatches
    allocate(idxs(n_mini_batch))
    allocate(x_batch(n_mini_batch, 784))
    allocate(y_batch(n_mini_batch, 10))
    
    do e=1, max_epoch, 1
        idx_gen = batch_idxs_generator(n_samples, n_mini_batch)
        do i=1, idx_gen%n_loop, 1
            ! Batch indices
            idxs = idx_gen%get_batch_idxs(loop_index=i)
            call progress_bar(i, idx_gen%n_loop, 1_8)

            ! Batch data
            x_batch = x_train(idxs, :)
            y_batch = y_train_ohe(idxs, :)

            ! Set variables
            input_var = variable(x_batch)
            y_true = variable(y_batch)

            ! Predict & Loss
            y_pred = mnist_clf%forward(input_var)
            loss = binary_cross_entropy_error_with_logits(y_true, y_pred)
            ! call mnist_clf%opt_ptr%stack_ptr%print()

            ! Clear Grad & Backward
            call clear_grad(loss)
            call backward(loss)

            ! Parameter Update
            call opt%update()
        end do

        ! Validation
        input_var = variable(x_test)
        call mnist_clf%no_list()
        y_pred = mnist_clf%forward(input_var)
        y_test_ohe = oh_enc%transform(y_test)
        y_true = variable(y_test_ohe)
        acc = accuracy_nn(y_true, y_pred)
        print*, "    Epoch: ", e, "loss: ", loss%var%s, " acc: ", acc
    end do




    stop
    opt = optimizer()
    call opt%sgd(learning_rate=0.1d0, momentum=0.9d0)

    ! X
    do i=1, 100, 1
        x(i,1) = 0.01*(i-1)
    end do    

    ! y
    y = sin(x*2d0*pi_)
    
    nn_model = my_mlp(opt=opt)
    
    input_var = variable(x)
    y_true = variable(y)
    do i=1, 10000, 1

        y_pred = nn_model%forward(input_var)
        loss = mean_squared_error(y_true, y_pred)
        if (mod(i,1000)==0) print*, "LOOP INDEX", i, "loss", loss%var%s

        call clear_grad(loss)
        call backward(loss)

        call opt%update()
    end do
contains

    function label_smoothing(x_hard, max_val) result(x_smooth)
        implicit none
        real(kind=8), intent(in) :: x_hard(:,:)
        real(kind=8), allocatable :: x_smooth(:,:)
        real(kind=8) :: max_val
        real(kind=8) :: smooth_val, n_samples, n_columns

        n_samples = size(x_hard, dim=1)
        n_columns = size(x_hard, dim=2)
        allocate(x_smooth, source=x_hard)

        smooth_val = (1d0 - max_val) / dble(n_columns - 1)

        x_smooth(:,:) = x_hard * max_val

        x_smooth(:,:) = x_smooth(:,:) + (1-x_hard(:,:)) * smooth_val
    end function 

    function accuracy_nn(y_true, y_pred) result(acc)
        implicit none
        type(variable) :: y_true
        type(variable) :: y_pred
        real(kind=8)   :: acc
        integer(kind=8), allocatable :: y_true_max(:), y_pred_max(:) 
        integer(kind=8) :: n, cnt, tot, factor

        cnt = 0
        tot = y_true%sizes()
        allocate(y_true_max(tot))
        allocate(y_pred_max(tot))
        if (y_true%var%dtype==2 .and. y_pred%var%dtype==2) then
            y_true_max(:) = maxloc(y_true%var%m, dim=2)
            y_pred_max(:) = maxloc(y_pred%var%m, dim=2)
            do n=1, tot, 1
                if (y_true_max(n) == y_pred_max(n)) cnt = cnt + 1
            end do
        end if
        acc = cnt / dble(tot)
    end function accuracy_nn





end program main_multi_layer_perceptron