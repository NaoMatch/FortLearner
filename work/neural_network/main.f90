program main
    use mod_common
    use mod_variable
    use mod_intrinsics
    use mod_functions
    use mod_layers
    use mod_optimizers
    use mod_visualize
    use mod_losses
    use mod_two_layer_model
    use mod_mnist_classifier
    use mod_simple_residual_block
    use mod_timer
    use mod_metric
    use mod_scaler
    implicit none

    type(variable) :: var, var1, var2, var3, var4
    type(variable) :: x, y, z, t
    type(variable) :: W, b
    type(variable) :: y_test_out
    type(variable) :: mse_loss, ce_loss, bce_loss
    type(mnist_classifier) :: mnist_clf
    type(linear) :: l1, l2, linears(3)
    type(two_layer_model) :: model
    type(sgd) :: opt
    type(momentum_sgd) :: opt_mom
    type(simple_residual_block) :: res_block
    type(metrics) :: mtrc
    type(standard_scaler) :: ss_scl
    character(len=:), allocatable :: dot_txt
    integer(kind=8) :: i, n, j, id, l
    integer(kind=8) :: n_rows, n_cols, n_outs
    real(kind=8) :: coef, lr
    real(kind=8) :: mat1(2,2), mat2(2,2)
    real(kind=8), allocatable :: mat3(:,:), mat4(:,:)
    real(kind=8), allocatable :: x_grad(:,:), y_grad(:,:)
    real(kind=8), allocatable :: x_(:,:), y_(:,:)
    real(kind=8), allocatable :: W_(:,:), b_(:,:)
    real(kind=8), allocatable :: x_train(:,:), x_test(:,:)
    real(kind=8), allocatable :: y_train(:,:), y_test(:,:)
    type(variable) :: x_train_, y_train_, y_out
    type(variable) :: x_test_, y_test_
    character(len=:), allocatable :: file_name_x_train_bin, file_name_x_test_bin
    character(len=:), allocatable :: file_name_y_train_bin, file_name_y_test_bin
    integer(kind=8) :: n_epoch, batch_size, epoch, batch, s, counter
    integer(kind=8)        :: date_value1(8), date_value2(8)
    integer(kind=8), allocatable :: hidden_dims(:)
    integer(kind=8) :: n_test
    integer(kind=8), allocatable :: y_pred(:), y_true(:)

    call init_stack()

    ! x = variable(2d0)
    ! y = variable(1d0)
    ! z = mean_squared_error_(x, y)
    ! call z%backward()

    ! print*, x%get_data(), x%get_grad()
    ! print*, y%get_data(), y%get_grad()
    ! print*, z%get_data(), z%get_grad()

    ! stop










    lr = 0.1d0
    opt = sgd(lr=lr)
    opt_mom = momentum_sgd(lr=lr, momentum=0.9d0)

    ! hidden_dims = []
    allocate(hidden_dims(0))
    ! mnist_clf = mnist_classifier([512_8, 128_8])
    mnist_clf = mnist_classifier(hidden_dims)
    file_name_x_train_bin = "../../sample_data/mnist_train_images.bin"
    file_name_y_train_bin = "../../sample_data/mnist_train_labels.bin"
    call read_bin_2d(file_name_x_train_bin, x_train)
    call read_bin_2d(file_name_y_train_bin, y_train)

    file_name_x_test_bin = "../../sample_data/mnist_test_images.bin"
    file_name_y_test_bin = "../../sample_data/mnist_test_labels.bin"
    call read_bin_2d(file_name_x_test_bin, x_test)
    call read_bin_2d(file_name_y_test_bin, y_test)

    ! x_train = x_train / 255d0
    ! x_test  = x_test  / 255d0

    ss_scl = standard_scaler()
    call ss_scl%fit(x_train)
    x_train = ss_scl%transform(x_train)
    x_test = ss_scl%transform(x_test)

    x_test_ = variable(x_test(:,:))
    y_test_ = variable(y_test(:,:))

    print*, "Start: ", size(vstack), size(fstack)
    n_epoch = 100
    batch_size = 10000
    do epoch=1, n_epoch, 1
        call train_mode()
        call date_and_time(values=date_value1)
        do batch=1, 60000-batch_size+1, batch_size
            counter = 0
            x_train_ = variable(x_train(batch:batch+batch_size-1,:))
            y_train_ = variable(y_train(batch:batch+batch_size-1,:))

            y_out = mnist_clf%act(x_train_)
            ! y_out = softmax_(y_out)
            ! bce_loss = cross_entropy_(y_train_, y_out, n_classes=10_8)
            bce_loss = softmax_with_loss_(y_train_, y_out, n_classes=10_8)
            call bce_loss%backward()

            y_true = maxloc(y_train(batch:batch+batch_size-1,:), dim=2)
            y_pred = maxloc(y_out%get_data(), dim=2)
            counter = 0
            n_test = size(y_true, dim=1)
            do n=1, n_test, 1
                if (y_true(n) == y_pred(n)) then
                    counter = counter + 1
                end if
            end do

            call opt_mom%update(bce_loss)
            print*, batch, "update", size(vstack), bce_loss%get_data(), sum(y_out%get_data()), counter
            
            call clear_stack(bce_loss)
        end do
        call date_and_time(values=date_value2)

        call test_mode()

        y_test_out = mnist_clf%act(x_test_)
        ! y_test_out = softmax_(y_test_out)
        ! bce_loss = cross_entropy_(y_test_, y_test_out, n_classes=10_8)
        bce_loss = softmax_with_loss_(y_test_, y_test_out, n_classes=10_8)

        y_true = maxloc(y_test(:,:), dim=2)
        y_pred = maxloc(y_test_out%get_data(), dim=2)
        counter = 0
        n_test = size(y_true, dim=1)
        do n=1, n_test, 1
            if (y_true(n) == y_pred(n)) then
                counter = counter + 1
            end if
        end do

        print*, "update", size(vstack), bce_loss%get_data(), counter, time_diff(date_value1, date_value2)
    end do


contains

    function rosenbrock(x, y) result(z)
        implicit none
        type(variable) :: x, y
        type(variable) :: z
        z = 100d0 * (y-x**2)**2 + (x-1d0) ** 2
    end function rosenbrock

    function factorial(i) result(res)
        implicit none
        integer(kind=8) :: i
        real(kind=8) :: res
        integer(kind=8) :: n
        
        res = 1
        if (i==0_8) then
            return
        end if
        
        do n=1, i, 1
            res = res*n
        end do
    end function factorial

end program main
