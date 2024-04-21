program main
    use mod_variable
    use mod_intrinsics
    use mod_functions
    use mod_layers
    use mod_optimizers
    use mod_visualize
    use mod_losses
    use mod_two_layer_model
    use mod_simple_residual_block
    implicit none

    type(variable) :: var, var1, var2, var3, var4
    type(variable) :: x, y, z, t
    type(variable) :: W, b
    type(variable) :: mse_loss
    type(linear) :: l1, l2, linears(3)
    type(two_layer_model) :: model
    type(sgd) :: opt
    type(momentum_sgd) :: opt_mom
    type(simple_residual_block) :: res_block
    character(len=:), allocatable :: dot_txt
    integer(kind=8) :: i, n, j, id, l
    integer(kind=8) :: n_rows, n_cols, n_outs
    real(kind=8) :: coef, lr
    real(kind=8) :: mat1(2,2), mat2(2,2)
    real(kind=8), allocatable :: mat3(:,:), mat4(:,:)
    real(kind=8), allocatable :: x_grad(:,:), y_grad(:,:)
    real(kind=8), allocatable :: x_(:,:), y_(:,:)
    real(kind=8), allocatable :: W_(:,:), b_(:,:)

    call init_stack()

    ! x = variable(2d0)
    ! y = x**2 
    ! z = y**2 + y**2
    ! call z%backward()
    ! print*, z%get_data()
    ! print*, x%get_grad()
    ! stop




    n_rows = 100
    n_cols = 1
    n_outs = 1
    allocate(x_(n_rows, n_cols), y_(n_rows, n_outs))
    call random_number(x_)
    call random_number(y_)
    y_ = y_ + sin(2d0 * 3.14159265359d0 * x_)
    ! y_ = sin(2d0 * 3.14159265359d0 * x_)

    x = variable(x_)
    y = variable(y_)

    linears(1) = linear(out_size=10_8, no_bias=.false.)
    linears(2) = linear(out_size=5_8,  no_bias=.false.)
    linears(3) = linear(out_size=1_8,  no_bias=.false.)

    lr = 0.2d0
    model = two_layer_model(hidden_size=10_8, out_size=1_8)
    res_block = simple_residual_block(hidden_size=10_8, out_size=1_8, no_bias=.false.)
    opt = sgd(lr=lr)
    opt_mom = momentum_sgd(lr=lr, momentum=0.9d0)
    do i=1, 10000, 1
        call init_stack()
        z = model%act(x)

        ! print*, "forward"
        ! z = linears(1)%act(x)
        ! z = sigmoid_(z)
        ! z = linears(2)%act(z)
        ! z = sigmoid_(z)
        ! z = linears(3)%act(z)
        
        ! print*, "loss"
        mse_loss = mean_squared_error_(y, z)
        
        ! print*, "backward"
        call mse_loss%backward()

        call opt%update(mse_loss)
        
        if (mod(i, 10)==0) then
            print*, i, " :: ", mse_loss%get_data(), size(vstack), sum(x%get_data())
        end if
    end do
    call plot_dot_graph(mse_loss, "two_layer_model.png", verbose=.true.)

    ! y = variable(reshape([1d0,1d0,1d0,1d0,1d0,1d0], [2,3]))
    ! z = get_item_(y, [1_8,1_8,2_8])
    ! call z%backward()

    ! print*, z%get_data()
    ! print*, y%get_data(), "::", y%get_grad()

    ! call random_number(x_)
    ! x_ = 20d0 * x_ - 10d0
    ! x = variable(x_(:3,:))
    ! y = relu_(x)
    ! call y%backward()

    ! print*, "x: ", x%get_data(), x%get_grad()
    ! print*, "y: ", y%get_data()



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
