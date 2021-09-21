program main
    use mod_optimization
    implicit none
    
    real(kind=8), allocatable :: x(:), x_min(:)
    type(steepest_descent) :: stp_des
    type(bfgs) :: bfgs_opt
    type(newton_method) :: newton
    type(simulated_annmealing) :: sim_ann

    stp_des = steepest_descent()
    bfgs_opt = bfgs()
    newton = newton_method()
    sim_ann = simulated_annmealing(initial_temperature=10d0, max_iter=5000_8)



    allocate(x(4))
    call random_number(x)
    x = 2*x

    print*, "Steepest Descent ---------------------------------------------"
    print*, "Initial: ", x
    x_min = stp_des % optimize(x, x_sq, x_sq_grad)
    print*, "Minimum: ", x_min, x_sq(x_min)

    print*, "BFGS ---------------------------------------------"
    print*, "Initial: ", x
    x_min = bfgs_opt % optimize(x, x_sq, x_sq_grad)
    print*, "Minimum: ", x_min, x_sq(x_min)

    print*, "BFGS ---------------------------------------------"
    print*, "Initial: ", x
    x_min = newton % optimize(x, x_sq, x_sq_grad, x_sq_hess)
    print*, "Minimum: ", x_min, x_sq(x_min)

    print*, "Simulated Annealing ---------------------------------------------"
    print*, "Initial: ", x
    x_min = sim_ann % optimize(x, x_sq)
    print*, "Minimum: ", x_min, x_sq(x_min)


contains

    function x_sq(x)
        implicit none
        real(kind=8), intent(in) :: x(:)
        real(kind=8)             :: x_sq
        x_sq = sum(x**2d0) / 2d0
    end function

    function x_sq_grad(x)
        implicit none
        real(kind=8), intent(in)  :: x(:)
        real(kind=8), allocatable :: x_sq_grad(:)
        x_sq_grad = x
    end function

    function x_sq_hess(x)
        implicit none
        real(kind=8), intent(in)  :: x(:)
        real(kind=8), allocatable :: x_sq_hess(:,:)
        integer(kind=8) :: n
        n = size(x)
        allocate(x_sq_hess(n,n))
        call identity(x_sq_hess, n)
    end function

end program main
