program main_optimize
    use mod_optimization
    implicit none

    type(steepest_descent) :: steep
    type(newton_method) :: newton
    real(kind=8), allocatable :: x_min(:), x_ini(:)
    integer(kind=8) :: n_vars
    
    n_vars = 100
    allocate(x_ini(n_vars))
    call random_number(x_ini)
    x_ini = 100


    steep = steepest_descent()
    x_min = steep%optimize(x_ini=x_ini, loss=loss, grad=grad)
    print*, "Steepest: ", x_min

    newton = newton_method()
    x_min = newton%optimize(x_ini=x_ini, loss=loss, grad=grad, hess=hess)
    print*, "Newton: ", x_min




contains

    function loss(x)
        real(kind=8) :: loss
        real(kind=8), intent(in) :: x(:)
        loss = sum(x(:) **2d0)
    end function loss
        
    function grad(x)
        real(kind=8), allocatable :: grad(:)
        real(kind=8), intent(in) :: x(:)
        grad = 2d0 * x
    end function grad

    function hess(x)
        real(kind=8), ALLOCATABLE :: hess(:,:)
        real(kind=8), intent(in) :: x(:)

        integer(kind=8) :: n_variable, i, j
        n_variable = size(x)
        allocate(hess(n_variable, n_variable))
        hess = 0d0
        do i=1, n_variable, 1
            hess(i,i) = 2d0
        end do
    end function hess



end program main_optimize
