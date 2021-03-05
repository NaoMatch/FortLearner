module mod_optimization
    use mod_linalg, only: multi_mat_vec, inversion
    implicit none
    
    type line_search
    contains
        procedure :: armijo_condition
    end type line_search


    type opt_method
        integer(kind=8) :: max_iter=500_8
        real(kind=8)    :: tolerance=1d-6
        real(kind=8)    :: alpha_ini=1d0
    end type opt_method


    type, extends(opt_method) :: steepest_descent
    contains
        procedure :: optimize => optimize_steepest_descent
    end type steepest_descent

    type, extends(opt_method) :: newton_method
    contains
        procedure :: optimize => optimize_newton_method
    end type newton_method



    interface steepest_descent
        module procedure :: new_steepest_descent
    end interface steepest_descent

    interface newton_method
        module procedure :: new_newton_method
    end interface newton_method

contains


    subroutine armijo_condition(this, alpha, theta, theta_update, loss, grad)
        implicit none
        class(line_search) :: this
        real(kind=8), intent(inout) :: alpha
        real(kind=8), intent(in)    :: theta(:)
        real(kind=8), intent(in)    :: theta_update(:)
        interface
            function loss(x)
                real(kind=8) :: loss
                real(kind=8), intent(in) :: x(:)
            end function loss
        end interface
        interface
            function grad(x)
                real(kind=8), allocatable :: grad(:)
                real(kind=8), intent(in) :: x(:)
            end function grad
        end interface

        real(kind=8), allocatable :: theta0(:)

        real(kind=8), parameter :: xi1=0.001d0
        real(kind=8), parameter :: xi2=0.9d0
        real(kind=8), parameter :: tau=0.6d0

        real(kind=8) :: lhs_a, rhs_a
        real(kind=8) :: tmp_1, tmp_2

        theta0 = theta
        tmp_1 = loss(theta0)
        tmp_2 = sum(grad(theta0)**2d0)

        do while (.true.)
            theta0 = theta - alpha * theta_update

            lhs_a = loss(theta0)
            rhs_a = tmp_1 - xi1 * alpha * tmp_2

            if ( lhs_a .gt. rhs_a ) then
                alpha = alpha * tau
            else
                exit
            end if
        end do
    end subroutine armijo_condition


    function new_steepest_descent(max_iter, tolerance, alpha_ini)
        implicit none
        type(steepest_descent) :: new_steepest_descent
        integer(kind=8), optional   :: max_iter
        real(kind=8), optional   :: tolerance
        real(kind=8), optional   :: alpha_ini

        if (present(max_iter)) new_steepest_descent % max_iter = max_iter
        if (present(tolerance)) new_steepest_descent % tolerance = tolerance
        if (present(alpha_ini)) new_steepest_descent % alpha_ini = alpha_ini
    end function new_steepest_descent

    function new_newton_method(max_iter, tolerance, alpha_ini)
        implicit none
        type(newton_method) :: new_newton_method
        integer(kind=8), optional   :: max_iter
        real(kind=8), optional   :: tolerance
        real(kind=8), optional   :: alpha_ini

        if (present(max_iter)) new_newton_method % max_iter = max_iter
        if (present(tolerance)) new_newton_method % tolerance = tolerance
        if (present(alpha_ini)) new_newton_method % alpha_ini = alpha_ini
    end function new_newton_method


    function optimize_steepest_descent(this, x_ini, loss, grad)
        implicit none
        class(steepest_descent)     :: this
        real(kind=8), allocatable   :: optimize_steepest_descent(:)
        real(kind=8), intent(in)    :: x_ini(:)
        interface
            function loss(x)
                real(kind=8) :: loss
                real(kind=8), intent(in) :: x(:)
            end function loss
        end interface
        interface
            function grad(x)
                real(kind=8), allocatable :: grad(:)
                real(kind=8), intent(in) :: x(:)
            end function grad
        end interface

        type(line_search) :: ls

        real(kind=8) :: alpha
        real(kind=8), allocatable :: x0(:), x0_update(:)
        integer(kind=8) :: iter, n_variables

        n_variables = size(x_ini)

        allocate(x0(n_variables), x0_update(n_variables))
        x0 = x_ini

        do iter=1, this%max_iter, 1
            alpha = 1d0
            x0_update = grad(x0)
            if ( sum(abs(grad(x0))) .le. this%tolerance ) exit
            call ls%armijo_condition(alpha, x0, x0_update, loss, grad)
            x0 = x0 - alpha * x0_update
        end do
        optimize_steepest_descent = x0
    end function optimize_steepest_descent


    function optimize_newton_method(this, x_ini, loss, grad, hess)
        implicit none
        class(newton_method)        :: this
        real(kind=8), allocatable   :: optimize_newton_method(:)
        real(kind=8), intent(in)    :: x_ini(:)

        interface
            function loss(x)
                real(kind=8) :: loss
                real(kind=8), intent(in) :: x(:)
            end function loss
        end interface
        interface
            function grad(x)
                real(kind=8), allocatable :: grad(:)
                real(kind=8), intent(in) :: x(:)
            end function grad
        end interface
        interface
            function hess(x)
                real(kind=8), allocatable :: hess(:,:)
                real(kind=8), intent(in) :: x(:)
            end function hess
        end interface

        type(line_search) :: ls

        real(kind=8) :: alpha
        real(kind=8), allocatable :: x0(:), x0_update(:)
        real(kind=8), allocatable :: hess0(:,:), hess0_inv(:,:), grad0(:)
        integer(kind=8) :: iter, n_variables

        n_variables = size(x_ini)
        allocate(x0(n_variables), x0_update(n_variables))
        allocate(hess0_inv(n_variables,n_variables))
        x0 = x_ini

        do iter=1, this%max_iter, 1
            alpha = 1d0
            grad0 = grad(x0)
            if ( sum(abs(grad0)) .le. this%tolerance ) exit
            hess0 = hess(x0)
            call inversion(hess0_inv, hess0, n_variables)
            call multi_mat_vec(hess0, grad0, x0_update, n_variables, n_variables)
            call ls%armijo_condition(alpha, x0, x0_update, loss, grad)
            x0 = x0 - alpha * x0_update
        end do
        optimize_newton_method = x0
    end function optimize_newton_method



end module mod_optimization
