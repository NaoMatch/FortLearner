module mod_optimization
    use mod_timer
    use mod_common, only: identity, vv2mat, vmv
    use mod_random, only: rand_uniform
    use mod_linalg, only: multi_mat_vec, inner_product, inversion
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

    type, extends(opt_method) :: bfgs
    contains
        procedure :: optimize => optimize_bfgs
    end type bfgs


    interface steepest_descent
        module procedure :: new_steepest_descent
    end interface steepest_descent

    interface newton_method
        module procedure :: new_newton_method
    end interface newton_method

    interface bfgs
        module procedure :: new_bfgs
    end interface bfgs

contains


    subroutine armijo_condition(this, alpha, theta, theta_update, loss_ini, loss, grad_ini, grad)
        implicit none
        class(line_search) :: this
        real(kind=8), intent(inout) :: alpha
        real(kind=8), intent(in)    :: theta(:)
        real(kind=8), intent(in)    :: theta_update(:)
        real(kind=8), intent(in)    :: loss_ini, grad_ini(:)
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
        tmp_1 = loss_ini
        tmp_2 = sum(grad_ini**2d0)

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


    ! subroutine wolfe_condition(this, alpha, theta_update, data_holder_ptr, probas, n_samples, n_columns)
    !     implicit none
    !     class(logistic_regression)  :: this
    !     real(kind=8), intent(inout) :: alpha
    !     real(kind=8), intent(in)    :: theta_update(n_columns+1)
    !     type(data_holder), pointer  :: data_holder_ptr
    !     real(kind=8), intent(inout) :: probas(n_samples,1)
    !     integer(kind=8), intent(in) :: n_samples, n_columns

    !     real(kind=8), parameter :: xi1=0.001d0
    !     real(kind=8), parameter :: xi2=0.9d0
    !     real(kind=8), parameter :: tau=0.6d0
    !     real(kind=8), ALLOCATABLE :: theta_fix(:)
    !     real(kind=8) :: intercept_fix

    !     real(kind=8) :: lhs_a, rhs_a
    !     real(kind=8) :: lhs_w, rhs_w
    !     real(kind=8) :: tmp_1, tmp_2
    !     real(kind=8) :: grad(n_columns+1)

    !     theta_fix = this%thetas_
    !     intercept_fix = this%intercept_
    !     tmp_1 = this%compute_loss(data_holder_ptr, probas, n_samples)

    !     call this%compute_grad(grad, data_holder_ptr, probas, n_samples, n_columns)
    !     tmp_2 = inner_product(grad, theta_update, n_columns+1)

    !     do while (t_)
    !         this%thetas_ = theta_fix - alpha * theta_update(1:n_columns)
    !         this%intercept_ = intercept_fix - alpha * theta_update(n_columns+1)
    !         probas = this%predict(data_holder_ptr%x_ptr%x_r8_ptr)

    !         lhs_a = this%compute_loss(data_holder_ptr, probas, n_samples)
    !         rhs_a = tmp_1 - xi1 * alpha * tmp_2

    !         call this%compute_grad(grad, data_holder_ptr, probas, n_samples, n_columns)
    !         lhs_w = sum(theta_update * grad)
    !         rhs_w = xi2 * tmp_2

    !         if ( all((/lhs_a .gt. rhs_a, lhs_w .lt. rhs_w/)) ) then
    !             alpha = alpha * tau
    !         else
    !             exit
    !         end if
    !     end do
    !     this%thetas_ = theta_fix
    !     this%intercept_ = intercept_fix
    ! end subroutine wolfe_condition


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

    function new_bfgs(max_iter, tolerance, alpha_ini)
        implicit none
        type(bfgs) :: new_bfgs
        integer(kind=8), optional   :: max_iter
        real(kind=8), optional   :: tolerance
        real(kind=8), optional   :: alpha_ini

        if (present(max_iter)) new_bfgs % max_iter = max_iter
        if (present(tolerance)) new_bfgs % tolerance = tolerance
        if (present(alpha_ini)) new_bfgs % alpha_ini = alpha_ini
    end function new_bfgs


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

        real(kind=8) :: alpha, loss_ini
        real(kind=8), allocatable :: x0(:), x0_update(:)
        integer(kind=8) :: iter, n_variables

        n_variables = size(x_ini)

        allocate(x0(n_variables), x0_update(n_variables))
        x0 = x_ini
        do iter=1, this%max_iter, 1
            loss_ini = loss(x0)
            alpha = 1d0
            x0_update = grad(x0)
            if ( sum(abs(grad(x0))) .le. this%tolerance ) exit
            call ls%armijo_condition(alpha, x0, x0_update, loss_ini, loss, x0_update, grad)
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
        integer(kind=8) :: date_value1(8), date_value2(8), tot_grad, tot_hess, tot_inv, tot_armi

        real(kind=8) :: alpha, loss_ini, norm_grad
        real(kind=8), allocatable :: x0(:), x0_update(:)
        real(kind=8), allocatable :: hess0(:,:), hess0_inv(:,:), grad0(:)
        integer(kind=8) :: iter, n_variables

        n_variables = size(x_ini)
        allocate(x0(n_variables), x0_update(n_variables))
        allocate(hess0_inv(n_variables,n_variables))
        x0 = x_ini

        tot_grad = 0
        tot_hess = 0
        tot_inv = 0
        tot_armi = 0

        do iter=1, this%max_iter, 1
            loss_ini = loss(x0)
            alpha = 1d0
            call date_and_time(values=date_value1)
            grad0 = grad(x0)
            call date_and_time(values=date_value2)
            tot_grad = tot_grad + time_diff(date_value1, date_value2)

            norm_grad = inner_product(grad0, grad0, n_variables)
            if ( norm_grad .le. this%tolerance ) exit

            call date_and_time(values=date_value1)
            hess0 = hess(x0)
            call date_and_time(values=date_value2)
            tot_hess = tot_hess + time_diff(date_value1, date_value2)

            call date_and_time(values=date_value1)
            call conjugate_gradient(hess0, x0_update, grad0, n_variables, this%max_iter)
            call date_and_time(values=date_value2)
            ! call date_and_time(values=date_value1)
            ! call inversion(hess0_inv, hess0, n_variables)
            ! call multi_mat_vec(hess0_inv, grad0, x0_update, n_variables, n_variables)
            ! call date_and_time(values=date_value2)
            tot_inv = tot_inv + time_diff(date_value1, date_value2)

            call date_and_time(values=date_value1)
            call ls%armijo_condition(alpha, x0, x0_update, loss_ini, loss, grad0, grad)
            call date_and_time(values=date_value2)
            tot_armi = tot_armi + time_diff(date_value1, date_value2)
            x0 = x0 - alpha * x0_update
            ! print*, "NEW: Grad, Hess, Inv, Armijo: ", iter, tot_grad, tot_hess, tot_inv, tot_armi
        end do
        optimize_newton_method = x0
    end function optimize_newton_method


    function optimize_bfgs(this, x_ini, loss, grad)
        implicit none
        class(bfgs)        :: this
        real(kind=8), allocatable   :: optimize_bfgs(:)
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
        integer(kind=8) :: date_value1(8), date_value2(8), tot_grad, tot_hess, tot_inv, tot_armi

        real(kind=8) :: alpha, loss_ini, norm_grad, denom, denom2
        real(kind=8) :: tmp_ij, tmp_ji, factor
        real(kind=8), allocatable :: x0(:), x0_update(:)
        real(kind=8), allocatable :: hess0(:,:), hess0_inv(:,:), grad0(:), y(:)
        real(kind=8), allocatable :: mat_l(:,:), mat_r(:,:), id(:,:), mat(:,:)
        integer(kind=8) :: iter, n_variables, i, j

        n_variables = size(x_ini)
        allocate(x0(n_variables), x0_update(n_variables))
        allocate(hess0_inv(n_variables,n_variables))
        allocate(id(n_variables,n_variables))
        allocate(mat_l(n_variables,n_variables))
        allocate(mat_r(n_variables,n_variables))
        allocate(mat(n_variables,n_variables))
        allocate(y(n_variables))
        call identity(id, n_variables)
        x0 = x_ini

        tot_grad = 0
        tot_hess = 0
        tot_inv = 0
        tot_armi = 0

        ! do iter=1, this%max_iter, 1
        !     if ( iter .eq. 1_8 ) then
        !         call identity(hess0_inv, n_variables)
        !     else
        !         denom = inner_product(x0_update, y, n_variables)
        !         denom2 = vmv(x0_update, hess0_inv, n_variables)
        !         call vv2mat(y/denom, y, mat_l, n_variables, n_variables)

        !         call vv2mat(x0_update, x0_update, mat_r, n_variables, n_variables)
        !         mat_r = matmul(hess0_inv, matmul(mat_r, hess0_inv))

        !         hess0_inv = hess0_inv + mat_l - mat_r / denom2
        !     end if

        !     loss_ini = loss(x0)
        !     grad0 = grad(x0)

        !     norm_grad = inner_product(grad0, grad0, n_variables)
        !     if ( norm_grad .le. this%tolerance ) exit

        !     call conjugate_gradient(hess0_inv, x0_update, grad0, n_variables, this%max_iter)

        !     alpha = 2d0
        !     call ls%armijo_condition(alpha, x0, x0_update, loss_ini, loss, grad0, grad)
        !     x0_update = - alpha * x0_update
        !     x0 = x0 + x0_update

        !     y = grad(x0) - grad0

        !     print*, "NEW: Grad, Hess, Inv, Armijo: ", iter, tot_grad, tot_hess, tot_inv, tot_armi, norm_grad, alpha
        ! end do
        do iter=1, this%max_iter, 1
            if ( iter .eq. 1_8 ) then
                call identity(hess0_inv, n_variables)
            else
                denom = 1d0/inner_product(x0_update, y, n_variables)
                ! call vv2mat(x0_update, y, mat_l, n_variables, n_variables)
                ! call vv2mat(y, x0_update, mat_r, n_variables, n_variables)
                ! call vv2mat(x0_update, x0_update, mat, n_variables, n_variables)
                ! mat_l = id - mat_l * denom
                ! mat_r = id - mat_r * denom
                ! mat = mat * denom
                ! hess0_inv = mat + matmul(mat_l, matmul(hess0_inv, mat_r))

                factor = (1d0/denom + vmv(y, hess0_inv, n_variables)) * denom**2d0
                call vv2mat(x0_update*factor, x0_update, mat_l, n_variables, n_variables)

                call vv2mat(y, x0_update, mat_r, n_variables, n_variables)
                mat_r = matmul(hess0_inv, mat_r)
                do i=1, n_variables, 1
                    mat_r(i,i) = mat_r(i,i) * 2d0
                end do
                do j=1, n_variables, 1
                    do i=j+1, n_variables
                        tmp_ij = mat_r(i,j)
                        tmp_ji = mat_r(j,i)
                        mat_r(i,j) = tmp_ij + tmp_ji
                        mat_r(j,i) = tmp_ij + tmp_ji
                    end do
                end do
                mat_r = mat_r * denom
                hess0_inv = hess0_inv + mat_l - mat_r
            end if

            loss_ini = loss(x0)
            grad0 = grad(x0)

            norm_grad = inner_product(grad0, grad0, n_variables)
            if ( norm_grad .le. this%tolerance ) exit

            call multi_mat_vec(hess0_inv, grad0, x0_update, n_variables, n_variables)

            alpha = 1d0
            call ls%armijo_condition(alpha, x0, x0_update, loss_ini, loss, grad0, grad)
            x0_update = - alpha * x0_update
            x0 = x0 + x0_update

            y = grad(x0) - grad0

            ! print*, "NEW: Grad, Hess, Inv, Armijo: ", iter, tot_grad, tot_hess, tot_inv, tot_armi, norm_grad
        end do
        optimize_bfgs = x0
    end function optimize_bfgs


    subroutine conjugate_gradient(hess, x0_update, grad, n_variables, max_iteration)
        implicit none
        real(kind=8), intent(in)               :: hess(n_variables,n_variables)
        real(kind=8), intent(inout)            :: x0_update(n_variables)
        real(kind=8), intent(in)               :: grad(n_variables)
        integer(kind=8), intent(in)            :: n_variables
        integer(kind=8), intent(in)            :: max_iteration

        real(kind=8), allocatable :: t(:), r(:), p(:) ! thetas_ and intercept_
        real(kind=8), allocatable :: h_v(:) ! hessian x arbitrary vector
        integer(kind=8) :: iter, i, j
        real(kind=8) :: alpha_i, beta_i, tmp
        real(kind=8) :: r_sq_old, r_sq_new

        allocate(t(n_variables), r(n_variables), p(n_variables), h_v(n_variables))

        call rand_uniform(t, -1d0, 1d0, n_variables)

        do j=1, n_variables, 1
            tmp = 0d0
            do i=1, n_variables, 1
                tmp = tmp + hess(i,j) * t(i)
            end do
            h_v(j) = tmp
        end do

        r = grad - h_v
        p = r
        r_sq_old = inner_product(r, r, n_variables)
        do iter=1, max_iteration, 1
            do j=1, n_variables, 1
                tmp = 0d0
                do i=1, n_variables, 1
                    tmp = tmp + hess(i,j) * p(i)
                end do
                h_v(j) = tmp
            end do
            alpha_i = r_sq_old / inner_product(h_v, p, n_variables)
            t = t + alpha_i * p
            r = r - alpha_i * h_v

            r_sq_new = inner_product(r, r, n_variables)
            beta_i = r_sq_new / r_sq_old
            if ( r_sq_new .le. 1d-6 ) exit
            p = r + alpha_i * p
            r_sq_old = r_sq_new
        end do
        x0_update = t
    end subroutine conjugate_gradient


end module mod_optimization
