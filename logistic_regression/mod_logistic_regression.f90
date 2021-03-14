module mod_logistic_regression
    use mod_const
    use mod_timer
    use mod_common,         only: ifdealloc, identity
    use mod_random,         only: rand_uniform
    use mod_math,           only: sigmoid
    use mod_metric,         only: metrics
    use mod_hyperparameter, only: hparam_logistic_regression
    use mod_linalg,         only: multi_mat_vec, inner_product, inversion
    use mod_error,          only: error
    use mod_data_holder,    only: data_holder
    use mod_optimization,   only: steepest_descent, newton_method
    implicit none

    type logistic_regression
        type(hparam_logistic_regression) :: hparam
        real(kind=8), allocatable :: thetas_(:)
        real(kind=8)              :: intercept_
        integer(kind=8)           :: n_columns
        integer(kind=8)           :: n_samples
        type(data_holder), pointer :: data_holder_ptr
        real(kind=8), allocatable :: probas(:,:), p_psq(:,:)
    contains
        procedure :: predict => predict_logistic_regression
        procedure :: fit     => fit_logistic_regression
        procedure :: fit_new => fit_new_logistic_regression
        procedure :: compute_loss
        procedure :: loss
        procedure :: compute_grad
        procedure :: grad
        procedure :: compute_hess
        procedure :: hess
        procedure :: compute_hess_x_vector
        procedure :: conjugate_gradient
        procedure :: armijo_condition
        procedure :: wolfe_condition
    end type logistic_regression
    
    interface logistic_regression
        module procedure :: new_logistic_regression
    end interface logistic_regression


    type(logistic_regression) :: lr_temp

contains

    function new_logistic_regression(penalty, lambda, tolerance, max_iteration)
        implicit none
        type(logistic_regression) :: new_logistic_regression
        type(logistic_regression) :: tmp

        character(len=*), optional :: penalty
        real(kind=8), optional :: lambda
        real(kind=8), optional :: tolerance
        integer(kind=8), optional :: max_iteration

        character(len=256) :: penalty_list(2)

        tmp%hparam%algo_name = "logistic_regression"

        penalty_list(1) = "l1"
        penalty_list(2) = "l2"

        if ( present(penalty) ) tmp%hparam%penalty = penalty
        if ( present(lambda) )  tmp%hparam%lambda  = lambda
        if ( present(tolerance) )  tmp%hparam%tolerance  = tolerance
        if ( present(max_iteration) )  tmp%hparam%max_iteration  = max_iteration

        call tmp%hparam%validate_real_range("lambda", tmp%hparam%lambda, 1d-10, huge(0d0))
        call tmp%hparam%validate_real_range("tolerance", tmp%hparam%tolerance, 1d-10, huge(0d0))
        call tmp%hparam%validate_char_list("penalty", tmp%hparam%penalty, penalty_list)
        call tmp%hparam%validate_int_range("max_iteration", tmp%hparam%max_iteration, 1_8, huge(0_8))
        new_logistic_regression = tmp
    end function new_logistic_regression


    function predict_logistic_regression(this, x)
        implicit none
        class(logistic_regression) :: this
        real(kind=8), intent(in)   :: x(:,:)
        real(kind=8), allocatable :: predict_logistic_regression(:,:)
        type(error) :: err

        integer(kind=8) :: x_shape(2), n_samples, n_columns

        x_shape = shape(x)
        n_samples = x_shape(1)
        n_columns = x_shape(2)

        call err%check_number_of_features_mismatch(this%n_columns, n_columns, this%hparam%algo_name)

        allocate(predict_logistic_regression(n_samples, 1))

        call multi_mat_vec(x, this%thetas_, predict_logistic_regression(:,1), n_samples, n_columns)
        predict_logistic_regression = predict_logistic_regression + this%intercept_
        predict_logistic_regression = sigmoid(predict_logistic_regression)
    end function predict_logistic_regression


    function compute_loss(this, data_holder_ptr, probas, n_samples)
        implicit none
        class(logistic_regression)  :: this
        real(kind=8)                :: compute_loss
        type(data_holder), pointer  :: data_holder_ptr
        real(kind=8), intent(in)    :: probas(n_samples, 1_8)
        integer(kind=8), intent(in) :: n_samples

        integer(kind=8) :: i, label
        real(kind=8)    :: proba
        
        compute_loss=0
        do i=1, n_samples
            label = data_holder_ptr%y_ptr%y_i8_ptr(i,1)
            proba = probas(i,1)
            compute_loss = compute_loss - label * log(proba+1d-8) - (1.0d0-label) * log(1.0d0-proba+1d-8)
        end do
        compute_loss = compute_loss / dble(n_samples)
    end function compute_loss

    function loss(this, x0)
        implicit none
        class(logistic_regression)  :: this
        real(kind=8)                :: loss
        real(kind=8), intent(in)    :: x0(:)

        integer(kind=8) :: i, label
        real(kind=8)    :: proba

        this%thetas_ = x0(1:this%n_columns)
        this%intercept_ = x0(this%n_columns+1)
        this%probas = this%predict(this%data_holder_ptr%x_ptr%x_r8_ptr)

        loss=0
        do i=1, this%n_samples
            label = this%data_holder_ptr%y_ptr%y_i8_ptr(i,1)
            proba = this%probas(i,1)
            loss = loss - label * log(proba+1d-8) - (1.0d0-label) * log(1.0d0-proba+1d-8)
        end do
        loss = loss / dble(this%n_samples)
    end function loss


    subroutine compute_grad(this, grad, data_holder_ptr, probas, n_samples, n_columns)
        implicit none
        class(logistic_regression)  :: this
        real(kind=8), intent(inout) :: grad(n_columns+1)
        type(data_holder), pointer  :: data_holder_ptr
        real(kind=8), intent(in)    :: probas(n_samples, 1_8)
        integer(kind=8), intent(in) :: n_samples, n_columns

        integer(kind=8) :: i, j, label
        real(kind=8)    :: tmp_grad, proba

        do j=1, n_columns, 1
            tmp_grad = 0d0
            do i=1, n_samples, 1
                label = data_holder_ptr%y_ptr%y_i8_ptr(i,1)
                proba = probas(i,1)
                tmp_grad = tmp_grad + (proba-label) * data_holder_ptr%x_ptr%x_r8_ptr(i,j)
            end do
            grad(j) = tmp_grad
        end do

        tmp_grad = 0d0
        do i=1, n_samples, 1
            label = data_holder_ptr%y_ptr%y_i8_ptr(i,1)
            proba = probas(i,1)
            tmp_grad = tmp_grad + (proba-label)
        end do
        grad(n_columns+1) = tmp_grad
        grad = grad / dble(n_samples)
    end subroutine compute_grad

    function grad(this, x0)
        implicit none
        class(logistic_regression)  :: this
        real(kind=8), allocatable   :: grad(:)
        real(kind=8), intent(in)    :: x0(:)

        integer(kind=8) :: i, j, label
        real(kind=8)    :: tmp_grad, proba

        allocate(grad(this%n_columns+1))

        ! do i=1, this%n_samples, 1
        !     label = this%data_holder_ptr%y_ptr%y_i8_ptr(i,1)
        !     proba = this%probas(i,1)
        !     this%p_psq(i,1) = proba - label
        ! end do

        do j=1, this%n_columns, 1
            tmp_grad = 0d0
            do i=1, this%n_samples, 1
                label = this%data_holder_ptr%y_ptr%y_i8_ptr(i,1)
                proba = this%probas(i,1)
                tmp_grad = tmp_grad + (proba-label) * this%data_holder_ptr%x_ptr%x_r8_ptr(i,j)
                ! proba = this%p_psq(i,1)
                ! tmp_grad = tmp_grad + proba * this%data_holder_ptr%x_ptr%x_r8_ptr(i,j)
            end do
            grad(j) = tmp_grad
        end do

        tmp_grad = 0d0
        do i=1, this%n_samples, 1
            label = this%data_holder_ptr%y_ptr%y_i8_ptr(i,1)
            proba = this%probas(i,1)
            tmp_grad = tmp_grad + (proba-label)
            ! tmp_grad = tmp_grad + this%p_psq(i,1)
        end do
        grad(this%n_columns+1) = tmp_grad
        grad = grad / dble(this%n_samples)
    end function grad


    subroutine compute_hess(this, hess, data_holder_ptr, probas, n_samples, n_columns)
        implicit none
        class(logistic_regression)  :: this
        real(kind=8), intent(inout) :: hess(n_columns+1, n_columns+1)
        type(data_holder), pointer  :: data_holder_ptr
        real(kind=8), intent(in)    :: probas(n_samples, 1_8)
        integer(kind=8), intent(in) :: n_samples, n_columns

        integer(kind=8) :: i, j, k, label
        real(kind=8)    :: tmp_hess, proba

        ! Wights x Weights
        do k=1, n_columns, 1
            do j=k, n_columns, 1
                tmp_hess = 0d0
                do i=1, n_samples, 1
                    proba = probas(i,1)
                    tmp_hess = tmp_hess &
                        + proba &
                        * data_holder_ptr%x_ptr%x_r8_ptr(i,j) * data_holder_ptr%x_ptr%x_r8_ptr(i,k)
                end do
                hess(j,k) = tmp_hess
                hess(k,j) = tmp_hess
            end do
        end do

        ! Wights x Bias
        do j=1, n_columns, 1
            tmp_hess = 0d0
            do i=1, n_samples, 1
                proba = probas(i,1)
                tmp_hess = tmp_hess &
                    + proba * data_holder_ptr%x_ptr%x_r8_ptr(i,j)
            end do
            hess(j,n_columns+1) = tmp_hess
            hess(n_columns+1,j) = tmp_hess
        end do

        ! Bias x Bias
        tmp_hess = 0d0
        do i=1, n_samples, 1
            proba = probas(i,1)
            tmp_hess = tmp_hess + proba
        end do
        hess(n_columns+1,n_columns+1) = tmp_hess

        hess = hess / dble(n_samples)
    end subroutine compute_hess

    function hess(this, x0)
        implicit none
        class(logistic_regression)  :: this
        real(kind=8), allocatable   :: hess(:,:)
        real(kind=8), intent(in)    :: x0(:)

        integer(kind=8) :: i, j, k, label
        real(kind=8)    :: tmp_hess, proba

        allocate(hess(this%n_columns+1, this%n_columns+1))

        do i=1, this%n_samples, 1
            proba = this%probas(i,1)
            this%p_psq(i,1) = proba*(1d0 - proba)
        end do

        ! Wights x Weights
        do k=1, this%n_columns, 1
            do j=k, this%n_columns, 1
                tmp_hess = 0d0
                do i=1, this%n_samples, 1
                    proba = this%p_psq(i,1)
                    tmp_hess = tmp_hess &
                        + proba &
                        * this%data_holder_ptr%x_ptr%x_r8_ptr(i,j) * this%data_holder_ptr%x_ptr%x_r8_ptr(i,k)
                end do
                hess(j,k) = tmp_hess
                hess(k,j) = tmp_hess
            end do
        end do

        ! Wights x Bias
        do j=1, this%n_columns, 1
            tmp_hess = 0d0
            do i=1, this%n_samples, 1
                proba = this%p_psq(i,1)
                tmp_hess = tmp_hess &
                    + proba * this%data_holder_ptr%x_ptr%x_r8_ptr(i,j)
            end do
            hess(j,this%n_columns+1) = tmp_hess
            hess(this%n_columns+1,j) = tmp_hess
        end do

        ! Bias x Bias
        tmp_hess = 0d0
        do i=1, this%n_samples, 1
            proba = this%p_psq(i,1)
            tmp_hess = tmp_hess + proba
        end do
        hess(this%n_columns+1,this%n_columns+1) = tmp_hess

        hess = hess / dble(this%n_samples)
    end function hess


    subroutine compute_hess_x_vector(this, hess_x_vector, vector, val, data_holder_ptr, probas, n_samples, n_columns)
        implicit none
        class(logistic_regression)  :: this
        real(kind=8), intent(inout) :: hess_x_vector(n_columns+1)
        real(kind=8), intent(inout) :: vector(n_columns), val
        type(data_holder), pointer  :: data_holder_ptr
        real(kind=8), intent(in)    :: probas(n_samples, 1_8)
        integer(kind=8), intent(in) :: n_samples, n_columns

        integer(kind=8) :: i, j, k, label
        real(kind=8)    :: tmp_hess_x_vector, proba
        real(kind=8), ALLOCATABLE :: mat_vec(:)

        allocate(mat_vec(n_samples))
        call multi_mat_vec(data_holder_ptr%x_ptr%x_r8_ptr, vector, mat_vec, n_samples, n_columns)
        do i=1, n_samples, 1
            mat_vec(i) = mat_vec(i) + val
        end do

        do j=1, n_columns, 1
            tmp_hess_x_vector = 0d0
            do i=1, n_samples, 1
                tmp_hess_x_vector = tmp_hess_x_vector & 
                    + data_holder_ptr%x_ptr%x_r8_ptr(i,j) * probas(i,1) * mat_vec(i)
            end do
            hess_x_vector(j) = tmp_hess_x_vector
        end do

        tmp_hess_x_vector = 0d0
        do i=1, n_samples, 1
            tmp_hess_x_vector = tmp_hess_x_vector + probas(i,1) * mat_vec(i)
        end do
        hess_x_vector(n_columns+1) = tmp_hess_x_vector

        hess_x_vector = hess_x_vector / dble(n_samples)
    end subroutine compute_hess_x_vector


    subroutine fit_logistic_regression(this, data_holder_ptr)
        implicit none
        class(logistic_regression) :: this
        type(data_holder), pointer :: data_holder_ptr
        type(error) :: err
        integer(kind=8) :: n_samples, n_columns
        real(kind=8), allocatable :: probas(:,:), grad(:), hess(:,:), hess_inv(:,:), p_psq(:,:)
        real(kind=8), allocatable :: theta_update(:), tmp(:,:), tmp_T(:,:)
        integer(kind=8) :: iter, i
        real(kind=8)    :: norm_grad, alpha, proba
        type(metrics)   :: metric

        integer(kind=8) :: date_value1(8), date_value2(8), tot_grad, tot_hess, tot_inv, tot_armi

        print*, "FIT LOGISTIC REGRESSSION"

        this%data_holder_ptr => data_holder_ptr
        this%n_columns = data_holder_ptr%n_columns
        this%n_samples = data_holder_ptr%n_columns
        call ifdealloc(this%thetas_)

        n_samples = data_holder_ptr%n_samples
        n_columns = data_holder_ptr%n_columns

        allocate(this%thetas_(n_columns))
        call rand_uniform(this%thetas_,    -1d0, 1d0, n_columns)
        call rand_uniform(this%intercept_, -1d0, 1d0)
        call err%is_binary_labels(data_holder_ptr%y_ptr%y_i8_ptr, n_samples, this%hparam%algo_name)

        this%thetas_ = 1d0
        this%intercept_ = 1d0

        allocate(probas(n_samples, 1))
        allocate(p_psq(n_samples, 1))
        allocate(theta_update(n_columns+1))
        allocate(grad(n_columns+1))
        allocate(hess(n_columns+1, n_columns+1))
        allocate(hess_inv(n_columns+1, n_columns+1))
        allocate(tmp(n_columns+1,1))
        allocate(tmp_t(1,n_columns+1))

        tot_grad = 0
        tot_hess = 0
        tot_inv = 0
        tot_armi = 0

        do iter=1, this%hparam%max_iteration, 1
            alpha=1d0
            probas = this%predict(data_holder_ptr%x_ptr%x_r8_ptr)
            call date_and_time(values=date_value1)
            call this%compute_grad(grad, data_holder_ptr, probas, n_samples, n_columns)
            call date_and_time(values=date_value2)
            tot_grad = tot_grad + time_diff(date_value1, date_value2)

            norm_grad = inner_product(grad, grad, n_columns+1_8)
            ! print*, metric%auc(data_holder_ptr%y_ptr%y_i8_ptr(:,1), probas(:,1)), norm_grad
            if (norm_grad .le. this%hparam%tolerance) return

            do i=1, n_samples, 1
                proba = probas(i,1)
                p_psq(i,1) = proba * (1d0-proba)
            end do
            call date_and_time(values=date_value1)
            call this%compute_hess(hess, data_holder_ptr, p_psq, n_samples, n_columns)
            call date_and_time(values=date_value2)
            tot_hess = tot_hess + time_diff(date_value1, date_value2)

            call RANDOM_NUMBER(tmp)
            tmp_t = transpose(tmp)
            if ( all(matmul(tmp_t, matmul(hess, tmp)) .le. 0d0) ) then
                call rand_uniform(this%thetas_,    -1d0, 1d0, n_columns)
                call rand_uniform(this%intercept_, -1d0, 1d0)
                cycle
            end if
            ! call this%conjugate_gradient(theta_update, data_holder_ptr, grad, hess, p_psq, n_samples, n_columns)
            call date_and_time(values=date_value1)
            call inversion(hess_inv, hess, n_columns+1_8)
            call date_and_time(values=date_value2)
            tot_inv = tot_inv + time_diff(date_value1, date_value2)

            call multi_mat_vec(hess_inv, grad, theta_update, n_columns+1_8, n_columns+1_8)
            call date_and_time(values=date_value1)
            call this%armijo_condition(alpha, theta_update, data_holder_ptr, probas, n_samples, n_columns)
            call date_and_time(values=date_value2)
            tot_armi = tot_armi + time_diff(date_value1, date_value2)
            ! call this%wolfe_condition(alpha, theta_update, data_holder_ptr, probas, n_samples, n_columns)

            this%thetas_    = this%thetas_    - alpha * theta_update(1:n_columns)
            this%intercept_ = this%intercept_ - alpha * theta_update(n_columns+1)
            print*, "OLD: Grad, Hess, Inv, Armijo: ", iter, tot_grad, tot_hess, tot_inv, tot_armi
        end do
    end subroutine fit_logistic_regression

    subroutine fit_new_logistic_regression(this, data_holder_ptr)
        implicit none
        class(logistic_regression) :: this
        type(logistic_regression)  :: lr
        type(data_holder), pointer :: data_holder_ptr
        type(error) :: err
        integer(kind=8) :: n_samples, n_columns
        real(kind=8), allocatable :: x_ini(:), x_min(:)
        type(steepest_descent) :: steep
        type(newton_method)    :: newton

        lr_temp%hparam = this%hparam
        lr_temp%data_holder_ptr => data_holder_ptr
        lr_temp%n_columns = data_holder_ptr%n_columns
        lr_temp%n_samples = data_holder_ptr%n_samples
        call ifdealloc(lr_temp%thetas_)

        allocate(lr_temp%thetas_(lr_temp%n_columns))
        allocate(x_ini(lr_temp%n_columns+1))
        allocate(x_min(lr_temp%n_columns+1))
        call rand_uniform(lr_temp%thetas_,    -1d0, 1d0, lr_temp%n_columns)
        call rand_uniform(lr_temp%intercept_, -1d0, 1d0)
        call err%is_binary_labels(data_holder_ptr%y_ptr%y_i8_ptr, lr_temp%n_samples, lr_temp%hparam%algo_name)

        lr_temp%thetas_ = 1d0
        lr_temp%intercept_ = 1d0

        allocate(lr_temp%probas(lr_temp%n_samples, 1))
        allocate(lr_temp%p_psq(lr_temp%n_samples, 1))
        newton = newton_method(max_iter=100_8, tolerance=lr_temp%hparam%tolerance) 
        x_ini(1:lr_temp%n_columns) = lr_temp%thetas_
        x_ini(lr_temp%n_columns+1) = lr_temp%intercept_

        x_min = newton%optimize(x_ini=x_ini, loss=lr_loss_, grad=lr_grad_, hess=lr_hess_)

        this%n_columns = lr_temp%n_columns
        this%thetas_ = lr_temp%thetas_
        this%intercept_ = lr_temp%intercept_
    contains
        function lr_loss_(x)
            real(kind=8) :: lr_loss_
            real(kind=8), intent(in) :: x(:)
            lr_loss_ = lr_temp%loss(x)
        end function lr_loss_

        function lr_grad_(x)
            real(kind=8), allocatable :: lr_grad_(:)
            real(kind=8), intent(in) :: x(:)
            lr_grad_ = lr_temp%grad(x)
        end function lr_grad_

        function lr_hess_(x)
            real(kind=8), allocatable :: lr_hess_(:,:)
            real(kind=8), intent(in) :: x(:)
            lr_hess_ = lr_temp%hess(x)
        end function lr_hess_
    end subroutine fit_new_logistic_regression


    subroutine conjugate_gradient(this, theta_update, data_holder_ptr, grad, hess, probas, n_samples, n_columns)
        implicit none
        class(logistic_regression)             :: this
        real(kind=8), intent(inout)            :: theta_update(n_columns+1)
        type(data_holder), pointer, intent(in) :: data_holder_ptr
        real(kind=8), intent(in)               :: grad(n_columns+1)
        real(kind=8), intent(in)               :: hess(n_columns+1,n_columns+1)
        real(kind=8), intent(in)               :: probas(n_samples,1)
        integer(kind=8), intent(in)            :: n_samples, n_columns

        real(kind=8), allocatable :: t(:), r(:), p(:) ! thetas_ and intercept_
        real(kind=8), allocatable :: h_v(:) ! hessian x arbitrary vector
        integer(kind=8) :: iter, i, j
        real(kind=8) :: alpha_i, beta_i, tmp
        real(kind=8) :: r_sq_old, r_sq_new

        allocate(t(n_columns+1), r(n_columns+1), p(n_columns+1))
        allocate(h_v(n_columns+1))

        call rand_uniform(t, -1d0, 1d0, n_columns)

        do j=1, n_columns+1, 1
            tmp = 0d0
            do i=1, n_columns+1, 1
                tmp = tmp + hess(i,j) * t(i)
            end do
            h_v(j) = tmp
        end do
        r = grad - h_v
        p = r
        r_sq_old = inner_product(r, r, n_columns+1_8)
        do iter=1, this%hparam%max_iteration, 1
            do j=1, n_columns+1, 1
                tmp = 0d0
                do i=1, n_columns+1, 1
                    tmp = tmp + hess(i,j) * p(i)
                end do
                h_v(j) = tmp
            end do
            alpha_i = r_sq_old / inner_product(h_v, p, n_columns+1_8)
            t = t + alpha_i * p
            r = r - alpha_i * h_v

            r_sq_new = inner_product(r, r, n_columns+1_8)
            beta_i = r_sq_new / r_sq_old
            if ( r_sq_new .le. 1d-6 ) exit
            p = r + alpha_i * p
            r_sq_old = r_sq_new
        end do
        theta_update = t
    end subroutine conjugate_gradient


    subroutine armijo_condition(this, alpha, theta_update, data_holder_ptr, probas, n_samples, n_columns)
        implicit none
        class(logistic_regression)  :: this
        real(kind=8), intent(inout) :: alpha
        real(kind=8), intent(in)    :: theta_update(n_columns+1)
        type(data_holder), pointer  :: data_holder_ptr
        real(kind=8), intent(inout) :: probas(n_samples,1)
        integer(kind=8), intent(in) :: n_samples, n_columns

        real(kind=8), parameter :: xi1=0.001d0
        real(kind=8), parameter :: xi2=0.9d0
        real(kind=8), parameter :: tau=0.6d0
        real(kind=8), ALLOCATABLE :: theta_fix(:)
        real(kind=8) :: intercept_fix

        real(kind=8) :: lhs_a, rhs_a
        real(kind=8) :: tmp_1, tmp_2
        real(kind=8) :: grad(n_columns+1)

        theta_fix = this%thetas_
        intercept_fix = this%intercept_
        tmp_1 = this%compute_loss(data_holder_ptr, probas, n_samples)

        call this%compute_grad(grad, data_holder_ptr, probas, n_samples, n_columns)
        tmp_2 = inner_product(grad, theta_update, n_columns+1)

        do while (t_)
            this%thetas_ = theta_fix - alpha * theta_update(1:n_columns)
            this%intercept_ = intercept_fix - alpha * theta_update(n_columns+1)

            probas = this%predict(data_holder_ptr%x_ptr%x_r8_ptr)
            lhs_a = this%compute_loss(data_holder_ptr, probas, n_samples)
            rhs_a = tmp_1 - xi1 * alpha * tmp_2

            if ( lhs_a .gt. rhs_a ) then
                alpha = alpha * tau
            else
                exit
            end if
        end do
        this%thetas_ = theta_fix
        this%intercept_ = intercept_fix
    end subroutine armijo_condition


    subroutine wolfe_condition(this, alpha, theta_update, data_holder_ptr, probas, n_samples, n_columns)
        implicit none
        class(logistic_regression)  :: this
        real(kind=8), intent(inout) :: alpha
        real(kind=8), intent(in)    :: theta_update(n_columns+1)
        type(data_holder), pointer  :: data_holder_ptr
        real(kind=8), intent(inout) :: probas(n_samples,1)
        integer(kind=8), intent(in) :: n_samples, n_columns

        real(kind=8), parameter :: xi1=0.001d0
        real(kind=8), parameter :: xi2=0.9d0
        real(kind=8), parameter :: tau=0.6d0
        real(kind=8), ALLOCATABLE :: theta_fix(:)
        real(kind=8) :: intercept_fix

        real(kind=8) :: lhs_a, rhs_a
        real(kind=8) :: lhs_w, rhs_w
        real(kind=8) :: tmp_1, tmp_2
        real(kind=8) :: grad(n_columns+1)

        theta_fix = this%thetas_
        intercept_fix = this%intercept_
        tmp_1 = this%compute_loss(data_holder_ptr, probas, n_samples)

        call this%compute_grad(grad, data_holder_ptr, probas, n_samples, n_columns)
        tmp_2 = inner_product(grad, theta_update, n_columns+1)

        do while (t_)
            this%thetas_ = theta_fix - alpha * theta_update(1:n_columns)
            this%intercept_ = intercept_fix - alpha * theta_update(n_columns+1)
            probas = this%predict(data_holder_ptr%x_ptr%x_r8_ptr)

            lhs_a = this%compute_loss(data_holder_ptr, probas, n_samples)
            rhs_a = tmp_1 - xi1 * alpha * tmp_2

            call this%compute_grad(grad, data_holder_ptr, probas, n_samples, n_columns)
            lhs_w = sum(theta_update * grad)
            rhs_w = xi2 * tmp_2

            if ( all((/lhs_a .gt. rhs_a, lhs_w .lt. rhs_w/)) ) then
                alpha = alpha * tau
            else
                exit
            end if
        end do
        this%thetas_ = theta_fix
        this%intercept_ = intercept_fix
    end subroutine wolfe_condition


end module mod_logistic_regression
