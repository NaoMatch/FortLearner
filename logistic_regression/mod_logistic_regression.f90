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
    use mod_optimization,   only: steepest_descent, newton_method, bfgs
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
        procedure :: fit_newton => fit_newton_logistic_regression
        procedure :: fit_bfgs => fit_bfgs_logistic_regression
        procedure :: loss
        procedure :: grad
        procedure :: hess
    end type logistic_regression
    
    interface logistic_regression
        module procedure :: new_logistic_regression
    end interface logistic_regression


    type(logistic_regression) :: lr_temp
    type(logistic_regression) :: lr_temp2

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


    subroutine fit_newton_logistic_regression(this, dholder)
        implicit none
        class(logistic_regression) :: this
        type(logistic_regression)  :: lr
        type(data_holder), target :: dholder
        type(error) :: err
        integer(kind=8) :: n_samples, n_columns
        real(kind=8), allocatable :: x_ini(:), x_min(:)
        type(steepest_descent) :: steep
        type(newton_method)    :: newton

        lr_temp%hparam = this%hparam
        lr_temp%data_holder_ptr => dholder
        lr_temp%n_columns = dholder%n_columns
        lr_temp%n_samples = dholder%n_samples
        call ifdealloc(lr_temp%thetas_)

        allocate(lr_temp%thetas_(lr_temp%n_columns))
        allocate(x_ini(lr_temp%n_columns+1))
        allocate(x_min(lr_temp%n_columns+1))
        call rand_uniform(lr_temp%thetas_,    -1d0, 1d0, lr_temp%n_columns)
        call rand_uniform(lr_temp%intercept_, -1d0, 1d0)
        call err%is_binary_labels(dholder%y_ptr%y_i8_ptr, lr_temp%n_samples, lr_temp%hparam%algo_name)

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
    end subroutine fit_newton_logistic_regression


    subroutine fit_bfgs_logistic_regression(this, dholder)
        implicit none
        class(logistic_regression) :: this
        type(logistic_regression)  :: lr
        type(data_holder), target :: dholder
        type(data_holder), pointer :: data_holder_ptr
        type(error) :: err
        integer(kind=8) :: n_samples, n_columns
        real(kind=8), allocatable :: x_ini(:), x_min(:)
        type(steepest_descent) :: steep
        type(newton_method)    :: newton
        type(bfgs)    :: bfgs_

        lr_temp2%hparam = this%hparam
        lr_temp2%data_holder_ptr => dholder
        lr_temp2%n_columns = dholder%n_columns
        lr_temp2%n_samples = dholder%n_samples
        call ifdealloc(lr_temp2%thetas_)

        allocate(lr_temp2%thetas_(lr_temp2%n_columns))
        allocate(x_ini(lr_temp2%n_columns+1))
        allocate(x_min(lr_temp2%n_columns+1))
        call rand_uniform(lr_temp2%thetas_,    -1d0, 1d0, lr_temp2%n_columns)
        call rand_uniform(lr_temp2%intercept_, -1d0, 1d0)
        call err%is_binary_labels(dholder%y_ptr%y_i8_ptr, lr_temp2%n_samples, lr_temp2%hparam%algo_name)

        lr_temp2%thetas_ = 1d0
        lr_temp2%intercept_ = 1d0

        allocate(lr_temp2%probas(lr_temp2%n_samples, 1))
        allocate(lr_temp2%p_psq(lr_temp2%n_samples, 1))
        ! newton = newton_method(max_iter=100_8, tolerance=lr_temp2%hparam%tolerance) 
        bfgs_ = bfgs(max_iter=100_8, tolerance=lr_temp2%hparam%tolerance) 
        x_ini(1:lr_temp2%n_columns) = lr_temp2%thetas_
        x_ini(lr_temp2%n_columns+1) = lr_temp2%intercept_

        x_min = bfgs_%optimize(x_ini=x_ini, loss=lr_loss_, grad=lr_grad_)

        this%n_columns = lr_temp2%n_columns
        this%thetas_ = lr_temp2%thetas_
        this%intercept_ = lr_temp2%intercept_
    contains
        function lr_loss_(x)
            real(kind=8) :: lr_loss_
            real(kind=8), intent(in) :: x(:)
            lr_loss_ = lr_temp2%loss(x)
        end function lr_loss_

        function lr_grad_(x)
            real(kind=8), allocatable :: lr_grad_(:)
            real(kind=8), intent(in) :: x(:)
            lr_grad_ = lr_temp2%grad(x)
        end function lr_grad_

        function lr_hess_(x)
            real(kind=8), allocatable :: lr_hess_(:,:)
            real(kind=8), intent(in) :: x(:)
            lr_hess_ = lr_temp2%hess(x)
        end function lr_hess_
    end subroutine fit_bfgs_logistic_regression




end module mod_logistic_regression
