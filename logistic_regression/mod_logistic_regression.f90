module mod_logistic_regression
    use mod_const
    use mod_common,         only: ifdealloc, identity
    use mod_random,         only: rand_uniform
    use mod_math,           only: sigmoid
    use mod_metric,         only: metrics
    use mod_hyperparameter, only: hparam_logistic_regression
    use mod_linalg,         only: multi_mat_vec, inner_product
    use mod_error,          only: error
    use mod_data_holder,    only: data_holder
    implicit none

    type logistic_regression
        type(hparam_logistic_regression) :: hparam
        real(kind=8), allocatable :: thetas_(:)
        real(kind=8)              :: intercept_
        integer(kind=8)           :: n_columns_train
    contains
        procedure :: predict => predict_logistic_regression
        procedure :: fit => fit_logistic_regression
        procedure :: compute_loss
        procedure :: compute_grad
        procedure :: compute_hess
        procedure :: compute_hess_x_vector
        procedure :: conjugate_gradient
        procedure :: armijo_condition
        procedure :: wolfe_condition
    end type logistic_regression
    
    interface logistic_regression
        module procedure :: new_logistic_regression
    end interface logistic_regression

contains

    function new_logistic_regression(penalty, lambda, tolerance)
        implicit none
        type(logistic_regression) :: new_logistic_regression
        type(logistic_regression) :: tmp

        character(len=*), optional :: penalty
        real(kind=8), optional :: lambda
        real(kind=8), optional :: tolerance

        character(len=256) :: penalty_list(2)

        tmp%hparam%algo_name = "logistic_regression"

        penalty_list(1) = "l1"
        penalty_list(2) = "l2"

        if ( present(penalty) ) tmp%hparam%penalty = penalty
        if ( present(lambda) )  tmp%hparam%lambda  = lambda
        if ( present(tolerance) )  tmp%hparam%tolerance  = tolerance

        call tmp%hparam%validate_real_range("lambda", tmp%hparam%lambda, 1d-10, huge(0d0))
        call tmp%hparam%validate_real_range("tolerance", tmp%hparam%tolerance, 1d-10, huge(0d0))
        call tmp%hparam%validate_char_list("penalty", tmp%hparam%penalty, penalty_list)
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

        call err%check_number_of_features_mismatch(this%n_columns_train, n_columns, this%hparam%algo_name)

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

        this%n_columns_train = data_holder_ptr%n_columns
        call ifdealloc(this%thetas_)

        n_samples = data_holder_ptr%n_samples
        n_columns = data_holder_ptr%n_columns

        allocate(this%thetas_(n_columns))
        call rand_uniform(this%thetas_,    -1d0, 1d0, n_columns)
        call rand_uniform(this%intercept_, -1d0, 1d0)
        call err%is_binary_labels(data_holder_ptr%y_ptr%y_i8_ptr, n_samples, this%hparam%algo_name)

        allocate(probas(n_samples, 1))
        allocate(p_psq(n_samples, 1))
        allocate(theta_update(n_columns+1))
        allocate(grad(n_columns+1))
        allocate(hess(n_columns+1, n_columns+1))
        allocate(hess_inv(n_columns+1, n_columns+1))
        allocate(tmp(n_columns+1,1))
        allocate(tmp_t(1,n_columns+1))

        do iter=1, this%hparam%max_iteration, 1
            alpha=1d0
            probas = this%predict(data_holder_ptr%x_ptr%x_r8_ptr)
            call this%compute_grad(grad, data_holder_ptr, probas, n_samples, n_columns)
            norm_grad = inner_product(grad, grad, n_columns+1_8)
            ! print*, metric%auc(data_holder_ptr%y_ptr%y_i8_ptr(:,1), probas(:,1)), norm_grad
            if (norm_grad .le. this%hparam%tolerance) return

            do i=1, n_samples, 1
                proba = probas(i,1)
                p_psq(i,1) = proba * (1d0-proba)
            end do
            call this%compute_hess(hess, data_holder_ptr, p_psq, n_samples, n_columns)
            call RANDOM_NUMBER(tmp)
            tmp_t = transpose(tmp)
            if ( all(matmul(tmp_t, matmul(hess, tmp)) .le. 0d0) ) then
                call rand_uniform(this%thetas_,    -1d0, 1d0, n_columns)
                call rand_uniform(this%intercept_, -1d0, 1d0)
                cycle
            end if
            call this%conjugate_gradient(theta_update, data_holder_ptr, grad, hess, p_psq, n_samples, n_columns)
            ! call this%armijo_condition(alpha, theta_update, data_holder_ptr, probas, n_samples, n_columns)
            call this%wolfe_condition(alpha, theta_update, data_holder_ptr, probas, n_samples, n_columns)

            this%thetas_    = this%thetas_    - alpha * theta_update(1:n_columns)
            this%intercept_ = this%intercept_ - alpha * theta_update(n_columns+1)
        end do
    end subroutine fit_logistic_regression


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
