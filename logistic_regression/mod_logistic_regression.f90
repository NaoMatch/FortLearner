module mod_logistic_regression
    use mod_common,         only: ifdealloc, identity
    use mod_random,         only: rand_uniform
    use mod_math,           only: sigmoid
    use mod_metric,         only: metrics
    use mod_hyperparameter, only: hparam_logistic_regression
    use mod_linalg,         only: multi_mat_vec, cholesky_decomposition_modified, inv_unit_lower_matrix, inner_product
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
                        + proba * (1d0-proba) &
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
                    + proba * (1d0-proba) &
                    * data_holder_ptr%x_ptr%x_r8_ptr(i,j)
            end do
            hess(j,n_columns+1) = tmp_hess
            hess(n_columns+1,j) = tmp_hess
        end do

        ! Bias x Bias
        tmp_hess = 0d0
        do i=1, n_samples, 1
            proba = probas(i,1)
            tmp_hess = tmp_hess + proba * (1d0-proba)
        end do
        hess(n_columns+1,n_columns+1) = tmp_hess

        hess = hess / dble(n_samples)
    end subroutine compute_hess


    subroutine fit_logistic_regression(this, data_holder_ptr)
        implicit none
        class(logistic_regression) :: this
        type(data_holder), pointer :: data_holder_ptr
        type(error) :: err
        integer(kind=8) :: n_samples, n_columns
        real(kind=8), allocatable :: probas(:,:), grad(:), hess(:,:), hess_inv(:,:)
        real(kind=8), allocatable :: theta_update(:)
        integer(kind=8) :: iter
        real(kind=8)    :: norm_grad, alpha
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
        allocate(theta_update(n_columns+1))
        allocate(grad(n_columns+1))
        allocate(hess(n_columns+1, n_columns+1))
        allocate(hess_inv(n_columns+1, n_columns+1))

        alpha=.2d0
        do iter=1, this%hparam%max_iteration, 1
            probas = this%predict(data_holder_ptr%x_ptr%x_r8_ptr)
            call this%compute_grad(grad, data_holder_ptr, probas, n_samples, n_columns)
            norm_grad = inner_product(grad, grad, n_columns+1_8)
            if (norm_grad .le. this%hparam%tolerance) return
            call this%compute_hess(hess, data_holder_ptr, probas, n_samples, n_columns)
            call inversion(hess_inv, hess, n_columns+1)
            call multi_mat_vec(hess_inv, grad, theta_update, n_columns+1, n_columns+1)

            this%thetas_    = this%thetas_    - alpha * theta_update(1:n_columns)
            this%intercept_ = this%intercept_ - alpha * theta_update(n_columns+1)
            print*, metric%auc(data_holder_ptr%y_ptr%y_i8_ptr(:,1), probas(:,1)), norm_grad, alpha
        end do
    end subroutine fit_logistic_regression
    

    subroutine inversion(mat_inv, mat, n_columns)
        implicit none
        real(kind=8), intent(inout) :: mat_inv(n_columns,n_columns)
        real(kind=8), intent(in)    :: mat(n_columns,n_columns)
        integer(kind=8)             :: n_columns

        real(kind=8), allocatable :: mat_lower(:,:), diagonals(:)
        real(kind=8), allocatable :: mat_lower_inv(:,:), diagonals_inv(:,:)
        integer(kind=8) :: i

        allocate(mat_lower(n_columns, n_columns))
        allocate(diagonals(n_columns))
        call cholesky_decomposition_modified(mat_lower, diagonals, mat, n_columns)

        ! Compute inverse of unit lower triangular matrix
        allocate(mat_lower_inv(n_columns, n_columns))
        call inv_unit_lower_matrix(mat_lower_inv, mat_lower, n_columns)

        ! Compute inverse of X^T X
        allocate(diagonals_inv(n_columns, n_columns))
        call identity(diagonals_inv, n_columns)
        do i=1, n_columns, 1
            diagonals_inv(i,i) = 1.0d0 / diagonals(i)
        end do
        mat_inv = matmul(matmul(transpose(mat_lower_inv), diagonals_inv), mat_lower_inv)
    end subroutine 


end module mod_logistic_regression
