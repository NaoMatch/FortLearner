!> A module for linear regression, lasso regression, ridge regression.
module mod_linear_regression
    use mod_common
    use mod_const
    use mod_error
    use mod_stats
    use mod_hyperparameter
    use mod_data_holder
    use mod_linalg
    implicit none

    !> Base Linear Regression Object
    type base_linear_regressor
        character(len=256) :: algo_name
        logical(kind=4) :: is_simple
        logical(kind=4) :: is_trained
        real(kind=8)    :: coef_               ! For Simple Linear Regression
        real(kind=8), allocatable :: coefs_(:) ! For Multiple Linear Regression
        real(kind=8) :: intercept_             ! Intercept
        integer(kind=8) :: n_columns=-1
    contains
        procedure :: predict
        procedure :: predict_simple
        procedure :: predict_multiple
        procedure :: dump => dump_model
        procedure :: load => load_model
    end type


    !> Linear Regression Object
    type, extends(base_linear_regressor) ::  linear_regression
        type(hparam_linear_regression) :: hparam 
    contains
        procedure :: fit => fit_linear_regression
        procedure :: fit_linear_regression_simple
        procedure :: fit_linear_regression_multiple
    end type linear_regression

    !> Override linear_regression
    interface linear_regression
        procedure :: new_linear_regression
    end interface linear_regression


    !> Ridge Regression Object
    type, extends(base_linear_regressor) ::  ridge_regression
        type(hparam_ridge_regression) :: hparam 
    contains
        procedure :: fit => fit_ridge_regression
        procedure :: fit_ridge_regression_simple
        procedure :: fit_ridge_regression_multiple
    end type ridge_regression

    !> Override ridge regression
    interface ridge_regression
        procedure :: new_ridge_regression
    end interface ridge_regression


    !> Ridge Regression Object
    type, extends(base_linear_regressor) ::  lasso_regression
        type(hparam_lasso_regression) :: hparam 
    contains
        procedure :: fit => fit_lasso_regression
    end type lasso_regression


    !> Override lasso regression
    interface lasso_regression
        procedure :: new_lasso_regression
    end interface lasso_regression

contains


    !> A function to override 'linear_regression' object.
    !! \param fit_intercept whether to calculate intercept, default .true..
    function new_linear_regression(fit_intercept)
        implicit none
        logical(kind=4), optional :: fit_intercept
        type(linear_regression)   :: new_linear_regression
        new_linear_regression%is_simple = f_
        new_linear_regression%is_trained = f_

        new_linear_regression%algo_name = "linear_regression"
        if ( present(fit_intercept) ) new_linear_regression%hparam%fit_intercept = fit_intercept
    end function new_linear_regression


    !> A function to override 'ridge_regression' object.
    !! \param fit_intercept whether to calculate intercept, default .true..
    function new_ridge_regression(lambda, fit_intercept)
        implicit none
        real(kind=8), optional    :: lambda
        logical(kind=4), optional :: fit_intercept
        type(ridge_regression)    :: new_ridge_regression, tmp
        tmp%is_simple = f_
        tmp%is_trained = f_
        if ( present(lambda) ) tmp%hparam%lambda_l2 = lambda
        if ( present(fit_intercept) ) tmp%hparam%fit_intercept = fit_intercept

        call tmp%hparam%validate_real_range("lambda", tmp%hparam%lambda_l2, 1d-10, huge(0d0))
        new_ridge_regression = tmp
    end function new_ridge_regression


    !> A function to override 'lasso_regression' object.
    !! \param fit_intercept whether to calculate intercept, default .true..
    function new_lasso_regression(lambda, fit_intercept, max_iter)
        implicit none
        real(kind=8), optional    :: lambda
        logical(kind=4), optional :: fit_intercept
        integer(kind=8), optional :: max_iter
        type(lasso_regression)    :: new_lasso_regression, tmp
        tmp%is_simple = f_
        tmp%is_trained = f_
        if ( present(lambda) ) tmp%hparam%lambda_l1 = lambda
        if ( present(fit_intercept) ) tmp%hparam%fit_intercept = fit_intercept
        if ( present(max_iter) ) tmp%hparam%max_iter = max_iter

        call tmp%hparam%validate_real_range("lambda", tmp%hparam%lambda_l1, 1d-10, huge(0d0))
        call tmp%hparam%validate_int_range("max_iter", tmp%hparam%max_iter, 1_8,   huge(0_8))
        new_lasso_regression = tmp
    end function new_lasso_regression


    !> A subroutine to fit linear regression.
    !> If the number of columns is '1', call 'fit_linear_regression_simple'.
    !> If the number of columns is greater than '1', call 'fit_linear_regression_multiple'.
    !! \param data_holder_ptr a pointer of data_holder
    subroutine fit_linear_regression(this, dholder)
        implicit none
        class(linear_regression)   :: this
        type(data_holder), target :: dholder
        type(data_holder), pointer :: data_holder_ptr
        integer(kind=8)            :: one_i8=1_8
        type(error)                :: err
        data_holder_ptr => dholder
        call err%only_accept_Nx1_matrix(data_holder_ptr%y_shape, "y", "linear_regression")

        if ( data_holder_ptr%n_columns .eq. one_i8 ) then
            call this%fit_linear_regression_simple(data_holder_ptr)
        else
            call this%fit_linear_regression_multiple(data_holder_ptr)
        end if
    end subroutine fit_linear_regression


    !> A subroutine to fit simple linear regression model.
    !! \param data_holder_ptr a pointer of data_holder
    subroutine fit_linear_regression_simple(this, data_holder_ptr)
        implicit none
        class(linear_regression) :: this
        type(data_holder), pointer :: data_holder_ptr

        integer(kind=8) :: n_samples, n_columns, one_i8=1_8, n
        real(kind=8)    :: covar_xy
        real(kind=8)    :: var_x, mean_x, mean_y
        real(kind=8), ALLOCATABLE :: x_copy(:), y_copy(:)

        n_samples = data_holder_ptr%n_samples
        n_columns = data_holder_ptr%n_columns

        allocate(x_copy(n_samples))
        allocate(y_copy(n_samples))
        do n=1, n_samples, 1
            x_copy(n) = data_holder_ptr%x_ptr%x_r8_ptr(n,1)
            y_copy(n) = data_holder_ptr%y_ptr%y_r8_ptr(n,1)
        end do

        mean_x = mean(x_copy, n_samples)
        mean_y = mean(y_copy, n_samples)
        var_x = variance(x_copy, n_samples, mean_x)
        covar_xy = covariance(x_copy, y_copy, n_samples, mean_x, mean_y)

        if ( this%hparam%fit_intercept ) then
            this%coef_ = covar_xy / var_x
            this%intercept_ = mean_y - this%coef_ * mean_x
        else
            covar_xy = covar_xy + mean_x * mean_y
            var_x = var_x + mean_x * mean_x
            this%coef_ = covar_xy / var_x
            this%intercept_ = 0d0
        end if

        this%is_simple = t_
        this%is_trained = t_
    end subroutine fit_linear_regression_simple


    !> A subroutine to fit multiple linear regression model.
    !! \param data_holder_ptr a pointer of data_holder
    subroutine fit_linear_regression_multiple(this, data_holder_ptr)
        implicit none
        class(linear_regression) :: this
        type(data_holder), pointer :: data_holder_ptr

        integer(kind=8)           :: n_samples, n_columns, n_columns_with
        real(kind=8), allocatable :: mat_sq(:,:), mat_sq_inv(:,:)
        real(kind=8), allocatable :: xy(:)
        real(kind=8), allocatable :: mat_lower(:,:), diagonals(:)
        real(kind=8), allocatable :: mat_lower_inv(:,:), diagonals_inv(:,:)
        integer(kind=8)           :: i, j
        real(kind=8)              :: tmp_sum
        real(kind=8), allocatable :: weights(:) ! coefficients and intercept
        type(error)               :: err

        if ( allocated(this%coefs_) ) deallocate(this%coefs_)

        n_samples = data_holder_ptr%n_samples
        n_columns = data_holder_ptr%n_columns

        ! Compute X^T X
        call mattxmat(mat_sq, data_holder_ptr%x_ptr%x_r8_ptr, &
            n_samples, n_columns, with_intercept=this%hparam%fit_intercept)

        ! Cholesky decomposition of X^T X
        n_columns_with = n_columns
        if ( this%hparam%fit_intercept ) n_columns_with = n_columns_with + 1
        allocate(mat_lower(n_columns_with, n_columns_with))
        allocate(diagonals(n_columns_with))
        call cholesky_decomposition_modified(mat_lower, diagonals, mat_sq, n_columns_with)

        ! Compute inverse of unit lower triangular matrix
        allocate(mat_lower_inv(n_columns_with, n_columns_with))
        call inv_unit_lower_matrix(mat_lower_inv, mat_lower, n_columns_with)

        ! Compute inverse of X^T X
        allocate(mat_sq_inv(n_columns_with, n_columns_with))
        allocate(diagonals_inv(n_columns_with, n_columns_with))
        call identity(diagonals_inv, n_columns_with)
        do i=1, n_columns_with, 1
            diagonals_inv(i,i) = 1.0d0 / diagonals(i)
        end do
        mat_sq_inv = matmul(matmul(transpose(mat_lower_inv), diagonals_inv), mat_lower_inv)

        ! Compute X^T Y
        allocate(xy(n_columns_with))
        xy = 0.0d0
        do j=1, n_columns, 1
            tmp_sum = 0.0d0
            do i=1, n_samples, 1
                tmp_sum = tmp_sum &
                    + data_holder_ptr%x_ptr%x_r8_ptr(i,j) & 
                    * data_holder_ptr%y_ptr%y_r8_ptr(i,1)
            end do
            xy(j) = tmp_sum
        end do
        if ( this%hparam%fit_intercept ) xy(n_columns_with) = sum(data_holder_ptr%y_ptr%y_r8_ptr)

        ! Compute weights 
        allocate(weights(n_columns_with))
        do j=1, n_columns_with, 1
            tmp_sum = 0.0d0
            do i=1, n_columns_with, 1
                tmp_sum = tmp_sum + mat_sq_inv(i,j) * xy(i)
            end do
            weights(j) = tmp_sum
        end do

        ! Store Training Results
        allocate(this%coefs_(n_columns))
        this%coefs_ = weights(1:n_columns)
        this%intercept_ = 0d0
        if ( this%hparam%fit_intercept ) this%intercept_ = weights(n_columns_with)

        this%n_columns = n_columns
        this%is_trained = t_
        this%is_simple = f_
    end subroutine fit_linear_regression_multiple


    !> A subroutine to fit ridge regression model.
    !> If the number of columns is '1', call 'fit_ridge_regression_simple'.
    !> If the number of columns is greater than '1', call 'fit_ridge_regression_multiple'.
    !! \param data_holder_ptr a pointer of data_holder
    subroutine fit_ridge_regression(this, dholder)
        implicit none
        class(ridge_regression)    :: this
        type(data_holder), target :: dholder
        type(data_holder), pointer :: data_holder_ptr
        integer(kind=8)            :: one_i8=1_8
        type(error)                :: err
        data_holder_ptr => dholder
        call err%only_accept_Nx1_matrix(data_holder_ptr%y_shape, "y", "linear_regression")

        if ( data_holder_ptr%n_columns .eq. one_i8 ) then
            call this%fit_ridge_regression_simple(data_holder_ptr)
        else
            call this%fit_ridge_regression_multiple(data_holder_ptr)
        end if
    end subroutine fit_ridge_regression


    !> A subroutine to fit simple ridge regression model.
    !! \param data_holder_ptr a pointer of data_holder
    subroutine fit_ridge_regression_simple(this, data_holder_ptr)
        implicit none
        class(ridge_regression)    :: this
        type(data_holder), pointer :: data_holder_ptr

        integer(kind=8) :: n_samples, n
        real(kind=8) :: covar_xy, var_x
        real(kind=8) :: mean_x, mean_y
        real(kind=8), ALLOCATABLE :: x_copy(:), y_copy(:)

        n_samples = data_holder_ptr%n_samples
        allocate(x_copy(n_samples), y_copy(n_samples))
        do n=1, n_samples, 1
            x_copy(n) = data_holder_ptr%x_ptr%x_r8_ptr(n,1)
            y_copy(n) = data_holder_ptr%y_ptr%y_r8_ptr(n,1)
        end do

        mean_x = mean(x_copy, n_samples)
        mean_y = mean(y_copy, n_samples)
        covar_xy = covariance(y_copy, y_copy, n_samples, mean_x, mean_y)
        var_x = variance(y_copy, n_samples, mean_x)

        if ( this%hparam%fit_intercept ) then
            this%coef_ = covar_xy / (var_x+this%hparam%lambda_l2/dble(n_samples))
            this%intercept_ = mean_y - this%coef_ * mean_x
        else
            covar_xy = covar_xy + mean_x * mean_y
            var_x = var_x + mean_x * mean_x
            this%coef_ = covar_xy / (var_x+this%hparam%lambda_l2/dble(n_samples))
            this%intercept_ = 0d0
        end if

        this%is_simple = t_
        this%is_trained = t_
    end subroutine fit_ridge_regression_simple


    !> A subroutine to fit multiple linear regression model with L2 regularization.
    !! \param data_holder_ptr a pointer of data_holder
    subroutine fit_ridge_regression_multiple(this, data_holder_ptr)
        implicit none
        class(ridge_regression)    :: this
        type(data_holder), pointer :: data_holder_ptr

        integer(kind=8)           :: n_samples, n_columns, n_columns_with
        real(kind=8), allocatable :: mat_sq(:,:), mat_sq_inv(:,:)
        real(kind=8), allocatable :: xy(:)
        real(kind=8), allocatable :: mat_lower(:,:), diagonals(:)
        real(kind=8), allocatable :: mat_lower_inv(:,:), diagonals_inv(:,:)
        integer(kind=8)           :: i, j
        real(kind=8)              :: tmp_sum
        real(kind=8), allocatable :: weights(:) ! coefficients and intercept
        type(error)               :: err

        if ( allocated(this%coefs_) ) deallocate(this%coefs_)

        n_samples = data_holder_ptr%n_samples
        n_columns = data_holder_ptr%n_columns

        ! Compute X^T X
        call mattxmat(mat_sq, data_holder_ptr%x_ptr%x_r8_ptr, &
            n_samples, n_columns, with_intercept=this%hparam%fit_intercept)

        ! Add Penalty
        do i=1, n_columns, 1
            mat_sq(i,i) = mat_sq(i,i) + this%hparam%lambda_l2
        end do

        ! Cholesky decomposition of X^T X
        n_columns_with = n_columns
        if ( this%hparam%fit_intercept ) n_columns_with = n_columns_with + 1
        allocate(mat_lower(n_columns_with, n_columns_with))
        allocate(diagonals(n_columns_with))
        call cholesky_decomposition_modified(mat_lower, diagonals, mat_sq, n_columns_with)

        ! Compute inverse of unit lower triangular matrix
        allocate(mat_lower_inv(n_columns_with, n_columns_with))
        call inv_unit_lower_matrix(mat_lower_inv, mat_lower, n_columns_with)

        ! Compute inverse of X^T X
        allocate(mat_sq_inv(n_columns_with, n_columns_with))
        allocate(diagonals_inv(n_columns_with, n_columns_with))
        call identity(diagonals_inv, n_columns_with)
        do i=1, n_columns_with, 1
            diagonals_inv(i,i) = 1.0d0 / diagonals(i)
        end do
        mat_sq_inv = matmul(matmul(transpose(mat_lower_inv), diagonals_inv), mat_lower_inv)

        ! Compute X^T Y
        allocate(xy(n_columns_with))
        xy = 0.0d0
        do j=1, n_columns, 1
            tmp_sum = 0.0d0
            do i=1, n_samples, 1
                tmp_sum = tmp_sum &
                    + data_holder_ptr%x_ptr%x_r8_ptr(i,j) & 
                    * data_holder_ptr%y_ptr%y_r8_ptr(i,1)
            end do
            xy(j) = tmp_sum
        end do
        if ( this%hparam%fit_intercept ) xy(n_columns_with) = sum(data_holder_ptr%y_ptr%y_r8_ptr)

        ! Compute weights 
        allocate(weights(n_columns_with))
        do j=1, n_columns_with, 1
            tmp_sum = 0.0d0
            do i=1, n_columns_with, 1
                tmp_sum = tmp_sum + mat_sq_inv(i,j) * xy(i)
            end do
            weights(j) = tmp_sum
        end do

        ! Store Training Results
        allocate(this%coefs_(n_columns))
        this%coefs_ = weights(1:n_columns)
        this%intercept_ = 0d0
        if ( this%hparam%fit_intercept ) this%intercept_ = weights(n_columns_with)

        this%n_columns = n_columns
        this%is_trained = t_
        this%is_simple = f_
    end subroutine fit_ridge_regression_multiple


    !> A subroutine to fit multiple linear regression model with L1 regularization.
    !! \todo OPTIMIZATION !!!!!
    !! \return returns trained model
    !! \param x 2-dim variable
    !! \param y 1-dim response
    subroutine fit_lasso_regression(this, dholder)
        use mod_timer
        implicit none
        class(lasso_regression)    :: this
        type(data_holder), target :: dholder
        type(data_holder), pointer :: data_holder_ptr

        integer(kind=8)    :: date_value1(8), date_value2(8)

        integer(kind=8)           :: n_samples, n_columns, n_columns_with
        real(kind=8), allocatable :: mat_sq(:,:), mat_sq_inv(:,:)
        real(kind=8), allocatable :: xy(:)
        real(kind=8), allocatable :: mat_lower(:,:), diagonals(:)
        real(kind=8), allocatable :: mat_lower_inv(:,:), diagonals_inv(:,:)
        integer(kind=8)           :: i, j, k, iter, d, n_samples_unroll
        real(kind=8)              :: tmp_sum, tmp_sq_sum, rho, w0, tmp
        real(kind=8), allocatable :: weights(:), weights_old(:) ! coefficients and intercept
        real(kind=8), allocatable :: x_sq_sum(:), xw(:), tmp_x(:)
        real(kind=8), allocatable :: tmp_y(:)
        real(kind=8) :: tmp_update(7), tmp_w
        data_holder_ptr => dholder

        ! Validation
        if ( allocated(this%coefs_) ) deallocate(this%coefs_)
        n_samples = data_holder_ptr%n_samples
        n_columns = data_holder_ptr%n_columns
        this%n_columns = n_columns
        n_samples_unroll = n_samples - mod(n_samples, 7)

        ! Compute sum(x^2, dim=1
        allocate(tmp_x(n_samples))
        allocate(x_sq_sum(this%n_columns))
        x_sq_sum = 0
        do j=1, this%n_columns, 1
            tmp_sq_sum = 0
            do i=1, n_samples, 1
                tmp = data_holder_ptr%x_ptr%x_r8_ptr(i,j)
                tmp_sq_sum = tmp_sq_sum + tmp**2.0
            end do
            x_sq_sum(j) = tmp_sq_sum
        end do

        ! x_sq_sum = 1d0 / x_sq_sum
        do j=1, this%n_columns, 1
            if ( x_sq_sum(j) .eq. 0 ) then
                x_sq_sum(j) = 0
            else
                x_sq_sum(j) = 1d0 / x_sq_sum(j)
            end if
        end do


        ! Initialize weights
        allocate(weights_old(this%n_columns))
        allocate(weights(this%n_columns))
        weights = 0d0
        weights_old = weights


        ! Compute Intercept
        allocate(xw(n_samples))
        call multi_mat_vec(data_holder_ptr%x_ptr%x_r8_ptr, weights, xw, n_samples, this%n_columns)
        w0 = 0
        if ( this%hparam%fit_intercept ) w0 = sum(data_holder_ptr%y_ptr%y_r8_ptr(:,1) - xw) / dble(n_samples)


        ! Iterate Cordinate Descent
        do iter=1, this%hparam%max_iter
            do d=1, this%n_columns, 1
                tmp_w = weights(d)
                if ( x_sq_sum(d) .eq. 0d0 ) cycle

                tmp_x = data_holder_ptr%x_ptr%x_r8_ptr(:,d)
                ! if ( tmp_w .ne. 0d0 ) then
                !     xw = xw - tmp_x * tmp_w
                ! end if
                if ( tmp_w .ne. 0d0 ) then
                    do i=1, n_samples_unroll, 7
                        do k=0, 7-1, 1
                            tmp_update(k+1) = xw(i+k) - tmp_x(i+k) * tmp_w
                        end do
                        do k=0, 7-1, 1
                            xw(i+k) = tmp_update(k+1)
                        end do
                    end do
                    do i=n_samples_unroll+1, n_samples
                        xw(i) = xw(i) - tmp_x(i) * tmp_w
                    end do
                end if

                tmp_sum = 0
                ! rho = sum((y-w0-xw) * tmp_x)
                do i=1, n_samples_unroll, 7
                    do k=0, 7-1, 1
                        tmp_sum = tmp_sum & 
                            + (data_holder_ptr%y_ptr%y_r8_ptr(i+k,1)-w0-xw(i+k)) & 
                              * tmp_x(i+k)
                    end do
                end do
                do i=n_samples_unroll+1, n_samples, 1
                    tmp_sum = tmp_sum &
                        + (data_holder_ptr%y_ptr%y_r8_ptr(i,1)-w0-xw(i)) & 
                          * tmp_x(i)
                end do
                rho = tmp_sum

                if     ( rho .gt.   this%hparam%lambda_l1 * n_samples ) then
                    weights(d) = (rho-this%hparam%lambda_l1 * n_samples) * x_sq_sum(d)
                    ! xw = xw + tmp_x * weights(d)
                    tmp_w = weights(d)
                    do i=1, n_samples_unroll, 7
                        do k=0, 7-1, 1
                            tmp_update(k+1) = xw(i+k) + tmp_x(i+k) * tmp_w
                        end do
                        do k=0, 7-1, 1
                            xw(i+k) = tmp_update(k+1)
                        end do
                    end do
                    do i=n_samples_unroll+1, n_samples
                        xw(i) = xw(i) + tmp_x(i) * tmp_w
                    end do
                    
                elseif ( rho .lt. - this%hparam%lambda_l1 * n_samples ) then
                    weights(d) = (rho+this%hparam%lambda_l1 * n_samples) * x_sq_sum(d)
                    ! xw = xw + tmp_x * weights(d)
                    tmp_w = weights(d)
                    do i=1, n_samples_unroll, 7
                        do k=0, 7-1, 1
                            tmp_update(k+1) = xw(i+k) + tmp_x(i+k) * tmp_w
                        end do
                        do k=0, 7-1, 1
                            xw(i+k) = tmp_update(k+1)
                        end do
                    end do
                    do i=n_samples_unroll+1, n_samples
                        xw(i) = xw(i) + tmp_x(i) * tmp_w
                    end do
                    
                else
                    weights(d) = 0
                end if
            end do

            if ( sum(abs(weights_old-weights)) .le. 1d-5 ) exit
            weights_old = weights
            if ( this%hparam%fit_intercept ) then
                tmp_sum = 0
                do i=1, n_samples_unroll, 7
                    do k=0, 7-1, 1
                        tmp_sum = tmp_sum+data_holder_ptr%y_ptr%y_r8_ptr(i+k,1)-xw(i+k)
                    end do
                end do
                do i=n_samples_unroll+1, n_samples, 1
                    tmp_sum = tmp_sum+data_holder_ptr%y_ptr%y_r8_ptr(i,1)-xw(i)
                end do
                w0 = tmp_sum / dble(n_samples)
            end if
        end do

        this%coefs_ = weights
        this%intercept_ = w0
        this%n_columns = n_columns
        this%is_trained = t_
        this%is_simple = f_

        if ( n_columns .eq. 1_8 ) then
            this%coef_ = this%coefs_(1)
            this%intercept_ = w0
            this%is_trained = t_
            this%is_simple = t_
            deallocate(this%coefs_)
        end if
    end subroutine fit_lasso_regression


    !> A function to predict linear regression model
    function predict(this, x)
        class(base_linear_regressor) :: this
        real(kind=8), intent(in)     :: x(:,:)
        real(kind=8), allocatable    :: predict(:,:)
        integer(kind=8)              :: x_shape(2), n_samples, n_columns, one_i8=1_8
        type(error)                  :: err
        call err%check_estimator_is_fitted(this%is_trained, this%algo_name)

        x_shape = shape(x)
        n_samples = x_shape(1)
        n_columns = x_shape(2)

        allocate(predict(n_samples,1))
        if ( n_columns .eq. one_i8 ) then
            call this%predict_simple(x, predict, n_samples)
        else
            call this%predict_multiple(x, predict, n_samples, n_columns)
        end if
    end function predict


    !> A subroutine to predict trained simple linear regression, simple ridge regression model.
    !! \return returns predicted values
    !! \param x 1-dim variable
    subroutine predict_simple(this, x, predict, n_samples)
        implicit none
        class(base_linear_regressor) :: this
        real(kind=8), intent(in)     :: x(n_samples,1)
        real(kind=8), intent(inout)  :: predict(n_samples,1)
        integer(kind=8), intent(in)  :: n_samples
        integer(kind=8) :: n

        do n=1, n_samples, 1
            predict(n,1) = x(n,1) * this%coef_ + this%intercept_
        end do
    end subroutine predict_simple


    !> A subroutine to predict trained multiple linear regression, multiple ridge regression model.
    !! \return returns predicted values
    !! \param x N-dim variable
    subroutine predict_multiple(this, x, predict, n_samples, n_columns)
        implicit none
        class(base_linear_regressor) :: this
        real(kind=8), intent(in)     :: x(n_samples, n_columns)
        real(kind=8), intent(inout)  :: predict(n_samples, 1)
        integer(kind=8), intent(in)  :: n_samples, n_columns
        integer(kind=8) :: n

        call multi_mat_vec(x, this%coefs_, predict(:,1), n_samples, n_columns)
        do n=1, n_samples, 1
            predict(n,1) = predict(n,1) + this%intercept_
        end do
    end subroutine predict_multiple


    subroutine dump_model(this, file_name)
        implicit none
        class(base_linear_regressor) :: this
        character(len=*)           :: file_name

        open(10, file=file_name, form='unformatted', status="replace")
        if (.not. this % is_trained) then
            print*, trim(this % algo_name),  " is not trained. Cannot dump model."
            write(10) f_ ! dump fail
            close(10)
            stop
        end if

        write(10) t_ ! dump success
        write(10) this%algo_name
        write(10) this%n_columns
        write(10) this%coef_
        if (.not. this % is_simple)  write(10) this%coefs_
        write(10) this%intercept_
        close(10)
    end subroutine

    subroutine load_model(this, file_name)
        implicit none
        class(base_linear_regressor) :: this
        character(len=*)           :: file_name
        logical(kind=4) :: is_dump_success

        open(10, file=file_name, form='unformatted')
        read(10) is_dump_success
        if (.not. is_dump_success) then
            print*, trim(file_name),  " failed dump of the model."
            stop
        end if
        read(10) this%algo_name
        read(10) this%n_columns
        read(10) this%coef_
        if (.not. this % is_simple)  then
            allocate(this%coefs_(this%n_columns))
            read(10) this%coefs_
        end if
        read(10) this%intercept_
        close(10)
        this%is_trained = t_
    end subroutine

end module mod_linear_regression