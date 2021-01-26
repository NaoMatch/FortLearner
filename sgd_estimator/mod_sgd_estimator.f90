module mod_sgd_estimator
    use mod_const
    use mod_common
    use mod_timer
    use mod_random
    use mod_error

    use mod_metric
    use mod_linalg
    use mod_hyperparameter
    use mod_data_holder
    implicit none


    type sgd_regressor
        logical(kind=4)    :: verbose=f_
        logical(kind=4)    :: is_fitted=f_
        character(len=256) :: algo_name="sgd_regressor"
        integer(kind=8)    :: n_columns
        real(kind=8), allocatable :: coefs_(:)
        real(kind=8)       :: intercept_
        type(hparam_sgd_estimator) :: hparam
    contains
        procedure :: fit => fit_sgd_regressor
        procedure :: predict => predict_sgd_regressor
    end type sgd_regressor

    interface sgd_regressor
        module procedure :: new_sgd_regressor 
    end interface sgd_regressor

contains

    function new_sgd_regressor(&
            loss, &
            max_iter, &
            learning_rate, &
            learning_rate_initial, &
            tolerance, &
            power_t, &
            alpha, &
            penalty, &
            l1_ratio, &
            fit_intercept, &
            verbose)
        type(sgd_regressor)        :: new_sgd_regressor, tmp

        character(len=*), optional :: loss
        integer(kind=8), optional  :: max_iter
        character(len=*), optional :: learning_rate
        real(kind=8), optional     :: learning_rate_initial
        real(kind=8), optional     :: tolerance
        real(kind=8), optional     :: power_t
        real(kind=8), optional     :: alpha
        character(len=*), optional :: penalty
        real(kind=8), optional     :: l1_ratio
        logical(kind=4), optional  :: fit_intercept
        logical(kind=4), optional  :: verbose

        character(len=256) :: loss_preset(1), lr_methods_preset(2), pnlty_methods_preset(3)
        tmp%hparam%algo_name = "sgd_regressor"
        
        loss_preset(1)       = "squared_loss"
        lr_methods_preset(1) = "constant"
        lr_methods_preset(2) = "invscaling"
        pnlty_methods_preset(1) = "l1"
        pnlty_methods_preset(2) = "l2"
        pnlty_methods_preset(3) = "elasticnet"

        if (present(loss)) tmp%hparam%loss = loss
        if (present(max_iter)) tmp%hparam%max_iter = max_iter
        if (present(learning_rate)) tmp%hparam%learning_rate = learning_rate
        if (present(learning_rate_initial)) tmp%hparam%learning_rate_initial = learning_rate_initial
        if (present(tolerance)) tmp%hparam%tolerance = tolerance
        if (present(power_t)) tmp%hparam%power_t = power_t
        if (present(alpha)) tmp%hparam%alpha = alpha
        if (present(penalty)) tmp%hparam%penalty = penalty
        if (present(l1_ratio)) tmp%hparam%l1_ratio = l1_ratio
        if (present(fit_intercept)) tmp%hparam%fit_intercept = fit_intercept
        if (present(verbose)) tmp%verbose = verbose

        call tmp%hparam%validate_int_range("max_iter", tmp%hparam%max_iter, 1_8, huge(0_8))
        call tmp%hparam%validate_real_range("learning_rate_initial", tmp%hparam%learning_rate_initial, 1d-10, huge(0d0))
        call tmp%hparam%validate_real_range("tolerance", tmp%hparam%tolerance, 1d-10, huge(0d0))
        call tmp%hparam%validate_real_range("power_t", tmp%hparam%power_t, 0d0, 1d0)
        call tmp%hparam%validate_real_range("alpha", tmp%hparam%alpha, 1d-10, 1d0)
        call tmp%hparam%validate_real_range("l1_ratio", tmp%hparam%l1_ratio, 1d-10, 1d0)

        call tmp%hparam%validate_char_list("loss", tmp%hparam%loss, loss_preset)
        call tmp%hparam%validate_char_list("learning_rate", tmp%hparam%learning_rate, lr_methods_preset)
        call tmp%hparam%validate_char_list("penalty", tmp%hparam%penalty, pnlty_methods_preset)
        tmp%hparam%learning_rate_int = tmp%hparam%convert_char_to_int(tmp%hparam%learning_rate, lr_methods_preset)
        tmp%hparam%penalty_int       = tmp%hparam%convert_char_to_int(tmp%hparam%penalty, pnlty_methods_preset)

        new_sgd_regressor = tmp
    end function new_sgd_regressor


    subroutine fit_sgd_regressor(this, data_holder_ptr)
        implicit none
        class(sgd_regressor)       :: this
        type(data_holder), pointer :: data_holder_ptr
        type(error) :: err
        type(metrics) :: metric

        integer(kind=8) :: epoch, batch, n_samples, n_columns, idx
        integer(kind=8) :: n_samples_train, n_samples_valid
        integer(kind=8), ALLOCATABLE :: indices_train(:), indices_valid(:), indices_all(:)
        real(kind=8), allocatable :: x_valid(:,:), y_pred_valid(:), y_true_valid(:), x_sample(:)
        real(kind=8) :: n_samples_train_inv, y_sample, learning_rate
        real(kind=8) :: update_b, best_intercept_, best_loss, current_loss
        real(kind=8), allocatable :: update_w(:), best_coefs_(:)
        integer(kind=8) :: count_no_change

        n_samples = data_holder_ptr%n_samples
        n_columns = data_holder_ptr%n_columns
        n_samples_train = int(n_samples*(1d0-this%hparam%validation_fraction), kind=kind(n_samples_train))
        n_samples_valid = n_samples - n_samples_train
        n_samples_train_inv = 1d0 / dble(n_samples_train)
        call err%only_accept_Nx1_matrix(data_holder_ptr%y_shape, "y", this%algo_name)

        print*, '============================================================='
        print*, "Initialize Weight and Intercept"
        allocate(this%coefs_(n_columns))
        allocate(update_w(n_columns))
        call random_number(this%coefs_)
        this%coefs_(:)  = 2d0 * this%coefs_(:)  - 1d0

        this%intercept_ = 0d0
        if (this%hparam%fit_intercept) then
            call random_number(this%intercept_)
            this%intercept_ = 2d0 * this%intercept_ - 1d0
        end if

        print*, '============================================================='
        print*, "Train Test Split"
        allocate(indices_all(n_samples))
        allocate(indices_train(n_samples_train))
        allocate(indices_valid(n_samples_valid))
        do batch=1, n_samples, 1
            indices_all(batch) = batch
        end do
        call permutation(indices_all, n_samples)
        indices_train(:) = indices_all(1:n_samples_train)
        indices_valid(:) = indices_all(n_samples_train+1:n_samples)
        allocate(x_valid(n_samples_valid, n_columns))
        allocate(y_pred_valid(n_samples_valid))
        allocate(y_true_valid(n_samples_valid))
        do batch=1, n_samples_valid, 1
            idx = indices_valid(batch)
            x_valid(batch,:) = data_holder_ptr%x_ptr%x_r8_ptr(idx,:)
            y_true_valid(batch) = data_holder_ptr%y_ptr%y_r8_ptr(idx,1)
        end do

        print*, '============================================================='
        print*, "START"
        allocate(x_sample(n_columns))
        allocate(best_coefs_(n_columns))
        best_loss = huge(0d0)
        learning_rate = this%hparam%learning_rate_initial
        count_no_change = 0_8

        do epoch=1, this%hparam%max_iter, 1
            if (this%hparam%shuffle) call permutation(indices_train, n_samples_train)
            do batch=1, n_samples_train, 1
                idx = indices_train(batch)
                x_sample(:) = data_holder_ptr%x_ptr%x_r8_ptr(idx,:)
                y_sample    = data_holder_ptr%y_ptr%y_r8_ptr(idx,1)

                update_b = - y_sample + sum(x_sample*this%coefs_) + this%intercept_
                if ( this%hparam%penalty_int .eq. 1_8 ) then
                    update_w = x_sample * update_b + this%hparam%alpha * sign(1d0, this%coefs_)
                elseif ( this%hparam%penalty_int .eq. 2_8 ) then
                    update_w = x_sample * update_b + this%hparam%alpha * this%coefs_
                elseif ( this%hparam%penalty_int .eq. 3_8 ) then
                    update_w = x_sample * update_b & 
                        + this%hparam%alpha * this%hparam%l1_ratio * sign(1d0, this%coefs_) & 
                        + this%hparam%alpha * (1d0-this%hparam%l1_ratio) * this%coefs_
                end if

                ! call meupdate(update_w, n_columns, top_k=.5d0)
                this%coefs_     = this%coefs_     - learning_rate * update_w
                if (this%hparam%fit_intercept) this%intercept_ = this%intercept_ - learning_rate * update_b
            end do
            call multi_mat_vec(x_valid, this%coefs_, y_pred_valid, n_samples_valid, n_columns)
            
            do batch=1, n_samples_valid, 1
                y_pred_valid(batch) = y_pred_valid(batch) + this%intercept_
            end do
            current_loss = metric%mean_square_error(y_true_valid, y_pred_valid)
            if ( best_loss .gt. current_loss ) then
                best_loss = current_loss
                best_coefs_ = this%coefs_
                best_intercept_ = this%intercept_
                count_no_change = 0_8
            elseif (this%hparam%tolerance .lt. abs(best_loss-current_loss) ) then
                count_no_change = count_no_change + 1
            end if

            if ( count_no_change .ge. this%hparam%n_iter_no_change ) exit
            if (this%verbose) print*, epoch, current_loss, best_loss, learning_rate, count_no_change
            if (this%hparam%learning_rate_int .eq. 2_8) then
                learning_rate = this%hparam%learning_rate_initial / epoch**this%hparam%power_t
            end if
        end do

        this%coefs_ = best_coefs_ 
        this%intercept_ = best_intercept_ 
        this%n_columns = n_columns
        this%is_fitted = t_
    end subroutine fit_sgd_regressor


    function predict_sgd_regressor(this, x)
        implicit none
        class(sgd_regressor) :: this
        real(kind=8), intent(in) :: x(:,:)
        real(kind=8), ALLOCATABLE :: predict_sgd_regressor(:,:)

        integer(kind=8) :: shape_x(2), n_samples, n_columns, i
        type(error) :: err

        shape_x = shape(x)
        n_samples = shape_x(1)
        n_columns = shape_x(2)
        call err%check_estimator_is_fitted(this%is_fitted, this%algo_name)
        call err%check_number_of_features_mismatch(this%n_columns, n_columns, this%algo_name)

        allocate(predict_sgd_regressor(n_samples, 1))

        call multi_mat_vec(x, this%coefs_, predict_sgd_regressor(:,1), n_samples, n_columns)
        do i=1, n_samples, 1
            predict_sgd_regressor(i,1) = predict_sgd_regressor(i,1) + this%intercept_
        end do
    end function predict_sgd_regressor


    subroutine meupdate(vector, n_samples, top_k)
        implicit none
        real(kind=8) :: vector(n_samples)
        integer(kind=8) :: n_samples
        real(kind=8) :: top_k

        real(kind=8), ALLOCATABLE :: vector_abs(:)
        integer(kind=8), ALLOCATABLE :: indices(:)
        integer(kind=8) :: i, n_samples_top_k, idx

        n_samples_top_k = minval((/int(n_samples * top_k, kind=kind(top_k)), 1_8/))
        allocate(vector_abs(n_samples))
        allocate(indices(n_samples))
        do i=1, n_samples, 1
            vector_abs(i) = abs(vector(i))
            indices(i) = i
        end do
        call quick_argsort(vector_abs, indices, n_samples)

        do i=1, n_samples_top_k, 1
            idx = indices(i)
            vector(idx) = 0d0
        end do
    end subroutine meupdate

end module mod_sgd_estimator
