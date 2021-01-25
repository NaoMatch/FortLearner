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
    
    type loss
        integer(kind=8) :: loss_type
    contains
        ! procedure :: compute
        ! procedure :: dcompute
    end type loss


    type sgd_regressor
        logical(kind=4) :: is_fitted=f_
        character(len=256) :: algo_name="sgd_regressor"
        integer(kind=8) :: n_columns
        real(kind=8), allocatable :: coefs_(:)
        real(kind=8) :: intercept_

        type(hparam_sgd_estimator) :: hparam
    contains
        procedure :: fit => fit_sgd_regressor
        procedure :: predict => predict_sgd_regressor
    end type sgd_regressor

    interface sgd_regressor
        module procedure :: new_sgd_regressor 
    end interface sgd_regressor

contains

    function new_sgd_regressor()
        type(sgd_regressor) :: new_sgd_regressor
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
        real(kind=8) :: n_samples_train_inv, y_sample
        real(kind=8) :: update_b
        real(kind=8), allocatable :: update_w(:)

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
        call random_number(this%intercept_)
        this%coefs_(:)  = 2d0 * this%coefs_(:)  - 1d0
        this%intercept_ = 2d0 * this%intercept_ - 1d0

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
        do epoch=1, this%hparam%max_iter, 1
            call permutation(indices_train, n_samples_train)
            do batch=1, n_samples_train, 1
                idx = indices_train(batch)
                x_sample(:) = data_holder_ptr%x_ptr%x_r8_ptr(idx,:)
                y_sample    = data_holder_ptr%y_ptr%y_r8_ptr(idx,1)

                update_b = & 
                    - (y_sample - sum(x_sample*this%coefs_) - this%intercept_) & 
                    * n_samples_train_inv
                update_w = x_sample * update_b
                this%coefs_     = this%coefs_     - this%hparam%learning_rate_initial * update_w
                this%intercept_ = this%intercept_ - this%hparam%learning_rate_initial * update_b
            end do
            call multi_mat_vec(x_valid, this%coefs_, y_pred_valid, n_samples_valid, n_columns)
            
            do batch=1, n_samples_valid, 1
                y_pred_valid(batch) = y_pred_valid(batch) + this%intercept_
            end do
            print*, metric%mean_square_error(y_true_valid, y_pred_valid)
        end do

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


end module mod_sgd_estimator
