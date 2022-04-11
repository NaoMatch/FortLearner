module mod_gradient_boosting_lawu
    use mod_const
    use mod_common
    use mod_timer
    use mod_stats

    use mod_lawu
    implicit none


    !> A type for gradient boosting tree whose base estimator is 'lawu_regressor'
    type gradient_boosting_lawu_regressor
        logical(kind=4) :: is_axis_parallel            !< axis parallel split decision tree or not. MUST BE .TRUE.
        real(kind=8), allocatable :: y_train_mean(:)   !< The average value of y during learning, and the initial value during prediction.
        integer(kind=8) :: n_estimators_               !< number of 'lawu_regressor'
        integer(kind=8) :: n_outputs_                  !< number of output dimensions
        type(hparam_decisiontree) :: hparam            !< type of hyperparameter for 'decision_tree_regressor'
        type(lawu_regressor), allocatable :: ltrees(:) !< array of trained 'lawu_regressor'
    contains
        procedure :: fit => fit_gradient_boosting_lawu_regressor
        procedure :: predict => predict_gradient_boosting_lawu_regressor
        procedure :: dump => dump_gradient_boosting_lawu_regressor
        procedure :: load => load_gradient_boosting_lawu_regressor
    end type gradient_boosting_lawu_regressor
    
    interface gradient_boosting_lawu_regressor
        procedure :: new_gradient_boosting_lawu_regressor
    end interface gradient_boosting_lawu_regressor

contains
    !> A function to create new 'gradient_boosting_lawu_regressor' object
    !! \param max_bins maximum number of bins. must be greater equal 2
    !! \param strategy binning strategy. 
    !!      'uniform': divide uniformly between the maximum and minimum values, 
    !!      'quantile': split according to quantile values, 
    !!      'kmeans': split by one-dimensional kmeans clustering, 
    !!      'greedy': see https://arxiv.org/pdf/2005.01653.pdf, 
    !!      'modified_greedy': see https://arxiv.org/pdf/2005.01653.pdf
    !! \param n_estimator number of estimators. [1, huge value)
    !! \param learning_rate learning rate for each tree. must be greater than 0
    !! \param learning_rate_layer learning rate for each layer in a tree
    !! \param max_depth max depth. must be greater equal 1
    !! \param boot_strap with or without bootstrap sampling. default value is .false.
    !! \param max_leaf_nodes maximum number of leaf node. must be greater equal 2
    !! \param min_samples_leaf minimum number of samples in node. must be greater equal 1 for existence of node
    !! \param fashion how to split node. 'best': split the node with largest information gain, 'depth': split the deepest splittable(not leaf) node, 
    !!        'level': split all specific depth nodes at a time, 'impurity': split the node with largest impurity, 
    !!        'sample': split the node with largest sample size
    !! \param max_features maximum number of features in node split phase. must be greater equal 1
    function new_gradient_boosting_lawu_regressor(&
        max_bins, strategy, &
        n_estimators, learning_rate, learning_rate_layer, &
        max_depth, boot_strap, max_leaf_nodes, min_samples_leaf, &
        fashion, max_features &
        )
        implicit none
        type(gradient_boosting_lawu_regressor) :: new_gradient_boosting_lawu_regressor
        type(gradient_boosting_lawu_regressor) :: tmp

        integer(kind=8), optional  :: max_bins
        character(len=*), optional :: strategy
        real(kind=8), optional     :: learning_rate
        real(kind=8), optional     :: learning_rate_layer
        integer(kind=8), optional  :: n_estimators
        integer(kind=8), optional  :: max_depth
        logical(kind=4), optional  :: boot_strap
        integer(kind=8), optional  :: max_leaf_nodes
        integer(kind=8), optional  :: min_samples_leaf
        character(len=*), optional :: fashion
        integer(kind=8), optional  :: max_features
        character(len=256)         :: fashion_list(5), strategy_list(5)

        tmp%is_axis_parallel = t_
        tmp%hparam%algo_name = "gradient_boosting_lawu_regressor"

        fashion_list(1) = "best"
        fashion_list(2) = "depth"
        fashion_list(3) = "level"
        fashion_list(4) = "impurity"
        fashion_list(5) = "sample"

        strategy_list(1) = "uniform"
        strategy_list(2) = "quantile"
        strategy_list(3) = "kmeans"
        strategy_list(4) = "greedy"
        strategy_list(5) = "modified_greedy"

        if ( present(max_bins) )         tmp%hparam%max_bins         = max_bins
        if ( present(strategy) )         tmp%hparam%strategy         = strategy
        if ( present(learning_rate) )    tmp%hparam%learning_rate    = learning_rate
        if ( present(learning_rate_layer) )    tmp%hparam%learning_rate_layer    = learning_rate_layer
        if ( present(n_estimators) )     tmp%hparam%n_estimators     = n_estimators
        if ( present(max_depth) )        tmp%hparam%max_depth        = max_depth
        if ( present(boot_strap) )       tmp%hparam%boot_strap       = boot_strap
        if ( present(max_leaf_nodes) )   tmp%hparam%max_leaf_nodes   = max_leaf_nodes
        if ( present(min_samples_leaf) ) tmp%hparam%min_samples_leaf = min_samples_leaf
        if ( present(fashion) )          tmp%hparam%fashion          = fashion
        if ( present(max_features) )     tmp%hparam%max_features     = max_features

        call tmp%hparam%validate_int_range("max_bins",         tmp%hparam%max_bins,         2_8,   huge(0_8))
        call tmp%hparam%validate_real_range("learning_rate",   tmp%hparam%learning_rate,    1d-10, huge(0d0))
        call tmp%hparam%validate_real_range("learning_rate_layer",   tmp%hparam%learning_rate_layer,    1d-10, huge(0d0))
        call tmp%hparam%validate_int_range("n_estimators",     tmp%hparam%n_estimators,     1_8,   huge(1_8))
        call tmp%hparam%validate_int_range("max_depth",        tmp%hparam%max_depth,        1_8,   huge(1_8))
        call tmp%hparam%validate_int_range("max_leaf_nodes",   tmp%hparam%max_leaf_nodes,   2_8,   huge(1_8))
        call tmp%hparam%validate_int_range("min_samples_leaf", tmp%hparam%min_samples_leaf, 1_8,   huge(1_8))
        call tmp%hparam%validate_char_list("fashion",          tmp%hparam%fashion,          fashion_list)
        call tmp%hparam%validate_int_range("max_features",     tmp%hparam%max_features,     1_8,   huge(1_8), exception=-1_8)

        tmp%hparam%fashion_int = tmp%hparam%convert_char_to_int(tmp%hparam%fashion, fashion_list)
        tmp%hparam%strategy_int = tmp%hparam%convert_char_to_int(tmp%hparam%strategy, strategy_list)

        new_gradient_boosting_lawu_regressor = tmp
    end function new_gradient_boosting_lawu_regressor


    !> A subroutine to fit gradient_boosting_lawu_regressor.
    !! \param data_holder_ptr pointer of data_holder
    subroutine fit_gradient_boosting_lawu_regressor(this, data_holder_ptr)
        implicit none
        class(gradient_boosting_lawu_regressor) :: this
        type(data_holder), pointer     :: data_holder_ptr
        integer(kind=8) :: i, n
        real(kind=8), allocatable :: y_save(:,:), y_pred(:,:), y_current_pred(:,:)

        if (allocated(this%ltrees)) deallocate(this%ltrees)
        if (allocated(this%y_train_mean)) deallocate(this%y_train_mean)
        if (allocated(y_save)) deallocate(y_save)
        if (allocated(y_pred)) deallocate(y_pred)
        if (allocated(y_current_pred)) deallocate(y_current_pred)
        allocate(this%ltrees(this%hparam%n_estimators))

        ! print*, '============================================================='
        if (this%hparam%max_features .eq. huge(0_8)) then
            this%hparam%max_features = int(sqrt(data_holder_ptr%n_columns+0d0), kind=8)
        end if

        this%n_estimators_ = this%hparam%n_estimators
        this%n_outputs_ = data_holder_ptr%n_outputs

        ! Mean
        allocate(this%y_train_mean(this%n_outputs_))
        allocate(y_save(data_holder_ptr%n_samples, data_holder_ptr%n_outputs))
        do i=1, data_holder_ptr%n_samples, 1
            y_save(i,:) = data_holder_ptr%y_ptr%y_r8_ptr(i,:)
        end do
        this%y_train_mean = mean(y_save, data_holder_ptr%n_samples, data_holder_ptr%n_outputs)

        ! Residual
        do i=1, data_holder_ptr%n_samples, 1
            data_holder_ptr%y_ptr%y_r8_ptr(i,:) = y_save(i,:) - this%y_train_mean(:)
        end do

        allocate(y_pred(data_holder_ptr%n_samples, data_holder_ptr%n_outputs))
        allocate(y_current_pred(data_holder_ptr%n_samples, data_holder_ptr%n_outputs))
        do i=1, data_holder_ptr%n_samples, 1
            y_current_pred(i,:) = this%y_train_mean(:)
        end do

        do n=1, this%hparam%n_estimators, 1
            this%ltrees(n) = lawu_regressor(&
                learning_rate_layer = this%hparam%learning_rate_layer, &
                max_bins = this%hparam%max_bins, &
                strategy = this%hparam%strategy, &
                max_depth = this%hparam%max_depth, &
                boot_strap = this%hparam%boot_strap, &
                max_leaf_nodes = this%hparam%max_leaf_nodes, &
                min_samples_leaf = this%hparam%min_samples_leaf, &
                fashion = this%hparam%fashion, &
                max_features = this%hparam%max_features &
            )
            call this%ltrees(n)%fit(data_holder_ptr)

            y_pred = this%ltrees(n)%predict(data_holder_ptr%x_ptr%x_r8_ptr)
            do i=1, data_holder_ptr%n_samples, 1
                y_current_pred(i,:) = y_current_pred(i,:) + this%hparam%learning_rate * y_pred(i,:)
            end do

            do i=1, data_holder_ptr%n_samples, 1
                data_holder_ptr%y_ptr%y_r8_ptr(i,:) &
                    = y_save(i,:) &
                    - y_current_pred(i,:)
            end do
        end do

        do i=1, data_holder_ptr%n_samples, 1
            data_holder_ptr%y_ptr%y_r8_ptr(i,:) = y_save(i,:)
        end do
    end subroutine fit_gradient_boosting_lawu_regressor


    !> A function to predict for gradient_boosting_lawu_regressor.
    !! \param x data to be predicted
    function predict_gradient_boosting_lawu_regressor(this, x)
        implicit none
        class(gradient_boosting_lawu_regressor) :: this
        real(kind=8), intent(in) :: x(:,:)
        real(kind=8), ALLOCATABLE :: predict_gradient_boosting_lawu_regressor(:,:)
        integer(kind=8) :: shape_x(2), n_samples, n, i
        shape_x = shape(x)
        n_samples = shape_x(1)
        allocate(predict_gradient_boosting_lawu_regressor(n_samples, this%n_outputs_))
        do i=1, n_samples, 1
            predict_gradient_boosting_lawu_regressor(i,:) = this%y_train_mean(:)
        end do

        do n=1, this%n_estimators_, 1
            predict_gradient_boosting_lawu_regressor = &
                predict_gradient_boosting_lawu_regressor & 
                + this%ltrees(n)%predict(x) * this%hparam%learning_rate
        end do
    end function predict_gradient_boosting_lawu_regressor


    subroutine dump_gradient_boosting_lawu_regressor(this, file_name)
        implicit none
        class(gradient_boosting_lawu_regressor) :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit, i
        open(newunit=newunit, file=file_name, form="unformatted", status="replace")
        write(newunit) this%hparam%n_estimators ! dump fail
        write(newunit) this%n_outputs_ ! dump fail
        write(newunit) this%y_train_mean(:) ! dump fail
        do i=1, size(this%ltrees(:)), 1
            call this%ltrees(i)%dump_base_tree(newunit)
        end do
        close(newunit)
    end subroutine dump_gradient_boosting_lawu_regressor


    subroutine load_gradient_boosting_lawu_regressor(this, file_name)
        implicit none
        class(gradient_boosting_lawu_regressor) :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit, i
        open(newunit=newunit, file=file_name, form="unformatted")
        read(newunit) this%n_estimators_; allocate(this%ltrees(this%n_estimators_))
        read(newunit) this%n_outputs_; allocate(this%y_train_mean(this%n_outputs_))
        read(newunit) this%y_train_mean(:)
        do i=1, size(this%ltrees(:)), 1
            call this%ltrees(i)%load_base_tree(newunit)
        end do
        close(newunit)
    end subroutine load_gradient_boosting_lawu_regressor

end module mod_gradient_boosting_lawu