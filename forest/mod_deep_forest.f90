module mod_deep_forest
    use mod_const
    use mod_common
    use mod_timer
    use mod_error
    use mod_random
    use mod_metric
    use mod_decision_tree
    use mod_forest
    use mod_forest
    implicit none
    
    type feature_range
        integer(kind=8) :: frange(2)
    end type feature_range

    type subsets
        type(feature_range), allocatable :: feature_ranges(:)
    end type subsets

    type deep_forest_regressor
        integer(kind=8) :: n_grains
        integer(kind=8), ALLOCATABLE :: n_columns_in_grains(:)
        type(subsets), ALLOCATABLE :: feature_subsets(:)
        type(random_forest_regressor), ALLOCATABLE :: rf_mgs(:) ! random forest regressors for multi-grained scanning
        type(extra_trees_regressor),   ALLOCATABLE :: et_mgs(:) ! extra trees regressors for multi-grained scanning

        type(random_forest_regressor), ALLOCATABLE :: rf_cas(:,:)
        type(extra_trees_regressor),   ALLOCATABLE :: et_cas(:,:)

        type(random_forest_regressor), ALLOCATABLE :: rf_out(:)
        type(extra_trees_regressor), ALLOCATABLE   :: et_out(:)

        type(hparam_decisiontree) :: hparam
    contains
        procedure :: fit_mgrain => fit_mgrain_df_regressor
        procedure :: predict    => predict_df_regressor
    end type deep_forest_regressor


    interface deep_forest_regressor
        procedure new_deep_forest_regressor
    end interface deep_forest_regressor

contains

    function new_deep_forest_regressor(n_estimators, feature_fractions, step_size_for_multi_grain, min_columns_in_grain, &
        max_leaf_nodes, n_cascades, n_forest_per_layer)
        implicit none
        type(deep_forest_regressor) :: new_deep_forest_regressor, tmp
        integer(kind=8), optional   :: n_estimators
        real(kind=8), optional      :: feature_fractions(:)
        integer(kind=8), optional   :: step_size_for_multi_grain
        integer(kind=8), optional   :: min_columns_in_grain
        integer(kind=8), optional   :: max_leaf_nodes
        integer(kind=8), optional   :: n_cascades
        integer(kind=8), optional   :: n_forest_per_layer

        type(error) :: err

        if (present(n_estimators))              tmp % hparam % n_estimators = n_estimators
        if (present(feature_fractions))         tmp % hparam % feature_fractions = feature_fractions
        if (present(step_size_for_multi_grain)) tmp % hparam % step_size_for_multi_grain = step_size_for_multi_grain
        if (present(min_columns_in_grain))      tmp % hparam % min_columns_in_grain = min_columns_in_grain
        if (present(max_leaf_nodes))            tmp % hparam % max_leaf_nodes = max_leaf_nodes
        if (present(n_cascades))                tmp % hparam % n_cascades = n_cascades
        if (present(n_forest_per_layer))        tmp % hparam % n_forest_per_layer = n_forest_per_layer

        new_deep_forest_regressor = tmp
    end function new_deep_forest_regressor


    subroutine fit_mgrain_df_regressor(this, data_holder_ptr)
        implicit none
        class(deep_forest_regressor) :: this
        type(data_holder), pointer   :: data_holder_ptr

        integer(kind=8) :: g, n, f, n_columns, counter, ini, fin, n_outputs, lidx, max_n_est
        integer(kind=8) :: n_samples, n_columns_to_be_created, n_columns_to_be_created_add_cascade, layer
        integer(kind=8), ALLOCATABLE :: n_columns_to_be_created_per_grain(:)
        integer(kind=8), ALLOCATABLE :: feature_indices(:)
        type(feature_range) :: franges
        character(:),allocatable :: n_new_columns
        character(len=512) :: msg
        type(decision_tree_regressor) :: dt
        type(random_forest_regressor) :: rf
        type(extra_trees_regressor) :: et
        type(metrics)                 :: metric
        integer(kind=8) :: n_forest_type = 2, stack, fid
        real(kind=8), allocatable :: x_input_copy(:,:)
        real(kind=8), allocatable, target :: x_stacked_fatures(:,:)
        real(kind=8), allocatable :: y_out(:,:)

        ! Store Feature Sliding 
        n_samples = data_holder_ptr % n_samples
        n_columns_to_be_created = 0
        n_outputs = data_holder_ptr % n_outputs
        n_columns = data_holder_ptr % n_columns
        this % n_grains = size(this % hparam % feature_fractions)
        allocate(this % n_columns_in_grains(this % n_grains))
        allocate(n_columns_to_be_created_per_grain(this % n_grains))
        n_columns_to_be_created_per_grain = 0
        max_n_est = 0
        do g=1, this%n_grains, 1
            this % n_columns_in_grains(g) & 
                = maxval((/ this%hparam%min_columns_in_grain, int(n_columns * this%hparam%feature_fractions(g), kind=kind(g)) /))
        end do
        allocate(this%feature_subsets(this%n_grains))
        do g=1, this%n_grains, 1
            allocate(this%feature_subsets(g)%feature_ranges(0))
            counter = 0
            fin = n_columns
            ini = n_columns - this % n_columns_in_grains(g) + 1
            do while (ini .ge. 1_8)
                franges%frange = (/ini, fin/)
                this%feature_subsets(g)%feature_ranges = [this%feature_subsets(g)%feature_ranges, franges]
                ini = ini - this%hparam%step_size_for_multi_grain
                fin = fin - this%hparam%step_size_for_multi_grain
                counter = counter + 1
                max_n_est = max_n_est + 1
            end do
            n_columns_to_be_created_per_grain(g) = counter * n_outputs * n_forest_type
            franges = this%feature_subsets(g)%feature_ranges(counter)
            if ( franges%frange(1) .ne. 1_8 ) then
                franges%frange = (/1_8, this % n_columns_in_grains(g)/)
                this%feature_subsets(g)%feature_ranges = [this%feature_subsets(g)%feature_ranges, franges]
                n_columns_to_be_created_per_grain(g) = n_columns_to_be_created_per_grain(g) + n_outputs * n_forest_type
                max_n_est = max_n_est + 1
            end if
        end do
        n_new_columns = num2char(sum(n_columns_to_be_created_per_grain))
        msg = "Total Number of Columns to be created is " // trim(n_new_columns) // "("
        do g=1, this%n_grains-1, 1
            n_new_columns = num2char(n_columns_to_be_created_per_grain(g))
            msg = trim(msg) // trim(n_new_columns) // ", "
        end do
        n_new_columns = num2char(n_columns_to_be_created_per_grain(this%n_grains))
        msg = trim(msg) // trim(n_new_columns) // "). "
        print*, msg

        allocate(feature_indices(n_columns))
        do n=1, n_columns, 1
            feature_indices(n) = n
        end do
        call permutation(feature_indices, n_columns)

        print*, '============================================================='
        print*, '============================================================='
        print*, "Multi-Grained Scanning"
        lidx = 1
        print*, "Fitting", max_n_est
        allocate(this%rf_mgs(0))
        allocate(this%et_mgs(0))
        do g=1, this%n_grains, 1
            this%hparam%max_features = -1
            do f=1, size(this%feature_subsets(g)%feature_ranges), 1
                call progress_bar(lidx, max_n_est, 1_8)
                rf = random_forest_regressor(& 
                    n_estimators=this%hparam%n_estimators, & 
                    max_depth=this%hparam%max_depth, & 
                    boot_strap=this%hparam%boot_strap, & 
                    max_leaf_nodes=this%hparam%max_leaf_nodes, & 
                    min_samples_leaf=this%hparam%min_samples_leaf, & 
                    fashion=this%hparam%fashion, & 
                    max_features=this%hparam%max_features)
                call rf%fit(data_holder_ptr, &
                    feature_indices = feature_indices, &
                    feature_indices_scanning_range = this%feature_subsets(g)%feature_ranges(f)%frange)

                et = extra_trees_regressor(& 
                    n_estimators=this%hparam%n_estimators, & 
                    max_depth=this%hparam%max_depth, & 
                    boot_strap=this%hparam%boot_strap, & 
                    max_leaf_nodes=this%hparam%max_leaf_nodes, & 
                    min_samples_leaf=this%hparam%min_samples_leaf, & 
                    fashion=this%hparam%fashion, & 
                    max_features=this%hparam%max_features)
                call et%fit(data_holder_ptr, &
                    feature_indices = feature_indices, &
                    feature_indices_scanning_range = this%feature_subsets(g)%feature_ranges(f)%frange)
                lidx = lidx + 1

                this%rf_mgs = [this%rf_mgs, rf]
                this%et_mgs = [this%et_mgs, et]
            end do
        end do

        print*, '============================================================='
        print*, '============================================================='
        print*, "Creating Stacking Features from Multi-Grained Scanning Layer"
        n_columns_to_be_created = sum(n_columns_to_be_created_per_grain)
        n_columns_to_be_created_add_cascade = n_columns_to_be_created& 
                                            + this % hparam % n_forest_per_layer * n_outputs * n_forest_type
        print*, "   check number of columns: ", n_columns_to_be_created, n_columns_to_be_created_add_cascade
        allocate(x_input_copy(n_samples, n_columns))
        lidx = 1
        do lidx=1, n_columns, 1
            x_input_copy(:,lidx) = data_holder_ptr%x_ptr%x_r8_ptr(:,lidx)
        end do
        allocate(x_stacked_fatures(n_samples, n_columns_to_be_created_add_cascade))
        lidx = 1
        do lidx=1, max_n_est, 1
            call progress_bar(lidx, max_n_est, 1_8)
            x_stacked_fatures(:,lidx:lidx)     = this % rf_mgs(lidx) % predict(data_holder_ptr % x_ptr % x_r8_ptr)
            x_stacked_fatures(:,lidx+1:lidx+1) = this % et_mgs(lidx) % predict(data_holder_ptr % x_ptr % x_r8_ptr)
        end do
        data_holder_ptr % x_ptr % x_r8_ptr => x_stacked_fatures

        print*, '============================================================='
        print*, '============================================================='
        print*, "Fitting Cascade Forest"
        deallocate(feature_indices)
        allocate(feature_indices(n_columns_to_be_created_add_cascade))
        do g=1, n_columns_to_be_created_add_cascade, 1
            feature_indices(g) = g
        end do
        franges%frange = (/1_8, n_columns_to_be_created/)

        data_holder_ptr % n_columns         =  n_columns_to_be_created_add_cascade
        data_holder_ptr % x_shape           =  (/n_samples, n_columns_to_be_created_add_cascade/)

        allocate(this % rf_cas(this % hparam % n_cascades, this % hparam % n_forest_per_layer))
        allocate(this % et_cas(this % hparam % n_cascades, this % hparam % n_forest_per_layer))
        do layer=1, this % hparam % n_cascades
            if (layer .gt. 1_8) franges%frange = (/1_8, n_columns_to_be_created_add_cascade/)
            do stack=1, this % hparam % n_forest_per_layer, 1
                rf = random_forest_regressor(& 
                    n_estimators=this%hparam%n_estimators, & 
                    max_depth=this%hparam%max_depth, & 
                    boot_strap=this%hparam%boot_strap, & 
                    max_leaf_nodes=this%hparam%max_leaf_nodes, & 
                    min_samples_leaf=this%hparam%min_samples_leaf, & 
                    fashion=this%hparam%fashion, & 
                    max_features=this%hparam%max_features)
                call rf%fit(data_holder_ptr, &
                    feature_indices = feature_indices, &
                    feature_indices_scanning_range = franges%frange)

                et = extra_trees_regressor(& 
                    n_estimators=this%hparam%n_estimators, & 
                    max_depth=this%hparam%max_depth, & 
                    boot_strap=this%hparam%boot_strap, & 
                    max_leaf_nodes=this%hparam%max_leaf_nodes, & 
                    min_samples_leaf=this%hparam%min_samples_leaf, & 
                    fashion=this%hparam%fashion, & 
                    max_features=this%hparam%max_features)
                call et%fit(data_holder_ptr, &
                    feature_indices = feature_indices, &
                    feature_indices_scanning_range = franges%frange)
                this % rf_cas(layer, stack) = rf
                this % et_cas(layer, stack) = et
            end do

            fid = n_columns_to_be_created
            do stack=0, this % hparam % n_forest_per_layer-1, 1
                print*, stack
                x_stacked_fatures(:,fid+stack:fid+stack)     & 
                    = this % rf_cas(layer, stack+1) % predict(data_holder_ptr % x_ptr % x_r8_ptr)
                x_stacked_fatures(:,fid+stack+1:fid+stack+1) & 
                    = this % et_cas(layer, stack+1) % predict(data_holder_ptr % x_ptr % x_r8_ptr)
            end do
            call permutation(feature_indices, n_columns_to_be_created_add_cascade)
        end do


        print*, '============================================================='
        print*, '============================================================='
        print*, "Fitting Output Layer"
        allocate(this % rf_out(this % hparam % n_forest_per_layer))
        allocate(this % et_out(this % hparam % n_forest_per_layer))
        do g=1, this % hparam % n_forest_per_layer
                rf = random_forest_regressor(& 
                    n_estimators=this%hparam%n_estimators, & 
                    max_depth=this%hparam%max_depth, & 
                    boot_strap=this%hparam%boot_strap, & 
                    max_leaf_nodes=this%hparam%max_leaf_nodes, & 
                    min_samples_leaf=this%hparam%min_samples_leaf, & 
                    fashion=this%hparam%fashion, & 
                    max_features=this%hparam%max_features)
                call rf%fit(data_holder_ptr)

                et = extra_trees_regressor(& 
                    n_estimators=this%hparam%n_estimators, & 
                    max_depth=this%hparam%max_depth, & 
                    boot_strap=this%hparam%boot_strap, & 
                    max_leaf_nodes=this%hparam%max_leaf_nodes, & 
                    min_samples_leaf=this%hparam%min_samples_leaf, & 
                    fashion=this%hparam%fashion, & 
                    max_features=this%hparam%max_features)
                call et%fit(data_holder_ptr)

                this % rf_out(g) = rf
                this % et_out(g) = et
        end do

        print*, '============================================================='
        print*, '============================================================='
        print*, "Check Output"
        allocate(y_out(n_samples, 1))
        y_out = 0d0
        do g=1, this % hparam % n_forest_per_layer
                y_out = y_out + this % rf_out(g) % predict(data_holder_ptr % x_ptr % x_r8_ptr)
                y_out = y_out + this % et_out(g) % predict(data_holder_ptr % x_ptr % x_r8_ptr)
        end do
        y_out = y_out / dble(this % hparam % n_forest_per_layer) / dble(n_forest_type)
        print*, metric%mean_square_error(data_holder_ptr % y_ptr % y_r8_ptr(:,1), y_out(:,1))

    end subroutine fit_mgrain_df_regressor
    



end module mod_deep_forest
