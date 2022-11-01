module mod_clouds
    use iso_fortran_env
    use mod_const
    use mod_common
    use mod_error
    use mod_random
    use mod_sort
    use mod_stats
    use mod_timer

    use mod_hyperparameter, only: hparam_decisiontree, fashion_list, strategy_list
    use mod_node
    use mod_woodworking_tools
    use mod_splitter
    use mod_base_tree
    implicit none

    !> Extended type of regressor of 'clouds'
    !> https://www.researchgate.net/publication/2424791_CLOUDS_A_decision_tree_classifier_for_large_datasets
    type, extends(base_tree) ::  clouds_regressor
    contains
        procedure :: fit => fit_clouds_regressor
        procedure :: predict => predict_clouds_regressor
        procedure :: dump => dump_clouds_regressor
        procedure :: load => load_clouds_regressor
    end type clouds_regressor
    
    !> An interface to create new 'clouds_regressor'
    interface clouds_regressor
        module procedure :: new_clouds_regressor
    end interface clouds_regressor

contains

    !> A function to create new 'clouds_regressor'.
    !! \param max_bins maximum number of bins. must be greater equal 2
    !! \param max_depth max depth. must be greater equal 1
    !! \param boot_strap with or without bootstrap sampling. default value is .false.
    !! \param max_leaf_nodes maximum number of leaf node. must be greater equal 2
    !! \param min_samples_leaf minimum number of samples in node. must be greater than 1
    !! \param fashion how to split node. 'best': split the node with largest information gain, 'depth': split the deepest splittable(not leaf) node, 
    !!        'level': split all specific depth nodes at a time, 'impurity': split the node with largest impurity, 
    !!        'sample': split the node with largest sample size
    !! \param max_features maximum number of features in node split phase. must be greater equal 1
    !! \param strategy binning strategy. 
    !!      'uniform': divide uniformly between the maximum and minimum values, 
    !!      'quantile': split according to quantile values, 
    !!      'kmeans': split by one-dimensional kmeans clustering, 
    !!      'greedy': see https://arxiv.org/pdf/2005.01653.pdf, 
    !!      'modified_greedy': see https://arxiv.org/pdf/2005.01653.pdf
    function new_clouds_regressor(&
        max_bins, &
        max_depth, &
        boot_strap, &
        max_leaf_nodes, &
        min_samples_leaf, &
        fashion, &
        max_features, &
        strategy)
        implicit none
        type(clouds_regressor) :: new_clouds_regressor
        type(clouds_regressor) :: tmp
        integer(kind=8), optional :: max_bins
        integer(kind=8), optional :: max_depth
        logical(kind=4), optional :: boot_strap
        integer(kind=8), optional :: max_leaf_nodes
        integer(kind=8), optional :: min_samples_leaf
        character(len=*), optional :: fashion
        integer(kind=8), optional :: max_features
        character(len=*), optional :: strategy

        tmp%is_axis_parallel = t_
        tmp%hparam%algo_name = "clouds_regressor"
        tmp%algo_name = "clouds_regressor"

        if ( present(max_bins) ) tmp%hparam%max_bins = max_bins
        if ( present(max_depth) ) tmp%hparam%max_depth = max_depth
        if ( present(boot_strap) ) tmp%hparam%boot_strap = boot_strap
        if ( present(max_leaf_nodes) ) tmp%hparam%max_leaf_nodes = max_leaf_nodes
        if ( present(min_samples_leaf) ) tmp%hparam%min_samples_leaf = min_samples_leaf
        if ( present(fashion) ) tmp%hparam%fashion = fashion
        if ( present(max_features) ) tmp%hparam%max_features = max_features
        if ( present(strategy) ) tmp%hparam%strategy = strategy

        call tmp%hparam%validate_int_range("max_bins",        tmp%hparam%max_bins,        2_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_depth",        tmp%hparam%max_depth,        1_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_leaf_nodes",   tmp%hparam%max_leaf_nodes,   2_8, huge(1_8))
        call tmp%hparam%validate_int_range("min_samples_leaf", tmp%hparam%min_samples_leaf, 1_8, huge(1_8))
        call tmp%hparam%validate_char_list("fashion",          tmp%hparam%fashion,          fashion_list)
        call tmp%hparam%validate_int_range("max_features",     tmp%hparam%max_features,     1_8, huge(1_8), exception=-1_8)

        tmp%hparam%max_samples = -1
        tmp%hparam%fashion_int = tmp%hparam%convert_char_to_int(tmp%hparam%fashion, fashion_list)
        tmp%hparam%strategy_int = tmp%hparam%convert_char_to_int(tmp%hparam%strategy, strategy_list)
        tmp%is_hist = t_
        tmp%is_layer_wise_sum = f_
        tmp%lr_layer = 0d0
        tmp%is_classification = f_
        new_clouds_regressor = tmp
    end function new_clouds_regressor


    !> A subtouine to fit 'clouds_regressor'. 
    !! \return returns fitted 'clouds_regressor'
    !! \param data_holder_ptr pointer of data_holder 
    !! \param print_node ***OPTIONAL*** if True, print node informations after training
    !! \param feature_indices ***OPTIONAL*** Order of features given by hand for 'DeepForest'
    !! \param feature_indices_scanning_range ***OPTIONAL*** The index of the range to be used in the "Tree" when "feature_indices" is given.
    subroutine fit_clouds_regressor(this, data_holder_ptr, print_node, &
        feature_indices, feature_indices_scanning_range)
        implicit none

        class(clouds_regressor) :: this
        type(data_holder), pointer     :: data_holder_ptr
        logical(kind=4), OPTIONAL      :: print_node
        integer(kind=8), optional      :: feature_indices(:)
        integer(kind=8), optional      :: feature_indices_scanning_range(2)

        type(node_axis), target            :: root_node
        type(hparam_decisiontree), target  :: hparam
        type(hparam_decisiontree), pointer :: hparam_ptr
        type(node_axis_ptr), allocatable   :: selected_node_ptrs(:)
        logical(kind=4)                    :: is_permute_per_node

        logical(kind=4) :: is_stop
        type(node_splitter) :: splitter
        integer(kind=8) :: depth, n_columns, n, i
        integer(kind=8), allocatable :: feature_indices_(:), feature_indices_scanning_range_(:)
        integer(kind=4), allocatable :: column_copy(:)

        ! print*, "feature"
        include "./include/set_feature_indices_and_scanning_range.f90"

        ! print*, "preprocess", this%hparam%max_bins, this%hparam%strategy
        call data_holder_ptr % preprocess_hist(this%hparam%max_bins, this%hparam%strategy)

        ! print*, "init"
        call this%init(data_holder_ptr)

        ! print*, "associcate"
        if (associated(this%root_node_axis_ptr)) nullify(this%root_node_axis_ptr)
        this%root_node_axis_ptr => root_node
        call this%init_root_node(data_holder_ptr, is_classification=this%is_classification)

        ! print*, "check"
        hparam = this%hparam
        hparam%max_bins = extract_max_bins(data_holder_ptr%disc)
        hparam_ptr => hparam
        call this%root_node_axis_ptr%hparam_check(hparam_ptr)
        is_stop = this%induction_stop_check(hparam_ptr)
        if ( is_stop ) return

        ! print*, "start"
        depth = 1
        do while (t_)
            ! print*, "depth: ", depth
            is_stop = f_
            if (allocated(selected_node_ptrs)) deallocate(selected_node_ptrs)
            allocate(selected_node_ptrs(0))

            call this%extract_split_node_ptrs_axis(selected_node_ptrs, depth)
            call splitter%split_clouds_regressor(selected_node_ptrs, data_holder_ptr, hparam_ptr, &
                n_columns, feature_indices_, feature_indices_scanning_range_, is_permute_per_node)
            call this%adopt_node_ptrs_axis(selected_node_ptrs, data_holder_ptr, hparam_ptr, &
                this%is_classification, this%is_threshold_tree, this%lr_layer, is_hist=t_)

            is_stop = this%induction_stop_check(hparam_ptr)
            if (is_stop) exit
            depth = depth + 1
        end do
        call termination_node_ptr_axis(this%root_node_axis_ptr)
        

        call this%postprocess(this%is_classification)
        call convert_thresholds_discretized_to_raw(& 
            this%results%split_thresholds_, this%results%split_features_, this%results%is_terminals_, &
            data_holder_ptr%disc)

        if ( present(print_node) ) then
            if ( print_node ) then
                call this%print_info(this%root_node_axis_ptr)
            end if
        end if

        this % is_trained = t_
    end subroutine fit_clouds_regressor


    !> A function to predict regression for 'x'.
    !! \return predicted values
    !! \param x input
    function predict_clouds_regressor(this, x)
        implicit none
        class(clouds_regressor)    :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), ALLOCATABLE :: predict_clouds_regressor(:,:)
        predict_clouds_regressor = this%predict_response(x)
    end function predict_clouds_regressor


    !> A subroutine to dump trained model.
    !! \param file_name output file name.
    subroutine dump_clouds_regressor(this, file_name)
        implicit none
        class(clouds_regressor)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        open(newunit=newunit, file=file_name, form="unformatted", status="replace")
        call this%dump_base_tree(newunit)
        close(newunit)
    end subroutine dump_clouds_regressor


    !> A subroutine to load trained model.
    !! \param file_name load file name.
    subroutine load_clouds_regressor(this, file_name)
        implicit none
        class(clouds_regressor)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        open(newunit=newunit, file=file_name, form="unformatted")
        call this%load_base_tree(newunit)
        close(newunit)
    end subroutine load_clouds_regressor


end module mod_clouds
