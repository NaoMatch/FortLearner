module mod_decision_tree
    use mod_const
    use mod_common
    use mod_error
    use mod_random
    use mod_sort
    use mod_stats
    use mod_timer

    use mod_hyperparameter, only: hparam_decisiontree, fashion_list
    use mod_node
    use mod_woodworking_tools
    use mod_splitter
    use mod_base_tree
    implicit none

    !> Extended type of regressor of 'classificaton and regression tree'
    !> https://www.routledge.com/Classification-and-Regression-Trees/Breiman-Friedman-Stone-Olshen/p/book/9780412048418
    type, extends(base_tree) :: decision_tree_regressor
    contains
        procedure :: fit => fit_decision_tree_regressor
        procedure :: predict => predict_decision_tree_regressor
        procedure :: dump => dump_decision_tree_regressor
        procedure :: load => load_decision_tree_regressor
    end type decision_tree_regressor

    !> An interface to create new 'decision_tree_regressor'
    interface decision_tree_regressor
        procedure :: new_decision_tree_regressor
    end interface decision_tree_regressor

contains

    !> A function to create new 'decision_tree_regressor'.
    !! \param max_depth max depth. must be greater equal 1
    !! \param boot_strap with or without bootstrap sampling. default value is .false.
    !! \param max_leaf_nodes maximum number of leaf node. must be greater equal 2
    !! \param min_samples_leaf minimum number of samples in node. must be greater than 1
    !! \param fashion how to split node. 'best': split the node with largest information gain, 'depth': split the deepest splittable(not leaf) node, 
    !!        'level': split all specific depth nodes at a time, 'impurity': split the node with largest impurity, 
    !!        'sample': split the node with largest sample size
    !! \param max_features maximum number of features in node split phase. must be greater equal 1
    function new_decision_tree_regressor(&
        max_depth, boot_strap, max_leaf_nodes, min_samples_leaf, fashion, max_features &
        )
        implicit none
        type(decision_tree_regressor) :: new_decision_tree_regressor
        type(decision_tree_regressor) :: tmp
        integer(kind=8), optional :: max_depth
        logical(kind=4), optional :: boot_strap
        integer(kind=8), optional :: max_leaf_nodes
        integer(kind=8), optional :: min_samples_leaf
        character(len=*), optional :: fashion
        integer(kind=8), optional :: max_features

        tmp%is_axis_parallel = t_
        tmp%hparam%algo_name = "decision_tree_regressor"
        tmp % algo_name = tmp%hparam%algo_name

        if ( present(max_depth) ) tmp%hparam%max_depth = max_depth
        if ( present(boot_strap) ) tmp%hparam%boot_strap = boot_strap
        if ( present(max_leaf_nodes) ) tmp%hparam%max_leaf_nodes = max_leaf_nodes
        if ( present(min_samples_leaf) ) tmp%hparam%min_samples_leaf = min_samples_leaf
        if ( present(fashion) ) tmp%hparam%fashion = fashion
        if ( present(max_features) ) tmp%hparam%max_features = max_features

        call tmp%hparam%validate_int_range("max_depth",        tmp%hparam%max_depth,        1_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_leaf_nodes",   tmp%hparam%max_leaf_nodes,   2_8, huge(1_8))
        call tmp%hparam%validate_int_range("min_samples_leaf", tmp%hparam%min_samples_leaf, 1_8, huge(1_8))
        call tmp%hparam%validate_char_list("fashion",          tmp%hparam%fashion,          fashion_list)
        call tmp%hparam%validate_int_range("max_features",     tmp%hparam%max_features,     1_8, huge(1_8), exception=-1_8)

        tmp%hparam%fashion_int = tmp%hparam%convert_char_to_int(tmp%hparam%fashion, fashion_list)
        tmp%hparam%max_samples = -1
        tmp%is_hist = f_
        tmp%is_layer_wise_sum = f_
        tmp%lr_layer = 0d0
        tmp%is_classification = f_
        new_decision_tree_regressor = tmp
    end function new_decision_tree_regressor


    !> A subtouine to fit 'decision_tree_regressor'. 
    !! \return returns fitted 'decision_tree_regressor'
    !! \param data_holder_ptr pointer of data_holder 
    !! \param print_node ***OPTIONAL*** if True, print node informations after training
    !! \param feature_indices ***OPTIONAL*** Order of features given by hand for 'DeepForest'
    !! \param feature_indices_scanning_range ***OPTIONAL*** The index of the range to be used in the "Tree" when "feature_indices" is given.
    subroutine fit_decision_tree_regressor(this, dholder, print_node, &
        feature_indices, feature_indices_scanning_range, sample_indices)
        implicit none

        class(decision_tree_regressor) :: this
        type(data_holder), target      :: dholder
        logical(kind=4), OPTIONAL      :: print_node
        integer(kind=8), optional      :: feature_indices(:)
        integer(kind=8), optional      :: feature_indices_scanning_range(2)
        integer(kind=8), optional      :: sample_indices(:)
        
        type(data_holder), pointer     :: data_holder_ptr
        type(node_axis), target            :: root_node
        type(hparam_decisiontree), target  :: hparam
        type(hparam_decisiontree), pointer :: hparam_ptr
        type(node_axis_ptr), allocatable   :: selected_node_ptrs(:)
        logical(kind=4)                    :: is_permute_per_node

        logical(kind=4) :: is_stop
        type(node_splitter) :: splitter
        integer(kind=8) :: depth, n_columns, n
        integer(kind=8), allocatable :: feature_indices_(:), feature_indices_scanning_range_(:)

        data_holder_ptr => dholder
        include "./include/set_feature_indices_and_scanning_range.f90"

        call this%init(data_holder_ptr)

        if (associated(this%root_node_axis_ptr)) nullify(this%root_node_axis_ptr)
        this%root_node_axis_ptr => root_node
        if (present(sample_indices)) then
            call this%init_root_node(data_holder_ptr, is_classification=this%is_classification, sample_indices=sample_indices)
        else
            call this%init_root_node(data_holder_ptr, is_classification=this%is_classification)
        end if

        hparam = this%hparam
        hparam_ptr => hparam
        call this%root_node_axis_ptr%hparam_check(hparam_ptr)
        is_stop = this%induction_stop_check(hparam_ptr)
        if ( is_stop ) return

        depth = 1
        do while (t_)
            is_stop = f_
            if (allocated(selected_node_ptrs)) deallocate(selected_node_ptrs)
            allocate(selected_node_ptrs(0))

            call this%extract_split_node_ptrs_axis(selected_node_ptrs, depth)
            call splitter%split_decision_tree_regressor(selected_node_ptrs, data_holder_ptr, hparam_ptr, &
                n_columns, feature_indices_, feature_indices_scanning_range_, is_permute_per_node)
            call this%adopt_node_ptrs_axis(selected_node_ptrs, data_holder_ptr, hparam_ptr, this%is_classification, &
                this%is_threshold_tree, this%lr_layer)

            is_stop = this%induction_stop_check(hparam_ptr)
            if (is_stop) exit
            depth = depth + 1
        end do
        call termination_node_ptr_axis(this%root_node_axis_ptr)
        
        if ( present(print_node) ) then
            if ( print_node ) then
                call this%print_info(this%root_node_axis_ptr)
            end if
        end if

        call this%postprocess(this%is_classification)
        this%is_trained = t_
    end subroutine fit_decision_tree_regressor


    !> A function to predict regression for 'x'.
    !! \return predicted values
    !! \param x input
    function predict_decision_tree_regressor(this, x, parallel)
        implicit none
        class(decision_tree_regressor)    :: this
        real(kind=8), intent(in) :: x(:,:)
        real(kind=8), ALLOCATABLE :: predict_decision_tree_regressor(:,:)
        logical(kind=4), optional :: parallel
        logical(kind=4) :: parallel_opt
        parallel_opt = f_
        if (present(parallel)) parallel_opt = parallel
        predict_decision_tree_regressor = this%predict_response(x, parallel=parallel_opt)
    end function predict_decision_tree_regressor


    !> A subroutine to dump trained model.
    !! \param file_name output file name.
    subroutine dump_decision_tree_regressor(this, file_name)
        implicit none
        class(decision_tree_regressor)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        open(newunit=newunit, file=file_name, form="unformatted", status="replace")
        call this%dump_base_tree(newunit)
        close(newunit)
    end subroutine dump_decision_tree_regressor


    !> A subroutine to load trained model.
    !! \param file_name load file name.
    subroutine load_decision_tree_regressor(this, file_name)
        implicit none
        class(decision_tree_regressor)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        open(newunit=newunit, file=file_name, form="unformatted")
        call this%load_base_tree(newunit)
        close(newunit)
    end subroutine load_decision_tree_regressor


end module mod_decision_tree
