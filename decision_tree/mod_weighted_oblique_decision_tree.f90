module mod_weighted_oblique_decision_tree
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
    type, extends(base_tree) :: weighted_oblique_decision_tree_classifier
        logical(kind=4) :: is_classification=t_
    contains
        procedure :: fit => fit_weighted_oblique_decision_tree_classifier
    end type weighted_oblique_decision_tree_classifier

    interface weighted_oblique_decision_tree_classifier
        procedure :: new_weighted_oblique_decision_tree_classifier
    end interface weighted_oblique_decision_tree_classifier

contains

    !> A function to override weighted_oblique_decision_tree_classifier.
    !! \param max_depth max depth
    !! \param boot_strap boot strap sampling
    !! \param max_leaf_nodes maximum number of leaf node
    !! \param min_samples_leaf minimum number of samples in node
    !! \param fashion how to split node
    !! \param max_features maximum number of features in node split phase
    function new_weighted_oblique_decision_tree_classifier(&
        max_depth, boot_strap, max_leaf_nodes, min_samples_leaf, fashion, max_features &
        )
        implicit none
        type(weighted_oblique_decision_tree_classifier) :: new_weighted_oblique_decision_tree_classifier
        type(weighted_oblique_decision_tree_classifier) :: tmp
        integer(kind=8), optional :: max_depth
        logical(kind=4), optional :: boot_strap
        integer(kind=8), optional :: max_leaf_nodes
        integer(kind=8), optional :: min_samples_leaf
        character(len=*), optional :: fashion
        integer(kind=8), optional :: max_features

        tmp%is_axis_parallel = f_
        tmp%hparam%algo_name = "weighted_oblique_decision_tree_classifier"

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

        tmp%hparam%max_samples = -1
        tmp%hparam%fashion_int = tmp%hparam%convert_char_to_int(tmp%hparam%fashion, fashion_list)
        tmp%is_hist = f_
        tmp%is_layer_wise_sum = f_
        tmp%lr_layer = 0d0
        new_weighted_oblique_decision_tree_classifier = tmp
    end function new_weighted_oblique_decision_tree_classifier


    !> A subtouine to fit regressor of 'decision_tree'. 
    !! \return returns fitted regressor tree
    !! \param data_holder_ptr pointer of data_holder 
    !! \param print_node ***OPTIONAL*** if True, print node informations
    !! \param feature_indices ***OPTIONAL*** Order of features given by hand for 'DeepForest'
    !! \param feature_indices_scanning_range ***OPTIONAL*** The index of the range to be used in the "Tree" when "feature_indices" is given.
    subroutine fit_weighted_oblique_decision_tree_classifier(this, data_holder_ptr, print_node, &
        feature_indices, feature_indices_scanning_range)
        implicit none

        class(weighted_oblique_decision_tree_classifier) :: this
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
        integer(kind=8) :: depth, n_columns, n
        integer(kind=8), allocatable :: feature_indices_(:), feature_indices_scanning_range_(:)

        include "./include/set_feature_indices_and_scanning_range.f90"

        call this%init(data_holder_ptr)

        if (associated(this%root_node_axis_ptr)) nullify(this%root_node_axis_ptr)
        this%root_node_axis_ptr => root_node
        call this%init_root_node(data_holder_ptr, is_classification=this%is_classification)

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
            call splitter%split_weighted_oblique_decision_tree_classifier(selected_node_ptrs, data_holder_ptr, hparam_ptr, &
                n_columns, feature_indices_, feature_indices_scanning_range_, is_permute_per_node)
            call this%adopt_node_ptrs_axis(selected_node_ptrs, data_holder_ptr, hparam_ptr, this%is_classification, &
                this%lr_layer)

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
    end subroutine fit_weighted_oblique_decision_tree_classifier

end module mod_weighted_oblique_decision_tree
