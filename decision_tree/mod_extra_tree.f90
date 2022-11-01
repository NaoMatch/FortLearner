module mod_extra_tree
    use mod_const
    use mod_common
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

    !> Extended type of regressor of 'extremely randomized tree' or 'extra tree'
    !> https://link.springer.com/article/10.1007/s10994-006-6226-1
    type, extends(base_tree) :: extra_tree_regressor
    contains
        ! procedure :: fit => fit_extra_tree_regressor
        procedure :: fit => fit_extra_tree_regressor
        procedure :: fit_faster => fit_extra_tree_regressor_faster
        procedure :: predict => predict_extra_tree_regressor
        procedure :: dump => dump_extra_tree_regressor
        procedure :: load => load_extra_tree_regressor
    end type extra_tree_regressor

    !> An interface to create new 'extra_tree_regressor'
    interface extra_tree_regressor
        procedure :: new_extra_tree_regressor
    end interface extra_tree_regressor

contains

    !> A function to override extra_tree_regressor.
    !! \param max_depth max depth. must be greater equal 1
    !! \param boot_strap with or without bootstrap sampling. default value is .false.
    !! \param max_leaf_nodes maximum number of leaf node. must be greater equal 2
    !! \param min_samples_leaf minimum number of samples in node. must be greater than 1
    !! \param fashion how to split node. 'best': split the node with largest information gain, 'depth': split the deepest splittable(not leaf) node, 
    !!        'level': split all specific depth nodes at a time, 'impurity': split the node with largest impurity, 
    !!        'sample': split the node with largest sample size
    !! \param max_features maximum number of features in node split phase. must be greater equal 1
    !! \param n_repeats number of iterations to find the best split extremely randomly at a node
    !! \param n_threads number of threads
    function new_extra_tree_regressor(&
        max_depth, boot_strap, max_leaf_nodes, min_samples_leaf, fashion, max_features, n_repeats, &
        n_threads &
        )
        implicit none
        type(extra_tree_regressor) :: new_extra_tree_regressor
        type(extra_tree_regressor) :: tmp
        integer(kind=8), optional :: max_depth
        logical(kind=4), optional :: boot_strap
        integer(kind=8), optional :: max_leaf_nodes
        integer(kind=8), optional :: min_samples_leaf
        character(len=*), optional :: fashion
        integer(kind=8), optional :: max_features
        integer(kind=8), optional :: n_repeats
        integer(kind=8), optional :: n_threads

        tmp%is_axis_parallel = t_
        tmp%hparam%algo_name = "extra_tree_regressor"
        tmp % algo_name = tmp%hparam%algo_name

        if ( present(max_depth) ) tmp%hparam%max_depth = max_depth
        if ( present(boot_strap) ) tmp%hparam%boot_strap = boot_strap
        if ( present(max_leaf_nodes) ) tmp%hparam%max_leaf_nodes = max_leaf_nodes
        if ( present(min_samples_leaf) ) tmp%hparam%min_samples_leaf = min_samples_leaf
        if ( present(fashion) ) tmp%hparam%fashion = fashion
        if ( present(max_features) ) tmp%hparam%max_features = max_features
        if ( present(n_repeats) ) tmp%hparam%n_repeats = n_repeats
        if ( present(n_threads) ) tmp%hparam%num_threads_in_node = n_threads

        call tmp%hparam%validate_int_range("max_depth",        tmp%hparam%max_depth,        1_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_leaf_nodes",   tmp%hparam%max_leaf_nodes,   2_8, huge(1_8))
        call tmp%hparam%validate_int_range("min_samples_leaf", tmp%hparam%min_samples_leaf, 1_8, huge(1_8))
        call tmp%hparam%validate_char_list("fashion",          tmp%hparam%fashion,          fashion_list)
        call tmp%hparam%validate_int_range("max_features",     tmp%hparam%max_features,     1_8, huge(1_8), exception=-1_8)
        call tmp%hparam%validate_int_range("n_repeats",        tmp%hparam%n_repeats,        1_8, huge(1_8))
        call tmp%hparam%validate_int_range("n_threads",        tmp%hparam%num_threads_in_node,        1_8, huge(1_8))

        tmp%hparam%max_samples = -1
        tmp%hparam%fashion_int = tmp%hparam%convert_char_to_int(tmp%hparam%fashion, fashion_list)
        tmp%is_hist = f_
        tmp%is_layer_wise_sum = f_
        tmp%lr_layer = 0d0
        tmp%is_classification = f_
        new_extra_tree_regressor = tmp
    end function new_extra_tree_regressor


    !> A subtouine to fit 'extra_tree_regressor'. 
    !! \return returns fitted 'extra_tree_regressor' tree
    !! \param data_holder_ptr pointer of data_holder 
    !! \param print_node ***OPTIONAL*** if True, print node informations after training
    !! \param feature_indices ***OPTIONAL*** Order of features given by hand for 'DeepForest'
    !! \param feature_indices_scanning_range ***OPTIONAL*** The index of the range to be used in the "Tree" when "feature_indices" is given.
    subroutine fit_extra_tree_regressor(this, data_holder_ptr, print_node, &
        feature_indices, feature_indices_scanning_range)
        implicit none

        class(extra_tree_regressor) :: this
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
        integer(kind=8) :: date_value1(8), date_value2(8)
        integer(kind=8), save :: time_splti=0

        include "./include/set_feature_indices_and_scanning_range.f90"

        call this%init(data_holder_ptr)

        if (associated(this%root_node_axis_ptr)) nullify(this%root_node_axis_ptr)
        this%root_node_axis_ptr => root_node
        call this%init_root_node(data_holder_ptr, is_classification=this%is_classification)

        hparam = this%hparam
        hparam_ptr => hparam
        call this%root_node_axis_ptr%hparam_check(hparam_ptr)
        is_stop = this%induction_stop_check(hparam_ptr)


        depth = 1
        do while (t_)
            is_stop = f_
            if (allocated(selected_node_ptrs)) deallocate(selected_node_ptrs)
            allocate(selected_node_ptrs(0))

            call this%extract_split_node_ptrs_axis(selected_node_ptrs, depth)

            call date_and_time(values=date_value1)
            call splitter%split_extra_tree_regressor(selected_node_ptrs, data_holder_ptr, hparam_ptr, &
                n_columns, feature_indices_, feature_indices_scanning_range_, is_permute_per_node)
            call date_and_time(values=date_value2)
            time_splti = time_splti + time_diff(date_value1, date_value2)

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
        this % is_trained = t_
        ! print*, "SplitTime: ", time_splti
    end subroutine fit_extra_tree_regressor


    !> A subtouine to fit 'extra_tree_regressor', faster than 'fit_extra_tree_regressor'. 
    !> input explanatory of data_holder must be transposed and number of output dimension of objective function must be 1.
    !! \return returns fitted 'extra_tree_regressor' tree
    !! \param data_holder_ptr pointer of data_holder 
    !! \param print_node ***OPTIONAL*** if True, print node informations after training
    !! \param feature_indices ***OPTIONAL*** Order of features given by hand for 'DeepForest'
    !! \param feature_indices_scanning_range ***OPTIONAL*** The index of the range to be used in the "Tree" when "feature_indices" is given.
    subroutine fit_extra_tree_regressor_faster(this, data_holder_ptr, print_node, &
        feature_indices, feature_indices_scanning_range)
        implicit none

        class(extra_tree_regressor) :: this
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
        integer(kind=8) :: date_value1(8), date_value2(8)
        integer(kind=8), save :: time_splti=0

        include "./include/set_feature_indices_and_scanning_range.f90"

        if (.not. data_holder_ptr % is_trans_x) then
            stop " 'is_trans_x' of 'data_holder' must be '.true.' when using 'fit_extra_tree_regressor_faster'"
        end if

        call this%init(data_holder_ptr)

        if (associated(this%root_node_axis_ptr)) nullify(this%root_node_axis_ptr)
        this%root_node_axis_ptr => root_node
        call this%init_root_node(data_holder_ptr, is_classification=this%is_classification)

        hparam = this%hparam
        hparam_ptr => hparam
        call this%root_node_axis_ptr%hparam_check(hparam_ptr)
        is_stop = this%induction_stop_check(hparam_ptr)


        depth = 1
        do while (t_)
            is_stop = f_
            if (allocated(selected_node_ptrs)) deallocate(selected_node_ptrs)
            allocate(selected_node_ptrs(0))

            call this%extract_split_node_ptrs_axis(selected_node_ptrs, depth)

            call date_and_time(values=date_value1)
            call splitter%split_extra_tree_regressor_faster(selected_node_ptrs, data_holder_ptr, hparam_ptr, &
                n_columns, feature_indices_, feature_indices_scanning_range_, is_permute_per_node)
            call date_and_time(values=date_value2)
            time_splti = time_splti + time_diff(date_value1, date_value2)


            call this%adopt_node_ptrs_axis(selected_node_ptrs, data_holder_ptr, hparam_ptr, this%is_classification, &
                this%is_threshold_tree, this%lr_layer)

            is_stop = this%induction_stop_check(hparam_ptr)
            depth = depth + 1
        end do
        call termination_node_ptr_axis(this%root_node_axis_ptr)
        
        if ( present(print_node) ) then
            if ( print_node ) then
                call this%print_info(this%root_node_axis_ptr)
            end if
        end if

        call this%postprocess(this%is_classification)

        ! print*, "SplitTime: ", time_splti
    end subroutine fit_extra_tree_regressor_faster


    !> A function to predict regression for 'x'.
    !! \return predicted values
    !! \param x input
    function predict_extra_tree_regressor(this, x)
        implicit none
        class(extra_tree_regressor)    :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), ALLOCATABLE :: predict_extra_tree_regressor(:,:)
        predict_extra_tree_regressor = this%predict_response(x)
    end function predict_extra_tree_regressor


    !> A subroutine to dump trained model.
    !! \param file_name output file name.
    subroutine dump_extra_tree_regressor(this, file_name)
        implicit none
        class(extra_tree_regressor)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        open(newunit=newunit, file=file_name, form="unformatted", status="replace")
        call this%dump_base_tree(newunit)
        close(newunit)
    end subroutine dump_extra_tree_regressor


    !> A subroutine to load trained model.
    !! \param file_name load file name.
    subroutine load_extra_tree_regressor(this, file_name)
        implicit none
        class(extra_tree_regressor)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        open(newunit=newunit, file=file_name, form="unformatted")
        call this%load_base_tree(newunit)
        close(newunit)
    end subroutine load_extra_tree_regressor

    
end module mod_extra_tree
