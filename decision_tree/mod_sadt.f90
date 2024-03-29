module mod_sadt
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

    !> Extended type of regressor of 'simulated annealing decision tree'
    type, extends(base_tree) :: sadt_regressor
    contains
        procedure :: fit => fit_sadt_regressor
        procedure :: predict => predict_sadt_regressor
        procedure :: dump => dump_sadt_regressor
        procedure :: load => load_sadt_regressor
    end type sadt_regressor

    !> An interface to create new 'sadt_regressor'
    interface sadt_regressor
        procedure :: new_sadt_regressor
    end interface sadt_regressor

contains

    !> A function to create sadt_regressor.
    !! \param max_depth max depth. must be greater equal 1
    !! \param boot_strap with or without bootstrap sampling. default value is .false.
    !! \param max_leaf_nodes maximum number of leaf node. must be greater equal 2
    !! \param min_samples_leaf minimum number of samples in node. must be greater than 1
    !! \param fashion how to split node. 'best': split the node with largest information gain, 'depth': split the deepest splittable(not leaf) node, 
    !!        'level': split all specific depth nodes at a time, 'impurity': split the node with largest impurity, 
    !!        'sample': split the node with largest sample size
    !! \param max_features maximum number of features in node split phase. must be greater equal 1
    !! \param initial_temperature initial temperature of simulated annealing at a node
    !! \param max_epoch maximum number of epoch of simulated annealing at a node
    function new_sadt_regressor(&
        max_depth, boot_strap, max_leaf_nodes, min_samples_leaf, fashion, max_features, &
        initial_temperature, max_epoch, cooling_rate &
        )
        implicit none
        type(sadt_regressor)       :: new_sadt_regressor
        type(sadt_regressor)       :: tmp

        integer(kind=8),  optional :: max_depth
        logical(kind=4),  optional :: boot_strap
        integer(kind=8),  optional :: max_leaf_nodes
        integer(kind=8),  optional :: min_samples_leaf
        character(len=*), optional :: fashion
        integer(kind=8),  optional :: max_features
        real(kind=8),     optional :: initial_temperature
        integer(kind=8),  optional :: max_epoch
        real(kind=8),     optional :: cooling_rate

        tmp%is_axis_parallel = f_
        tmp%hparam%algo_name = "sadt_regressor"
        tmp % algo_name = tmp%hparam%algo_name

        if ( present(max_depth) ) tmp%hparam%max_depth = max_depth
        if ( present(boot_strap) ) tmp%hparam%boot_strap = boot_strap
        if ( present(max_leaf_nodes) ) tmp%hparam%max_leaf_nodes = max_leaf_nodes
        if ( present(min_samples_leaf) ) tmp%hparam%min_samples_leaf = min_samples_leaf
        if ( present(fashion) ) tmp%hparam%fashion = fashion
        if ( present(max_features) ) tmp%hparam%max_features = max_features
        if ( present(initial_temperature) ) tmp%hparam%initial_temperature = initial_temperature
        if ( present(max_epoch) ) tmp%hparam%max_epoch = max_epoch
        if ( present(cooling_rate) ) tmp%hparam%cooling_rate = cooling_rate

        call tmp%hparam%validate_int_range("max_depth",            tmp%hparam%max_depth,        1_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_leaf_nodes",       tmp%hparam%max_leaf_nodes,   2_8, huge(1_8))
        call tmp%hparam%validate_int_range("min_samples_leaf",     tmp%hparam%min_samples_leaf, 1_8, huge(1_8))
        call tmp%hparam%validate_char_list("fashion",              tmp%hparam%fashion,          fashion_list)
        call tmp%hparam%validate_int_range("max_features",         tmp%hparam%max_features,     1_8, huge(1_8), exception=-1_8)
        call tmp%hparam%validate_real_range("initial_temperature", tmp%hparam%initial_temperature, epsilon_, huge(epsilon_))
        call tmp%hparam%validate_int_range("max_epoch",            tmp%hparam%max_epoch,        1_8, huge(1_8))
        call tmp%hparam%validate_real_range("cooling_rate",        tmp%hparam%cooling_rate,     epsilon_, 1d0-epsilon_)

        tmp%hparam%max_samples = -1
        tmp%hparam%fashion_int = tmp%hparam%convert_char_to_int(tmp%hparam%fashion, fashion_list)
        tmp%is_hist = f_
        tmp%is_layer_wise_sum = f_
        tmp%lr_layer = 0d0
        tmp%is_classification = f_
        new_sadt_regressor = tmp
    end function new_sadt_regressor


    !> A subtouine to fit 'sadt_regressor'. 
    !! \return returns fitted 'sadt_regressor'
    !! \param data_holder_ptr pointer of data_holder 
    !! \param print_node ***OPTIONAL*** if True, print node informations
    subroutine fit_sadt_regressor(this, data_holder_ptr, print_node)
        implicit none

        class(sadt_regressor) :: this
        type(data_holder), pointer     :: data_holder_ptr
        logical(kind=4), OPTIONAL      :: print_node

        type(node_oblq), target            :: root_node
        type(hparam_decisiontree), target  :: hparam
        type(hparam_decisiontree), pointer :: hparam_ptr
        type(node_oblq_ptr), allocatable   :: selected_node_ptrs(:)
        logical(kind=4)                    :: is_permute_per_node

        logical(kind=4) :: is_stop
        type(node_splitter) :: splitter
        integer(kind=8) :: depth, n_columns, n
        integer(kind=8), allocatable :: feature_indices_(:), feature_indices_scanning_range_(:)

        ! include "./include/set_feature_indices_and_scanning_range.f90"

        call this%init(data_holder_ptr)

        if (associated(this%root_node_oblq_ptr)) nullify(this%root_node_oblq_ptr)
        this%root_node_oblq_ptr => root_node
        call this%init_root_node(data_holder_ptr, is_classification=this%is_classification)

        hparam = this%hparam
        hparam_ptr => hparam
        call this%root_node_oblq_ptr%hparam_check(hparam_ptr)
        is_stop = this%induction_stop_check(hparam_ptr)
        if ( is_stop ) return

        depth = 1
        do while (t_)
            is_stop = f_
            if (allocated(selected_node_ptrs)) deallocate(selected_node_ptrs)
            allocate(selected_node_ptrs(0))

            ! print*, "Extract"
            call this%extract_split_node_ptrs_oblq(selected_node_ptrs, depth)

            ! print*, "Split"
            call splitter%split_sadt_regressor(selected_node_ptrs, data_holder_ptr, hparam_ptr, &
                n_columns, feature_indices_, feature_indices_scanning_range_, is_permute_per_node)
            call this%adopt_node_ptrs_oblq(selected_node_ptrs, data_holder_ptr, hparam_ptr, this%is_classification, &
                this%lr_layer)

            is_stop = this%induction_stop_check(hparam_ptr)
            if (is_stop) exit
            depth = depth + 1
        end do
        call termination_node_ptr(this%root_node_oblq_ptr)
        
        if ( present(print_node) ) then
            if ( print_node ) then
                call this%print_info_oblq(this%root_node_oblq_ptr)
            end if
        end if

        call this%postprocess(this%is_classification)
        this%is_trained = t_
    end subroutine fit_sadt_regressor


    !> A function to predict regression for 'x'.
    !! \return predicted values
    !! \param x input
    function predict_sadt_regressor(this, x)
        implicit none
        class(sadt_regressor)    :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), ALLOCATABLE :: predict_sadt_regressor(:,:)
        predict_sadt_regressor = this%predict_response(x)
    end function predict_sadt_regressor


    !> A subroutine to dump trained model.
    !! \param file_name output file name.
    subroutine dump_sadt_regressor(this, file_name)
        implicit none
        class(sadt_regressor)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        stop "NotImplementedError: 'dump_sadt_regressor' is not implemented." 
    end subroutine dump_sadt_regressor


    !> A subroutine to load trained model.
    !! \param file_name load file name.
    subroutine load_sadt_regressor(this, file_name)
        implicit none
        class(sadt_regressor)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        stop "NotImplementedError: 'load_sadt_regressor' is not implemented." 
    end subroutine load_sadt_regressor    


end module mod_sadt