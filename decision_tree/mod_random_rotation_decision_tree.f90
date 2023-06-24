module mod_random_rotation_decision_tree
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
    
    type, extends(base_tree) :: random_rotation_decision_tree_regressor
    contains
        procedure :: fit => fit_random_rotation_decision_tree_regressor
        procedure :: predict => predict_random_rotation_decision_tree_regressor
        procedure :: dump => dump_random_rotation_decision_tree_regressor
        procedure :: load => load_random_rotation_decision_tree_regressor
    end type random_rotation_decision_tree_regressor

    interface random_rotation_decision_tree_regressor
        procedure new_random_rotation_decision_tree_regressor
    end interface random_rotation_decision_tree_regressor


contains


    function new_random_rotation_decision_tree_regressor(&
        max_depth, max_leaf_nodes, min_samples_leaf, fashion &
        )
        implicit none

        type(random_rotation_decision_tree_regressor) :: new_random_rotation_decision_tree_regressor
        type(random_rotation_decision_tree_regressor) :: tmp

        integer(kind=8),  optional :: max_depth
        integer(kind=8),  optional :: max_leaf_nodes
        integer(kind=8),  optional :: min_samples_leaf
        character(len=*), optional :: fashion

        tmp%is_axis_parallel = f_
        tmp%hparam%algo_name = "random_rotation_decision_tree_regressor"
        tmp % algo_name = tmp%hparam%algo_name

        if ( present(max_depth) ) tmp%hparam%max_depth = max_depth
        if ( present(max_leaf_nodes) ) tmp%hparam%max_leaf_nodes = max_leaf_nodes
        if ( present(min_samples_leaf) ) tmp%hparam%min_samples_leaf = min_samples_leaf
        if ( present(fashion) ) tmp%hparam%fashion = fashion

        call tmp%hparam%validate_int_range("max_depth",            tmp%hparam%max_depth,        1_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_leaf_nodes",       tmp%hparam%max_leaf_nodes,   2_8, huge(1_8))
        call tmp%hparam%validate_int_range("min_samples_leaf",     tmp%hparam%min_samples_leaf, 1_8, huge(1_8))
        call tmp%hparam%validate_char_list("fashion",              tmp%hparam%fashion,          fashion_list)

        tmp%hparam%max_samples = -1
        tmp%hparam%fashion_int = tmp%hparam%convert_char_to_int(tmp%hparam%fashion, fashion_list)
        tmp%is_hist = f_
        tmp%is_layer_wise_sum = f_
        tmp%lr_layer = 0d0
        tmp%is_classification = f_

        new_random_rotation_decision_tree_regressor = tmp
    end function new_random_rotation_decision_tree_regressor

    subroutine fit_random_rotation_decision_tree_regressor(this, dholder, print_node)
        implicit none
        class(random_rotation_decision_tree_regressor) :: this
        type(data_holder), target     :: dholder
        logical(kind=4), OPTIONAL      :: print_node
        
        type(data_holder), pointer     :: data_holder_ptr
        type(node_oblq), target            :: root_node
        type(hparam_decisiontree), target  :: hparam
        type(hparam_decisiontree), pointer :: hparam_ptr
        type(node_oblq_ptr), allocatable   :: selected_node_ptrs(:)
        logical(kind=4)                    :: is_permute_per_node

        logical(kind=4) :: is_stop
        type(node_splitter) :: splitter
        integer(kind=8) :: depth, n_columns, n
        integer(kind=8), allocatable :: feature_indices_(:), feature_indices_scanning_range_(:)

        data_holder_ptr => dholder

        call data_holder_ptr%preprocess_random_rotate_new(this%rr_works, this%rr_mat_r8)

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
            call splitter%split_random_rotation_tree_regressor( &
                selected_node_ptrs, data_holder_ptr, hparam_ptr)
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
    end subroutine fit_random_rotation_decision_tree_regressor


    !> A function to predict regression for 'x'.
    !! \return predicted values
    !! \param x input    
    function predict_random_rotation_decision_tree_regressor(this, x)
        implicit none
        class(random_rotation_decision_tree_regressor)    :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), ALLOCATABLE :: predict_random_rotation_decision_tree_regressor(:,:)

        predict_random_rotation_decision_tree_regressor = this%predict_response(x)
    end function predict_random_rotation_decision_tree_regressor


    !> A subroutine to dump trained model.
    !! \param file_name output file name.
    subroutine dump_random_rotation_decision_tree_regressor(this, file_name)
        implicit none
        class(random_rotation_decision_tree_regressor)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        open(newunit=newunit, file=file_name, form="unformatted", status="replace")
        call this%dump_base_tree(newunit)
        close(newunit)
    end subroutine dump_random_rotation_decision_tree_regressor


    !> A subroutine to load trained model.
    !! \param file_name load file name.
    subroutine load_random_rotation_decision_tree_regressor(this, file_name)
        implicit none
        class(random_rotation_decision_tree_regressor)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        open(newunit=newunit, file=file_name, form="unformatted")
        call this%load_base_tree(newunit)
        close(newunit)
    end subroutine load_random_rotation_decision_tree_regressor






end module mod_random_rotation_decision_tree