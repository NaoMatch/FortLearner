module mod_sliq
    use iso_fortran_env
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
    
    type, extends(base_tree) :: sliq_regressor
    contains
        procedure :: fit => fit_sliq_regressor
        procedure :: predict => predict_sliq_regressor
        procedure :: dump => dump_sliq_regressor
        procedure :: load => load_sliq_regressor
    end type sliq_regressor

    interface sliq_regressor
        module procedure :: new_sliq_regressor
    end interface sliq_regressor

contains

    function new_sliq_regressor(&
        max_depth, &
        boot_strap, &
        max_leaf_nodes, &
        min_samples_leaf, &
        max_features)
        implicit none
        type(sliq_regressor) :: new_sliq_regressor
        type(sliq_regressor) :: tmp
        integer(kind=8), optional :: max_depth
        logical(kind=4), optional :: boot_strap
        integer(kind=8), optional :: max_leaf_nodes
        integer(kind=8), optional :: min_samples_leaf
        integer(kind=8), optional :: max_features

        tmp%is_axis_parallel = t_
        tmp%hparam%algo_name = "sliq_regressor"
        tmp%algo_name = tmp%hparam%algo_name

        if ( present(max_depth) ) tmp%hparam%max_depth = max_depth
        if ( present(boot_strap) ) tmp%hparam%boot_strap = boot_strap
        if ( present(max_leaf_nodes) ) tmp%hparam%max_leaf_nodes = max_leaf_nodes
        if ( present(min_samples_leaf) ) tmp%hparam%min_samples_leaf = min_samples_leaf
        if ( present(max_features) ) tmp%hparam%max_features = max_features
        tmp%hparam%fashion = "level"

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
        new_sliq_regressor = tmp
    end function new_sliq_regressor

    subroutine fit_sliq_regressor(this, data_holder_ptr, print_node, &
        feature_indices, feature_indices_scanning_range)
        implicit none
        integer(kind=8)        :: date_value1(8), date_value2(8)
        class(sliq_regressor) :: this
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

        include "./include/set_feature_indices_and_scanning_range.f90"
        call date_and_time(values=date_value1)
        call data_holder_ptr % preprocess_presort()
        call date_and_time(values=date_value2)
        ! print*, "Presort: ", time_diff(date_value1, date_value2)

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
            call splitter%split_sliq_regressor(selected_node_ptrs, data_holder_ptr, hparam_ptr, &
                n_columns, feature_indices_, feature_indices_scanning_range_, is_permute_per_node)
            call this%adopt_node_ptrs_axis(selected_node_ptrs, data_holder_ptr, hparam_ptr, & 
                this%is_classification, &
                this%is_threshold_tree, this%lr_layer)

            is_stop = this%induction_stop_check(hparam_ptr)
            if (is_stop) exit
            depth = depth + 1
        end do
        call termination_node_ptr_axis(this%root_node_axis_ptr)
        call this%postprocess(this%is_classification)

        if ( present(print_node) ) then
            if ( print_node ) then
                call this%print_info(this%root_node_axis_ptr)
            end if
        end if

        this % is_trained = t_
    end subroutine fit_sliq_regressor


    !> A function to predict regression for 'x'.
    !! \return predicted values
    !! \param x input    
    function predict_sliq_regressor(this, x)
        implicit none
        class(sliq_regressor)    :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), ALLOCATABLE :: predict_sliq_regressor(:,:)
        predict_sliq_regressor = this%predict_response(x)
    end function predict_sliq_regressor


    !> A subroutine to dump trained model.
    !! \param file_name output file name.
    subroutine dump_sliq_regressor(this, file_name)
        implicit none
        class(sliq_regressor)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        open(newunit=newunit, file=file_name, form="unformatted", status="replace")
        call this%dump_base_tree(newunit)
        close(newunit)
    end subroutine dump_sliq_regressor


    !> A subroutine to load trained model.
    !! \param file_name load file name.
    subroutine load_sliq_regressor(this, file_name)
        implicit none
        class(sliq_regressor)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        open(newunit=newunit, file=file_name, form="unformatted")
        call this%load_base_tree(newunit)
        close(newunit)
    end subroutine load_sliq_regressor

end module mod_sliq