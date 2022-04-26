module mod_oblivious_tree
    use mod_const
    use mod_common
    use mod_error
    use mod_random
    use mod_sort
    use mod_stats
    use mod_timer

    use mod_hyperparameter
    use mod_node
    use mod_woodworking_tools
    use mod_splitter
    use mod_base_tree
    implicit none

    type, extends(base_tree) :: oblivious_tree_regressor
    contains
        procedure :: fit => fit_oblivious_tree_regressor
        procedure :: predict => predict_oblivious_tree_regressor
        procedure :: dump => dump_oblivious_tree_regressor
        procedure :: load => load_oblivious_tree_regressor
    end type oblivious_tree_regressor

    interface oblivious_tree_regressor
        procedure :: new_oblivious_tree_regressor
    end interface oblivious_tree_regressor
    
contains
    
    function new_oblivious_tree_regressor(max_depth, min_samples_leaf)
        implicit none
        type(oblivious_tree_regressor) :: new_oblivious_tree_regressor
        type(oblivious_tree_regressor) :: tmp
        integer(kind=8), optional :: max_depth
        integer(kind=8), optional :: min_samples_leaf
        character(len=256)        :: fashion_list(5)

        tmp%is_axis_parallel = t_
        tmp%hparam%algo_name = "oblivious_tree_regressor"
        tmp%algo_name = tmp%hparam%algo_name

        fashion_list(1) = "best"
        fashion_list(2) = "depth"
        fashion_list(3) = "level"
        fashion_list(4) = "impurity"
        fashion_list(5) = "sample"

        if ( present(max_depth) ) tmp%hparam%max_depth = max_depth
        if ( present(min_samples_leaf) ) tmp%hparam%min_samples_leaf = min_samples_leaf
        tmp%hparam%fashion = "level"

        call tmp%hparam%validate_int_range("max_depth",        tmp%hparam%max_depth,        1_8, huge(1_8))
        call tmp%hparam%validate_int_range("min_samples_leaf", tmp%hparam%min_samples_leaf, 1_8, huge(1_8))

        tmp%hparam%fashion_int = tmp%hparam%convert_char_to_int(tmp%hparam%fashion, fashion_list)
        tmp%hparam%max_samples = -1
        tmp%is_hist = f_
        tmp%is_layer_wise_sum = f_
        tmp%lr_layer = 0d0
        tmp%is_classification = f_
        new_oblivious_tree_regressor = tmp
    end function new_oblivious_tree_regressor

    subroutine fit_oblivious_tree_regressor(this, data_holder_ptr, print_node, &
        feature_indices, feature_indices_scanning_range)
        implicit none

        integer(kind=8)        :: date_value1(8), date_value2(8)
        class(oblivious_tree_regressor) :: this
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
        call date_and_time(values=date_value1)
        call data_holder_ptr % preprocess_presort()
        call date_and_time(values=date_value2)
        ! print*, "Presort: ", time_diff(date_value1, date_value2)
        
        call date_and_time(values=date_value1)
        call data_holder_ptr % preprocess_y_sq()
        call date_and_time(values=date_value2)
        ! print*, "SQSUM:  ", time_diff(date_value1, date_value2)
        
        call this%init(data_holder_ptr)

        if (associated(this%root_node_axis_ptr)) nullify(this%root_node_axis_ptr)
        this%root_node_axis_ptr => root_node
        call this%init_root_node(data_holder_ptr, is_classification=this%is_classification)

        hparam = this%hparam
        hparam_ptr => hparam
        call this%root_node_axis_ptr%hparam_check(hparam_ptr)
        call this%induction_stop_check(hparam_ptr, is_stop)
        if ( is_stop ) return

        depth = 1
        do while (t_)
            is_stop = f_
            if (allocated(selected_node_ptrs)) deallocate(selected_node_ptrs)
            allocate(selected_node_ptrs(0))

            call this%extract_split_node_ptrs_axis(selected_node_ptrs, depth)

            call splitter%split_oblivious_tree_regressor(selected_node_ptrs, data_holder_ptr, hparam_ptr)
            call this%adopt_node_ptrs_axis(selected_node_ptrs, data_holder_ptr, hparam_ptr, this%is_classification, &
                this%is_threshold_tree, this%lr_layer)
            call this%induction_stop_check(hparam_ptr, is_stop)
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
    end subroutine fit_oblivious_tree_regressor

    function predict_oblivious_tree_regressor(this, x)
        implicit none
        class(oblivious_tree_regressor)    :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), ALLOCATABLE :: predict_oblivious_tree_regressor(:,:)
        predict_oblivious_tree_regressor = this%predict_response(x)
    end function predict_oblivious_tree_regressor


    subroutine dump_oblivious_tree_regressor(this, file_name)
        implicit none
        class(oblivious_tree_regressor)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        open(newunit=newunit, file=file_name, form="unformatted", status="replace")
        call this%dump_base_tree(newunit)
        close(newunit)
    end subroutine dump_oblivious_tree_regressor


    subroutine load_oblivious_tree_regressor(this, file_name)
        implicit none
        class(oblivious_tree_regressor)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        open(newunit=newunit, file=file_name, form="unformatted")
        call this%load_base_tree(newunit)
        close(newunit)
    end subroutine load_oblivious_tree_regressor

end module mod_oblivious_tree