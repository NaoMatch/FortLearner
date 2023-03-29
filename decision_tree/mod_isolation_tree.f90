module mod_isolation_tree
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


    !> Extended type of 'isolation_tree'
    type, extends(base_tree) :: isolation_tree
    contains
        procedure :: fit => fit_isolation_tree
        procedure :: predict => predict_isolation_tree
        procedure :: dump => dump_isolation_tree
        procedure :: load => load_isolation_tree
    end type isolation_tree

    !> An interface to create new 'isolation_tree'
    interface isolation_tree
        procedure :: new_isolation_tree
    end interface isolation_tree

contains

    !> A function to create new 'isolation_tree'.
    !! \param max_samples maximum number of samples in tree. must be greater equal 2
    !! \param contamination contamination ratio
    function new_isolation_tree(max_samples, max_features, feature_selection, split_selection, depth)
        implicit none
        type(isolation_tree)       :: new_isolation_tree
        type(isolation_tree)       :: tmp
        integer(kind=8), optional  :: max_samples
        integer(kind=8), optional  :: max_features
        character(len=*), optional :: feature_selection
        character(len=*), optional :: split_selection
        character(len=*), optional :: depth

        tmp%is_axis_parallel = t_
        tmp%hparam%algo_name = "isolation_tree"
        tmp % algo_name = tmp%hparam%algo_name

        if ( present(max_samples) )   tmp%hparam%max_samples   = max_samples
        if ( present(max_features) )   tmp%hparam%max_features   = max_features
        if ( present(feature_selection) ) tmp%hparam%feature_selection = feature_selection
        if ( present(split_selection) ) tmp%hparam%split_selection = split_selection
        if ( present(depth) ) tmp%hparam%depth = depth

        call tmp%hparam%validate_int_range("max_samples",       tmp%hparam%max_samples, 2_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_features",       tmp%hparam%max_features, 1_8, huge(1_8))
        call tmp%hparam%validate_char_list("feature_selection", tmp%hparam%feature_selection, feature_selection_list)
        call tmp%hparam%validate_char_list("split_selection", tmp%hparam%split_selection, split_selection_list)
        call tmp%hparam%validate_char_list("depth", tmp%hparam%depth, depth_list)

        tmp%hparam%boot_strap = t_
        tmp%hparam%fashion_int = 2_8
        tmp%hparam%feature_selection_int = tmp%hparam%convert_char_to_int(tmp%hparam%feature_selection, feature_selection_list)
        tmp%hparam%split_selection_int = tmp%hparam%convert_char_to_int(tmp%hparam%split_selection, split_selection_list)
        tmp%hparam%depth_int = tmp%hparam%convert_char_to_int(tmp%hparam%depth, depth_list)
        tmp%is_hist = f_
        tmp%is_layer_wise_sum = f_
        tmp%lr_layer = 0d0
        tmp%is_isolation_tree = t_
        tmp%is_classification = f_
        new_isolation_tree = tmp
    end function new_isolation_tree


    !> A subtouine to fit 'isolation_tree'. 
    !! \return returns fitted 'isolation_tree' tree
    !! \param data_holder_ptr pointer of data_holder 
    !! \param print_node ***OPTIONAL*** if True, print node informations after training
    subroutine fit_isolation_tree(this, data_holder_ptr, print_node)
        implicit none

        class(isolation_tree)          :: this

        type(data_holder), pointer     :: data_holder_ptr
        logical(kind=4), optional      :: print_node

        type(node_axis), target            :: root_node
        type(hparam_decisiontree), target  :: hparam
        type(hparam_decisiontree), pointer :: hparam_ptr
        type(node_axis_ptr), allocatable   :: selected_node_ptrs(:)
        logical(kind=4)                    :: is_permute_per_node

        logical(kind=4)              :: is_stop
        type(node_splitter)          :: splitter
        integer(kind=8)              :: depth, n_columns, n
        integer(kind=8) :: debug
        debug = 0

        ! Overwrite Hyperparameters
        this%hparam%max_samples = minval((/this%hparam%max_samples, data_holder_ptr%n_samples/))
        this%hparam%max_features = minval((/this%hparam%max_features, data_holder_ptr%n_columns/))
        if (this%hparam%depth_int==1_8) then
            this%hparam%max_depth = ceiling(log2(this%hparam%max_samples))
        else
            this%hparam%max_depth = huge(1_8)
        end if
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
            ! print*, "depth: ", depth
            if (allocated(selected_node_ptrs)) deallocate(selected_node_ptrs)
            allocate(selected_node_ptrs(0))
            call this%extract_split_node_ptrs_axis(selected_node_ptrs, depth)
            call splitter%split_isolation_tree(selected_node_ptrs, data_holder_ptr, hparam_ptr, &
                n_columns)
            call this%adopt_node_ptrs_axis_for_isolation_tree(selected_node_ptrs, data_holder_ptr, hparam_ptr)
            is_stop = this%induction_stop_check(hparam_ptr)
            ! print*, "is_stop: ", is_stop
            if (is_stop) exit
            depth = depth + 1
        end do

        ! print*, "termination_node_ptr_axis"
        call termination_node_ptr_axis(this%root_node_axis_ptr)
        if ( present(print_node) ) then
            if ( print_node ) then
                call this%print_info(this%root_node_axis_ptr)
            end if
        end if
        ! print*, "postprocess"
        call this%postprocess(this%is_classification)
        this%is_trained = t_
    end subroutine fit_isolation_tree


    !> A function to predict anomaly score for 'x'.
    !! \return predicted values
    !! \param x input
    function predict_isolation_tree(this, x, return_depth)
        implicit none
        class(isolation_tree)    :: this
        real(kind=8), intent(in) :: x(:,:)
        real(kind=8), ALLOCATABLE :: predict_isolation_tree(:,:)
        logical(kind=4), optional :: return_depth
        if (present(return_depth)) then
            predict_isolation_tree = this%predict_response(x, return_depth=return_depth)
        else
            predict_isolation_tree = this%predict_response(x)
        end if
    end function predict_isolation_tree


    !> A subroutine to dump trained model.
    !! \param file_name output file name.
    subroutine dump_isolation_tree(this, file_name)
        implicit none
        class(isolation_tree)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        open(newunit=newunit, file=file_name, form="unformatted", status="replace")
        call this%dump_base_tree(newunit)
        close(newunit)
    end subroutine dump_isolation_tree


    !> A subroutine to load trained model.
    !! \param file_name load file name.
    subroutine load_isolation_tree(this, file_name)
        implicit none
        class(isolation_tree)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        open(newunit=newunit, file=file_name, form="unformatted")
        call this%load_base_tree(newunit)
        close(newunit)
    end subroutine load_isolation_tree


end module mod_isolation_tree
