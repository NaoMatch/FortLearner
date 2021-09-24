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


    !> Extended type of regressor of 'classificaton and regression tree'
    type, extends(base_tree) :: isolation_tree
        logical(kind=4) :: is_classification=f_
    contains
        procedure :: fit => fit_isolation_tree
    end type isolation_tree

    interface isolation_tree
        procedure :: new_isolation_tree
    end interface isolation_tree

contains

    function new_isolation_tree(max_samples, contamination)
        implicit none
        type(isolation_tree)      :: new_isolation_tree
        type(isolation_tree)      :: tmp
        integer(kind=8), optional :: max_samples
        real(kind=8), optional    :: contamination

        tmp%is_axis_parallel = t_
        tmp%hparam%algo_name = "isolation_tree"
        tmp % algo_name = tmp%hparam%algo_name

        if ( present(max_samples) )   tmp%hparam%max_samples   = max_samples
        if ( present(contamination) ) tmp%hparam%contamination = contamination

        call tmp%hparam%validate_int_range("max_samples", tmp%hparam%max_samples, 2_8, huge(1_8))
        call tmp%hparam%validate_real_range("contamination", tmp%hparam%contamination, epsilon_, .5d0)

        tmp%hparam%boot_strap = t_
        tmp%hparam%fashion_int = 2_8
        tmp%is_hist = f_
        tmp%is_layer_wise_sum = f_
        tmp%lr_layer = 0d0
        tmp%is_isolation_tree = t_
        new_isolation_tree = tmp
    end function new_isolation_tree

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

        ! Overwrite Hyperparameters
        this%hparam%max_samples = minval((/this%hparam%max_samples, data_holder_ptr%n_samples/))
        this%hparam%max_depth = ceiling(log2(this%hparam%max_samples))

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
            ! print*, "depth: ", depth
            if (allocated(selected_node_ptrs)) deallocate(selected_node_ptrs)
            allocate(selected_node_ptrs(0))

            call this%extract_split_node_ptrs_axis(selected_node_ptrs, depth)
            call splitter%split_isolation_tree(selected_node_ptrs, data_holder_ptr, hparam_ptr, &
                n_columns)
            call this%adopt_node_ptrs_axis_for_isolation_tree(selected_node_ptrs, data_holder_ptr, hparam_ptr)
            call this%induction_stop_check(hparam_ptr, is_stop)
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


end module mod_isolation_tree