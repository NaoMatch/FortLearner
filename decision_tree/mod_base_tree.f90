module mod_base_tree
    use mod_const
    use mod_common
    use mod_node
    use mod_hyperparameter
    use mod_random
    use mod_sort
    use mod_woodworking_tools
    implicit none
    
    type base_tree
        character(len=256)               :: algo_name
        logical(kind=4)                  :: is_trained = f_
        logical(kind=4)                  :: is_axis_parallel = t_
        logical(kind=4)                  :: is_hist=f_
        logical(kind=4)                  :: is_layer_wise_sum=f_
        type(hparam_decisiontree)        :: hparam
        ! Axis-Parallel
        type(node_axis), pointer         :: root_node_axis_ptr
        integer(kind=8), allocatable     :: split_features_(:)
        ! Oblique
        type(node_oblq), pointer         :: root_node_oblq_ptr
        real(kind=8), allocatable        :: coefs_(:,:)
        ! Common
        real(kind=8), allocatable        :: split_thresholds_(:)
        logical(kind=4), allocatable     :: is_terminals_(:)
        ! Classification and Regression
        real(kind=8), allocatable        :: responses_(:,:)
        ! 
        type(train_results) :: results
        real(kind=8), allocatable        :: mean_y(:)
        real(kind=8)                     :: lr_layer

        integer(kind=8) :: n_samples_
        integer(kind=8) :: n_columns_
        integer(kind=8) :: n_outputs_
        integer(kind=8) :: n_leaf_nodes_
    contains
        procedure :: init => init_base_tree
        procedure :: induction_stop_check
        procedure :: init_root_node
        procedure :: postprocess
        procedure :: predict => predict_response
        procedure :: extract_split_node_ptrs_axis
        ! procedure :: extract_split_node_ptrs_oblq
        procedure :: adopt_node_ptrs_axis
        ! procedure :: adopt_node_ptrs_oblq

        procedure :: dump => dump_tree
        procedure :: load => load_tree

        procedure :: print_info
    end type base_tree

contains

    subroutine dump_tree(this, file_name)
        implicit none
        class(base_tree) :: this
        character(len=*) :: file_name
        integer(kind=8)  :: n_nodes, n_feature_fractions
        integer(kind=8)  :: dummy
        open(10, file=file_name, form="unformatted", status="replace")
        if (.not. this % is_trained) then
            print*, trim(this % algo_name),  " is not trained. Cannot dump model."
            write(10) f_ ! dump fail
            close(10)
            stop
        end if
        write(10) t_ ! dump fail
        write(10) this%algo_name
        write(10) this%is_axis_parallel
        write(10) this%is_hist
        write(10) this%is_layer_wise_sum
        ! 
        ! 
        dummy = allocated(this%hparam%feature_fractions)
        n_feature_fractions = size(this%hparam%feature_fractions) * dummy
        write(10) n_feature_fractions
        write(10) this%hparam%n_estimators
        write(10) this%hparam%criterion_int
        write(10) this%hparam%criterion_boost_int
        write(10) this%hparam%verbose
        write(10) this%hparam%max_epoch
        write(10) this%hparam%max_patient
        write(10) this%hparam%max_retry
        write(10) this%hparam%max_alpha
        write(10) this%hparam%min_alpha
        write(10) this%hparam%n_repeats
        write(10) this%hparam%n_rounds
        write(10) this%hparam%max_bins
        write(10) this%hparam%strategy_int
        write(10) this%hparam%max_depth
        write(10) this%hparam%min_samples_split
        write(10) this%hparam%min_samples_leaf
        write(10) this%hparam%max_features
        write(10) this%hparam%max_feature_use
        write(10) this%hparam%max_leaf_nodes
        write(10) this%hparam%fashion_int
        write(10) this%hparam%print_mod
        write(10) this%hparam%num_threads_in_node
        write(10) this%hparam%num_threads_in_forest
        write(10) this%hparam%step_size_for_multi_grain
        write(10) this%hparam%min_columns_in_grain
        write(10) this%hparam%n_cascades
        write(10) this%hparam%n_forest_per_layer
        if (n_feature_fractions .gt. 0_8) then
            write(10) this%hparam%feature_fractions
        else
            write(10) 0_8
        end if
        write(10) this%hparam%learning_rate
        write(10) this%hparam%learning_rate_layer
        write(10) this%hparam%drop_rate
        write(10) this%hparam%update_ratio
        write(10) this%hparam%momentum
        write(10) this%hparam%prunig_threshold
        write(10) this%hparam%weight_decay
        write(10) this%hparam%top_ratio
        write(10) this%hparam%min_weight_fraction_leaf
        write(10) this%hparam%min_impurity_decrease
        write(10) this%hparam%other_ratio
        write(10) this%hparam%lambda1
        write(10) this%hparam%lambda2
        write(10) this%hparam%row_sampling
        write(10) this%hparam%skip_used_features
        write(10) this%hparam%boot_strap
        write(10) this%hparam%random_splitter
        write(10) this%hparam%criterion
        write(10) this%hparam%strategy
        write(10) this%hparam%fashion
        
        n_nodes = size(this%results%split_features_)
        write(10) n_nodes
        write(10) this%results%n_columns_
        write(10) this%results%n_outputs_
        if (this%is_axis_parallel) then
            write(10) this%results%split_features_
            write(10) this%results%split_thresholds_
            write(10) this%results%is_terminals_
            write(10) this%results%responses_
        else
            stop "NotImplementedError: 'dump_tree' for oblique tree is not implemented."
        end if

        if (this%is_layer_wise_sum) then
            write(10) this%mean_y
        else
            write(10) 0_8
        end if

        print*, "Dump Success!, ", trim(this % algo_name)
        close(10)
    end subroutine dump_tree

    subroutine load_tree(this, file_name)
        implicit none
        class(base_tree) :: this
        character(len=*) :: file_name
        logical(kind=4)  :: is_dump_successed, is_allocated_feature_fractions
        integer(kind=8)  :: n_nodes, n_feature_fractions, dummy, n_columns_, n_outputs_
        open(10, file=file_name, form="unformatted")
        read(10) is_dump_successed
        if (.not. is_dump_successed) then
            print*, trim(this % algo_name),  " failed dump of the model."
            stop
        end if

        this%is_trained = is_dump_successed
        read(10) this%algo_name
        read(10) this%is_axis_parallel
        read(10) this%is_hist
        read(10) this%is_layer_wise_sum
        ! 
        ! 
        read(10) n_feature_fractions
        read(10) this%hparam%n_estimators
        read(10) this%hparam%criterion_int
        read(10) this%hparam%criterion_boost_int
        read(10) this%hparam%verbose
        read(10) this%hparam%max_epoch
        read(10) this%hparam%max_patient
        read(10) this%hparam%max_retry
        read(10) this%hparam%max_alpha
        read(10) this%hparam%min_alpha
        read(10) this%hparam%n_repeats
        read(10) this%hparam%n_rounds
        read(10) this%hparam%max_bins
        read(10) this%hparam%strategy_int
        read(10) this%hparam%max_depth
        read(10) this%hparam%min_samples_split
        read(10) this%hparam%min_samples_leaf
        read(10) this%hparam%max_features
        read(10) this%hparam%max_feature_use
        read(10) this%hparam%max_leaf_nodes
        read(10) this%hparam%fashion_int
        read(10) this%hparam%print_mod
        read(10) this%hparam%num_threads_in_node
        read(10) this%hparam%num_threads_in_forest
        read(10) this%hparam%step_size_for_multi_grain
        read(10) this%hparam%min_columns_in_grain
        read(10) this%hparam%n_cascades
        read(10) this%hparam%n_forest_per_layer
        if (n_feature_fractions .gt. 0_8) then
            allocate(this%hparam%feature_fractions(n_feature_fractions))
            read(10) this%hparam%feature_fractions
        else
            read(10) dummy
        end if
        read(10) this%hparam%learning_rate
        read(10) this%hparam%learning_rate_layer
        read(10) this%hparam%drop_rate
        read(10) this%hparam%update_ratio
        read(10) this%hparam%momentum
        read(10) this%hparam%prunig_threshold
        read(10) this%hparam%weight_decay
        read(10) this%hparam%top_ratio
        read(10) this%hparam%min_weight_fraction_leaf
        read(10) this%hparam%min_impurity_decrease
        read(10) this%hparam%other_ratio
        read(10) this%hparam%lambda1
        read(10) this%hparam%lambda2
        read(10) this%hparam%row_sampling
        read(10) this%hparam%skip_used_features
        read(10) this%hparam%boot_strap
        read(10) this%hparam%random_splitter
        read(10) this%hparam%criterion
        read(10) this%hparam%strategy
        read(10) this%hparam%fashion

        read(10) n_nodes
        read(10) this%results % n_columns_
        read(10) this%results % n_outputs_
        this%n_columns_ = this%results % n_columns_
        this%n_outputs_ = this%results % n_outputs_
        if (this%is_axis_parallel) then
            allocate(this%results%split_features_(n_nodes))
            allocate(this%results%split_thresholds_(n_nodes))
            allocate(this%results%is_terminals_(n_nodes))
            allocate(this%results%responses_(n_nodes, this%results % n_outputs_))
            read(10) this%results % split_features_
            read(10) this%results % split_thresholds_
            read(10) this%results % is_terminals_
            read(10) this%results % responses_
        else
            stop "Oblique load and dump is not implemented."
        end if

        if (this%is_layer_wise_sum) then
            allocate(this%mean_y(this%n_outputs_))
            read(10) this%mean_y
        else
            read(10) dummy
        end if

        print*, "Load Success!, ", trim(this % algo_name)
        close(10); return
    end subroutine load_tree

    !> A subroutine to print node informations.
    !! \param root_node_ptr_axis root node pointer of axis-parallel
    recursive subroutine print_info(this, root_node_ptr_axis)
        implicit none
        class(base_tree) :: this
        type(node_axis), pointer :: root_node_ptr_axis
        type(node_axis), pointer :: node_l_ptr_axis, node_r_ptr_axis
        print*, "print"
        call root_node_ptr_axis%print_node_info_axis()
        if ( allocated(root_node_ptr_axis%node_l) ) then
            node_l_ptr_axis => root_node_ptr_axis%node_l
            node_r_ptr_axis => root_node_ptr_axis%node_r
            call this%print_info(node_l_ptr_axis)
            call this%print_info(node_r_ptr_axis)
        end if
    end subroutine print_info


    !> A subtouine to fit regressor of 'classificaton and regression tree'. 
    subroutine induction_stop_check(this, hparam_ptr, is_stop)
        implicit none
        class(base_tree) :: this
        type(hparam_decisiontree), pointer :: hparam_ptr
        logical(kind=4), intent(inout) :: is_stop
        integer(kind=8) :: n_leaf_nodes
        logical(kind=4) :: exist_splittable_leaf
        is_stop = f_

        ! Max Leaf Node
        if (associated(this%root_node_axis_ptr)) then
            call count_leaf_nodes(this%root_node_axis_ptr, n_leaf_nodes, is_root=t_)
        elseif (associated(this%root_node_oblq_ptr)) then
            ! call count_leaf_nodes(this%root_node_oblq_ptr, n_leaf_nodes, is_root=t_)
        end if
        this%n_leaf_nodes_ = n_leaf_nodes
        ! print*, "N_LEAF_NODES : ", n_leaf_nodes
        if (hparam_ptr%max_leaf_nodes .ne. -1_8 .and. hparam_ptr%max_leaf_nodes .le. n_leaf_nodes) then
            ! print*, "STOP: Max Leaf Node"
            is_stop = t_
            return
        end if

        ! NO SPLITTABLE LEAF
        exist_splittable_leaf = f_
        if (associated(this%root_node_axis_ptr)) then
            call check_splittable_leaf(this%root_node_axis_ptr, exist_splittable_leaf)
        elseif (associated(this%root_node_oblq_ptr)) then
            ! call check_splittable_leaf(this%root_node_oblq_ptr, exist_splittable_leaf)
        end if
        if (.not. exist_splittable_leaf) then
            ! print*, "STOP: NO SPLITTABLE LEAF"
            is_stop = t_
            return
        end if
    end subroutine induction_stop_check


    !> Initialize tree, dellocate all allocated arrays
    subroutine init_base_tree(this, data_holder_ptr)
        implicit none
        class(base_tree)           :: this
        type(data_holder), pointer :: data_holder_ptr
        if ( associated(this%root_node_axis_ptr) ) nullify(this%root_node_axis_ptr)
        if ( associated(this%root_node_oblq_ptr) ) nullify(this%root_node_oblq_ptr)

        this%is_trained = f_
        if ( allocated(this%split_features_) ) deallocate(this%split_features_)
        if ( allocated(this%coefs_) ) deallocate(this%coefs_)
        if ( allocated(this%split_thresholds_) ) deallocate(this%split_thresholds_)
        if ( allocated(this%is_terminals_) ) deallocate(this%is_terminals_)

        this%n_samples_ = data_holder_ptr%n_samples
        this%n_columns_ = data_holder_ptr%n_columns
        this%n_outputs_ = data_holder_ptr%n_outputs
    end subroutine init_base_tree


    !> A subroutine to initialize root node pointer.
    !! \return returns tree with initialized root node
    !! \param data_holder_ptr pointer of 'data_holder'
    !! \param is_classification classification tree or not
    subroutine init_root_node(this, data_holder_ptr, is_classification)
        implicit none
        class(base_tree)           :: this
        type(data_holder), pointer :: data_holder_ptr
        logical(kind=4)            :: is_classification
        integer(kind=8)            :: i

        this%n_outputs_ = data_holder_ptr%n_outputs
        if (associated(this%root_node_axis_ptr)) then
            allocate(this%root_node_axis_ptr%indices(this%n_samples_))
            allocate(this%root_node_axis_ptr%is_used(this%n_columns_))
            allocate(this%root_node_axis_ptr%is_useless(this%n_columns_))
            allocate(this%root_node_axis_ptr%sum_p(this%n_outputs_))
            this%root_node_axis_ptr%is_used = f_
            this%root_node_axis_ptr%is_useless = f_
            this%root_node_axis_ptr%is_hist = this%is_hist

            if ( this%hparam%boot_strap ) then
                call rand_integer(1_8, this%n_samples_, this%root_node_axis_ptr%indices, this%n_samples_)
                call quick_sort(this%root_node_axis_ptr%indices, this%n_samples_)
            else
                do i=1, this%n_samples_, 1
                    this%root_node_axis_ptr%indices(i) = i
                end do
            end if

            if (is_classification) then
                ! pass
            else
                this%root_node_axis_ptr%sum_p = sum(data_holder_ptr%y_ptr%y_r8_ptr, dim=1)
                this%root_node_axis_ptr%response = mean(data_holder_ptr%y_ptr%y_r8_ptr, this%n_samples_, this%n_outputs_)
                this%root_node_axis_ptr%impurity = sum(variance(data_holder_ptr%y_ptr%y_r8_ptr, this%n_samples_, this%n_outputs_, &
                                                        this%root_node_axis_ptr%response)) / dble(this%n_outputs_)
            end if
            this%root_node_axis_ptr%depth = 0_8 
            this%root_node_axis_ptr%n_columns = this%n_columns_
            this%root_node_axis_ptr%n_samples = this%n_samples_
        else
            ! allocate(this%root_node_oblq_ptr%indices(this%n_samples_))
            ! allocate(this%root_node_oblq_ptr%is_used(this%n_columns_))
            ! allocate(this%root_node_oblq_ptr%is_useless(this%n_columns_))
            ! allocate(this%root_node_oblq_ptr%sum_p(this%n_outputs_))
            ! this%root_node_oblq_ptr%is_used = f_
            ! this%root_node_oblq_ptr%is_useless = f_
            ! allocate(this%root_node_oblq_ptr%coef_(this%n_columns_))

            ! if ( this%hparam%boot_strap ) then
            !     call rand_integer(1_8, this%n_samples_, this%root_node_oblq_ptr%indices, this%n_samples_)
            !     call quick_sort(this%root_node_oblq_ptr%indices, this%n_samples_)
            ! else
            !     do i=1, this%n_samples_, 1
            !         this%root_node_oblq_ptr%indices(i) = i
            !     end do
            ! end if

            ! if (is_classification) then
            !     ! pass
            ! else
            !     this%root_node_oblq_ptr%sum_p = sum(data_holder_ptr%y_ptr%y_r8_ptr, dim=1)
            !     this%root_node_oblq_ptr%response = mean(data_holder_ptr%y_ptr%y_r8_ptr, this%n_samples_, this%n_outputs_)
            !     this%root_node_oblq_ptr%impurity = sum(variance(data_holder_ptr%y_ptr%y_r8_ptr, this%n_samples_, this%n_outputs_, &
            !                                                 this%root_node_oblq_ptr%response)) / dble(this%n_outputs_)
            ! end if
            ! this%root_node_oblq_ptr%depth = 0_8 
            ! this%root_node_oblq_ptr%n_columns = this%n_columns_
            ! this%root_node_oblq_ptr%n_samples = this%n_samples_
        end if
    end subroutine init_root_node


    !> A subroutine to extract 'axis-parallel' node pointer(s) to be split.
    !> Which nodes are extracted is determined by hyperparameter 'fashion'. 
    !! \return returns axis-parallel' node pointer(s) to be split
    !! \param selected_node_ptrs selected axis-parallel' node pointer(s)
    !! \param depth depth of nodes to be extracted for level-wise fashion.
    subroutine extract_split_node_ptrs_axis(this, selected_node_ptrs, depth)
        implicit none
        class(base_tree)                                :: this
        type(node_axis_ptr), allocatable, intent(inout) :: selected_node_ptrs(:)
        integer(kind=8), optional                       :: depth
        select case(this%hparam%fashion_int)
            case(1_8) ! best-first
                call extract_unsplit_node_ptrs_axis(this%root_node_axis_ptr, selected_node_ptrs)
            case(2_8) ! depth-first
                call extract_most_left_unsplit_node_ptr_axis(this%root_node_axis_ptr, selected_node_ptrs)
            case(3_8) ! level-wise
                call extract_specific_depth_node_ptrs_axis(this%root_node_axis_ptr, depth-1, selected_node_ptrs)
            case(4_8) ! impurity-first
                call extract_largetst_impurity_node_ptr_axis(this%root_node_axis_ptr, selected_node_ptrs)
            case(5_8) ! sample-first
                call extract_largetst_sample_node_ptr_axis(this%root_node_axis_ptr, selected_node_ptrs)
        end select
    end subroutine extract_split_node_ptrs_axis


    !> Which nodes are extracted is determined by hyperparameter 'fashion'. 
    !! \return returns adopted 'axis-parallel' node pointer(s)
    !! \param node_ptrs 'axis-parallel' node pointer(s) to be adopted
    !! \param data_holder_ptr pointer of 'data_holder'
    !! \param hparam_ptr pointer of decision tree hyperparameter
    !! \param is_classification classification tree or not
    subroutine adopt_node_ptrs_axis(this, node_ptrs, data_holder_ptr, hparam_ptr, is_classification, lr_layer, is_hist)
        implicit none
        class(base_tree) :: this
        type(node_axis_ptr), allocatable, intent(inout) :: node_ptrs(:)
        type(data_holder), pointer                      :: data_holder_ptr
        type(hparam_decisiontree), pointer              :: hparam_ptr
        logical(kind=4)                                 :: is_classification
        real(kind=8)                                    :: lr_layer
        logical(kind=4), optional                       :: is_hist

        logical(kind=4)          :: is_hist_optional
        type(node_axis), pointer :: selected_node_ptr
        integer(kind=8)          :: n, n_nodes

        is_hist_optional = f_
        if (present(is_hist)) is_hist_optional = is_hist

        select case(this%hparam%fashion_int)
            case(1_8) ! best
                ! print*, '============================================================='
                ! call count_all_nodes_axis(this%root_node_axis_ptr, n_nodes, is_root=.true.)
                ! print*, "No. Nodes: ", n_nodes
                if ( associated(selected_node_ptr) ) nullify(selected_node_ptr)
                call extract_best_split_node_axis(this%root_node_axis_ptr, selected_node_ptr)
                call adopting_twins_axis(selected_node_ptr, data_holder_ptr, hparam_ptr, is_classification, &
                    this%lr_layer, is_hist=is_hist_optional)
            case(2_8:5_8) ! others
                do n=1, size(node_ptrs), 1
                    call adopting_twins_axis(node_ptrs(n)%node_ptr, data_holder_ptr, hparam_ptr, & 
                        is_classification, this%lr_layer, is_hist=is_hist_optional)
                end do
        end select
    end subroutine adopt_node_ptrs_axis


    !> A function to predict responses.
    !> If classification tree, returns class probabiities.
    !> If regression tree, returns responses.
    !! \param x feature
    function predict_response(this, x)
        implicit none
        class(base_tree)                  :: this
        real(kind=8), target, intent(in)  :: x(:,:)
        real(kind=8), allocatable         :: predict_response(:,:)

        real(kind=8), pointer        :: x_ptr(:,:)
        integer(kind=8), allocatable :: indices(:)
        integer(kind=8)              :: x_shape(2), n_samples, n_columns, i

        x_shape = shape(x)
        n_samples  = x_shape(1)
        n_columns  = x_shape(2)

        if ( n_columns .ne. this%n_columns_ ) goto 990

        allocate(predict_response(n_samples, this%n_outputs_))
        allocate(indices(n_samples))
        do i=1, n_samples, 1
            indices(i) = i
        end do

        x_ptr => x
        predict_response = 0d0
        if (this%is_layer_wise_sum) then
            do i=1, n_samples, 1
                predict_response(i,:) = this%mean_y(:)
            end do
        end if
        if (this%is_axis_parallel) then
            call predict_response_axis(this%results, x_ptr, indices, & 
                predict_response, n_samples, this%n_outputs_, is_root=t_, & 
                is_layer_wise_sum=this%is_layer_wise_sum, &
                lr_layer=this%lr_layer)
        else
            ! call predict_response_oblq(this%results, x_ptr, indices, & 
            !     predict_response, n_samples, this%n_outputs_, is_root=t_)
        end if
        return
        990 continue
        stop "Number of feature mismatch."
    end function predict_response


    !> A function to predict responses for tree with 'axis-parallel' node.
    !! \param x_ptr pointer of feature 'x'
    !! \param indices sample indices in current path.
    !! \param responses sample responses
    !! \param n_samples n_samplesber of samples in current path
    !! \param n_outputs n_samplesber of outputs
    !! \param is_root is root node or not
    recursive subroutine predict_response_axis(result, x_ptr, indices, responses, n_samples, n_outputs, &
        is_root, is_layer_wise_sum, lr_layer)
        implicit none
        type(train_results)   :: result
        real(kind=8), pointer :: x_ptr(:,:)
        integer(kind=8)       :: indices(n_samples)
        real(kind=8)          :: responses(:,:)
        integer(kind=8)       :: n_samples, n_outputs
        logical(kind=4)       :: is_root
        logical(kind=4)       :: is_layer_wise_sum
        real(kind=8)          :: lr_layer

        integer(kind=8), save        :: node_id
        integer(kind=8)              :: idx, i, fid
        integer(kind=8)              :: count_l, count_r, factor
        real(kind=8)                 :: threshold
        real(kind=8), allocatable    :: res(:), tmp_f(:)
        logical(kind=4), allocatable :: lt_thresholds(:)
        integer(kind=8), allocatable :: indices_l(:), indices_r(:)

        if (is_root) node_id = 0_8
        node_id = node_id + 1_8
        
        if (n_samples .eq. 0 .and. result%is_terminals_(node_id)) return
        if (n_samples .eq. 0) goto 999

        if (is_layer_wise_sum) then
            allocate(res(result%n_outputs_))
            res = lr_layer * result%responses_(node_id,:)
            do i=1, n_samples
                idx = indices(i)
                responses(idx,:) = responses(idx,:) + res
            end do
            if (result%is_terminals_(node_id)) return
        else
            if ( result%is_terminals_(node_id) ) then
                allocate(res(result%n_outputs_))
                res = result%responses_(node_id,:)
                do i=1, n_samples
                    idx = indices(i)
                    responses(idx,:) = res
                end do
                return
            end if
        end if

        allocate(tmp_f(n_samples))
        fid = result%split_features_(node_id)
        do i=1, n_samples, 1
            idx = indices(i)
            tmp_f(i) = x_ptr(idx, fid)
        end do
        threshold = result%split_thresholds_(node_id)

        allocate(lt_thresholds(n_samples))
        lt_thresholds = tmp_f .le. threshold
        count_l = count(lt_thresholds)
        count_r = n_samples - count_l
        allocate(indices_l(count_l))
        allocate(indices_r(count_r))
        indices_l = -1_8
        indices_r = -1_8
        count_l = 1_8
        count_r = 1_8
        do i=1, n_samples, 1
            idx = indices(i)
            if (lt_thresholds(i)) then
                indices_l(count_l) = idx
                count_l = count_l + 1_8
            else
                indices_r(count_r) = idx
                count_r = count_r + 1_8
            end if
        end do


        999 continue
        if ( .not. allocated(indices_l) .and. .not. allocated(indices_r)) then
            count_l = 1_8
            count_r = 1_8
            allocate(indices_l(0_8))
            allocate(indices_r(0_8))
        end if

        call predict_response_axis(result, x_ptr, indices_l, responses, count_l-1, n_outputs, & 
            is_root=f_, is_layer_wise_sum=is_layer_wise_sum, lr_layer=lr_layer)
        call predict_response_axis(result, x_ptr, indices_r, responses, count_r-1, n_outputs, &
            is_root=f_, is_layer_wise_sum=is_layer_wise_sum, lr_layer=lr_layer)
        ! deallocate(indices_l, indices_r, tmp_f, lt_thresholds)
    end subroutine predict_response_axis


    !> Postprocess for Tree.
    !> Extract training results, and nullify node pointers
    subroutine postprocess(this, is_classification)
        implicit none
        class(base_tree) :: this
        logical(kind=4) :: is_classification
        integer(kind=8) :: n_nodes, node_id
        type(node_axis), pointer :: node_axis_ptr
        type(node_oblq), pointer :: node_oblq_ptr
        type(train_results) :: results

        results%n_columns_ = this%n_columns_
        results%n_outputs_  = this%n_outputs_

        if ( associated(this%root_node_axis_ptr) ) then
            node_axis_ptr => this%root_node_axis_ptr
            call count_all_nodes(node_axis_ptr, n_nodes, is_root=t_)
            allocate(this%split_features_(n_nodes))
        else
            ! node_oblq_ptr => this%root_node_oblq_ptr
            ! call count_all_nodes(node_oblq_ptr, n_nodes, is_root=t_)
            ! allocate(this%coefs_(n_nodes, this%n_columns_))
        end if

        if ( allocated(this%split_thresholds_) ) deallocate(this%split_thresholds_)
        if ( allocated(this%is_terminals_) ) deallocate(this%is_terminals_)
        if ( allocated(this%responses_) ) deallocate(this%responses_)

        allocate(this%split_thresholds_(n_nodes))
        allocate(this%is_terminals_(n_nodes))
        allocate(this%responses_(n_nodes, this%n_outputs_))
        call results%alloc(n_nodes, this%n_columns_, this%n_outputs_, is_classification)

        if ( associated(this%root_node_axis_ptr) ) then
            call extract_train_results_axis(node_axis_ptr, results, node_id, & 
                is_classification=is_classification, is_root=t_)
        else
            ! call extract_train_results_oblq(node_oblq_ptr, results, node_id, & 
            !     is_classification=is_classification, is_root=t_)
        end if

        if ( associated(this%root_node_axis_ptr) ) then
            this%split_features_ = results%split_features_
        else
            ! this%coefs_ = results%coefs_
        end if

        this%split_thresholds_ = results%split_thresholds_
        this%is_terminals_ = results%is_terminals_
        this%responses_ = results%responses_

        this%results = results
        if ( associated(this%root_node_axis_ptr) ) nullify(this%root_node_axis_ptr)
        if ( associated(this%root_node_oblq_ptr) ) nullify(this%root_node_oblq_ptr)
    end subroutine postprocess


end module mod_base_tree
