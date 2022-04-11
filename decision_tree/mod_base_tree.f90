module mod_base_tree
    use mod_const
    use mod_common
    use mod_math
    use mod_node
    use mod_hyperparameter
    use mod_random
    use mod_sort
    use mod_woodworking_tools
    use mod_stats
    implicit none
    
    !> A type of base tree for various decision tree algorithms
    type base_tree
        logical(kind=4)                  :: is_classification = f_
        logical(kind=4)                  :: is_threshold_tree = f_

        character(len=256)               :: algo_name              !< algorithm name
        logical(kind=4)                  :: is_trained = f_        !< is trained or not. If not, cannot predict and dump.
        logical(kind=4)                  :: is_axis_parallel = t_  !< is axis paralle split or not.
        logical(kind=4)                  :: is_hist=f_             !< use binned array or not.
        logical(kind=4)                  :: is_layer_wise_sum=f_   !< performs response summation for each layer, instead of returning only the response of the terminal node. **'lawu_regressor' only**
        type(hparam_decisiontree)        :: hparam                 !< hyperparameter of decision tree.
        ! Axis-Parallel
        type(node_axis), pointer         :: root_node_axis_ptr     !< pointer to root node with axis-parallel split.
        integer(kind=8), allocatable     :: split_features_(:)     !< indices of split features (#nodes). In internal node, 1-#columns. In terminal node, set to be -2.
        ! Oblique
        type(node_oblq), pointer         :: root_node_oblq_ptr     !< pointer to root node with oblique split.
        real(kind=8), allocatable        :: coefs_(:,:)            !< splitting coefficients (#nodes, #samples)
        real(kind=8), allocatable        :: intercepts_(:)         !< splitting intercepts (#nodes)
        ! Common
        real(kind=8), allocatable        :: split_thresholds_(:)   !< splitting thresholds (#nodes)
        logical(kind=4), allocatable     :: is_terminals_(:)       !< boolean array of terminal or not (#nodes)
        ! Classification and Regression
        real(kind=8), allocatable        :: responses_(:,:)        !< output response of all nodes. calculated by sample mean.
        integer(kind=8), allocatable     :: labels_(:)        !< output response of all nodes. calculated by sample mean.
        ! Anomaly Detection
        logical(kind=4)                  :: is_isolation_tree = f_ !< is isolation tree or not.

        ! 
        type(train_results)              :: results                !< training results
        real(kind=8), allocatable        :: mean_y(:)              !< mean of objective variable during training.
        real(kind=8)                     :: lr_layer               !< learinng rate per layer

        integer(kind=8) :: n_samples_                              !< number of samples in training
        integer(kind=8) :: n_columns_                              !< number of columns
        integer(kind=8) :: n_clusters_ = 0_8                       !< number of columns
        integer(kind=8) :: n_outputs_                              !< number of output dimension of objective variable
        integer(kind=8) :: n_labels_                               !< number of output dimension of objective variable
        integer(kind=8) :: n_leaf_nodes_                           !< number of leaf node
    contains
        procedure :: init => init_base_tree
        procedure :: induction_stop_check
        procedure :: init_root_node
        procedure :: postprocess
        procedure :: predict_response
        procedure :: predict_labels
        procedure :: extract_split_node_ptrs_axis
        procedure :: extract_split_node_ptrs_oblq
        procedure :: adopt_node_ptrs_axis
        procedure :: adopt_node_ptrs_oblq
        procedure :: adopt_node_ptrs_axis_for_isolation_tree

        procedure :: dump_base_tree
        procedure :: load_base_tree

        procedure, pass :: print_info_axis
        procedure, pass :: print_info_oblq
        generic :: print_info => print_info_axis, print_info_oblq
    end type base_tree

contains

    !> A function to calculate average depth of binary search tree.
    !! \return returns average depth of binary search tree
    !! \param n_samples number of samples
    function avg_depth(n_samples)
        implicit none
        real(kind=8) :: avg_depth
        integer(kind=8), intent(in) :: n_samples
        if (n_samples .eq. 1_8) then
            avg_depth = 0_8
            return 
        elseif ( n_samples .eq. 2_8 ) then
            avg_depth = 1_8
            return 
        end if
        avg_depth = 2d0*harmonic_number_approx(n_samples-1) - 2d0*(n_samples-1d0)/n_samples
    end function avg_depth


    !> A subroutine to dump fitted 'base_tree' object or its extended objects
    !> If not fitted, cannot dump training results.
    !! \param file_name output file name
    subroutine dump_base_tree(this, unit)
        implicit none
        class(base_tree) :: this
        integer(kind=8), intent(in)  :: unit
        integer(kind=8)  :: n_nodes, n_feature_fractions
        integer(kind=8)  :: dummy
        ! open(unit, file=file_name, form="unformatted", status="replace")
        if (.not. this % is_trained) then
            print*, trim(this % algo_name),  " is not trained. Cannot dump model."
            write(unit) f_ ! dump fail
            close(unit)
            stop
        end if
        write(unit) t_ ! dump fail
        write(unit) this%is_classification
        write(unit) this%is_threshold_tree
        write(unit) this%algo_name
        write(unit) this%is_axis_parallel
        write(unit) this%is_hist
        write(unit) this%is_layer_wise_sum
        ! 
        ! 
        dummy = allocated(this%hparam%feature_fractions)
        n_feature_fractions = size(this%hparam%feature_fractions) * dummy
        write(unit) n_feature_fractions
        write(unit) this%hparam%n_estimators
        write(unit) this%hparam%criterion_int
        write(unit) this%hparam%criterion_boost_int
        write(unit) this%hparam%verbose
        write(unit) this%hparam%max_epoch
        write(unit) this%hparam%max_patient
        write(unit) this%hparam%max_retry
        write(unit) this%hparam%max_alpha
        write(unit) this%hparam%min_alpha
        write(unit) this%hparam%n_repeats
        write(unit) this%hparam%n_rounds
        write(unit) this%hparam%max_bins
        write(unit) this%hparam%strategy_int
        write(unit) this%hparam%max_depth
        write(unit) this%hparam%min_samples_split
        write(unit) this%hparam%min_samples_leaf
        write(unit) this%hparam%max_features
        write(unit) this%hparam%max_feature_use
        write(unit) this%hparam%max_leaf_nodes
        write(unit) this%hparam%fashion_int
        write(unit) this%hparam%print_mod
        write(unit) this%hparam%num_threads_in_node
        write(unit) this%hparam%num_threads_in_forest
        write(unit) this%hparam%step_size_for_multi_grain
        write(unit) this%hparam%min_columns_in_grain
        write(unit) this%hparam%n_cascades
        write(unit) this%hparam%n_forest_per_layer
        if (n_feature_fractions .gt. 0_8) then
            write(unit) this%hparam%feature_fractions
        else
            write(unit) 0_8
        end if
        write(unit) this%hparam%learning_rate
        write(unit) this%hparam%learning_rate_layer
        write(unit) this%hparam%drop_rate
        write(unit) this%hparam%update_ratio
        write(unit) this%hparam%momentum
        write(unit) this%hparam%prunig_threshold
        write(unit) this%hparam%weight_decay
        write(unit) this%hparam%top_ratio
        write(unit) this%hparam%min_weight_fraction_leaf
        write(unit) this%hparam%min_impurity_decrease
        write(unit) this%hparam%other_ratio
        write(unit) this%hparam%lambda1
        write(unit) this%hparam%lambda2
        write(unit) this%hparam%row_sampling
        write(unit) this%hparam%skip_used_features
        write(unit) this%hparam%boot_strap
        write(unit) this%hparam%random_splitter
        write(unit) this%hparam%criterion
        write(unit) this%hparam%strategy
        write(unit) this%hparam%fashion
        
        n_nodes = size(this%results%split_features_)
        write(unit) n_nodes
        write(unit) this%results%n_columns_
        write(unit) this%results%n_outputs_
        if (this%is_axis_parallel) then
            write(unit) this%results%split_features_
        else
            write(unit) this%results%coefs_
        end if
        write(unit) this%results%split_thresholds_
        write(unit) this%results%is_terminals_
        write(unit) this%results%responses_

        if (this%is_layer_wise_sum) then
            write(unit) this%mean_y
        else
            write(unit) 0_8
        end if
        write(unit) this%lr_layer
        ! print*, "Dump Success!, ", trim(this % algo_name)
        ! close(unit)
    end subroutine dump_base_tree


    !> A subroutine to load 'base_tree' object or its extended objects
    !> If not fitted, cannot load.
    !! \param file_name loading file name
    subroutine load_base_tree(this, unit)
        implicit none
        class(base_tree) :: this
        integer(kind=8), intent(in)  :: unit
        logical(kind=4)  :: is_dump_successed, is_allocated_feature_fractions
        integer(kind=8)  :: n_nodes, n_feature_fractions, dummy, n_columns_, n_outputs_
        ! open(unit, file=file_name, form="unformatted")
        read(unit) is_dump_successed
        if (.not. is_dump_successed) then
            print*, trim(this % algo_name),  " failed dump of the model."
            stop
        end if

        this%is_trained = is_dump_successed
        read(unit) this%is_classification
        read(unit) this%is_threshold_tree
        read(unit) this%algo_name
        read(unit) this%is_axis_parallel
        read(unit) this%is_hist
        read(unit) this%is_layer_wise_sum
        ! 
        ! 
        read(unit) n_feature_fractions
        read(unit) this%hparam%n_estimators
        read(unit) this%hparam%criterion_int
        read(unit) this%hparam%criterion_boost_int
        read(unit) this%hparam%verbose
        read(unit) this%hparam%max_epoch
        read(unit) this%hparam%max_patient
        read(unit) this%hparam%max_retry
        read(unit) this%hparam%max_alpha
        read(unit) this%hparam%min_alpha
        read(unit) this%hparam%n_repeats
        read(unit) this%hparam%n_rounds
        read(unit) this%hparam%max_bins
        read(unit) this%hparam%strategy_int
        read(unit) this%hparam%max_depth
        read(unit) this%hparam%min_samples_split
        read(unit) this%hparam%min_samples_leaf
        read(unit) this%hparam%max_features
        read(unit) this%hparam%max_feature_use
        read(unit) this%hparam%max_leaf_nodes
        read(unit) this%hparam%fashion_int
        read(unit) this%hparam%print_mod
        read(unit) this%hparam%num_threads_in_node
        read(unit) this%hparam%num_threads_in_forest
        read(unit) this%hparam%step_size_for_multi_grain
        read(unit) this%hparam%min_columns_in_grain
        read(unit) this%hparam%n_cascades
        read(unit) this%hparam%n_forest_per_layer
        if (n_feature_fractions .gt. 0_8) then
            allocate(this%hparam%feature_fractions(n_feature_fractions))
            read(unit) this%hparam%feature_fractions
        else
            read(unit) dummy
        end if
        read(unit) this%hparam%learning_rate
        read(unit) this%hparam%learning_rate_layer
        read(unit) this%hparam%drop_rate
        read(unit) this%hparam%update_ratio
        read(unit) this%hparam%momentum
        read(unit) this%hparam%prunig_threshold
        read(unit) this%hparam%weight_decay
        read(unit) this%hparam%top_ratio
        read(unit) this%hparam%min_weight_fraction_leaf
        read(unit) this%hparam%min_impurity_decrease
        read(unit) this%hparam%other_ratio
        read(unit) this%hparam%lambda1
        read(unit) this%hparam%lambda2
        read(unit) this%hparam%row_sampling
        read(unit) this%hparam%skip_used_features
        read(unit) this%hparam%boot_strap
        read(unit) this%hparam%random_splitter
        read(unit) this%hparam%criterion
        read(unit) this%hparam%strategy
        read(unit) this%hparam%fashion

        read(unit) n_nodes
        read(unit) this%results % n_columns_
        read(unit) this%results % n_outputs_
        this%n_columns_ = this%results % n_columns_
        this%n_outputs_ = this%results % n_outputs_
        if (this%is_axis_parallel) then
            allocate(this%results%split_features_(n_nodes))
            read(unit) this%results % split_features_
        else
            allocate(this%results%coefs_(n_nodes, this%n_columns_))
            read(unit) this%results % coefs_
        end if
        allocate(this%results%split_thresholds_(n_nodes))
        allocate(this%results%is_terminals_(n_nodes))
        allocate(this%results%responses_(n_nodes, this%results % n_outputs_))
        read(unit) this%results % split_thresholds_
        read(unit) this%results % is_terminals_
        read(unit) this%results % responses_

        if (this%is_layer_wise_sum) then
            allocate(this%mean_y(this%n_outputs_))
            read(unit) this%mean_y
        else
            read(unit) dummy
        end if
        read(unit) this%lr_layer
        ! print*, "Load Success!, ", trim(this % algo_name)
        ! close(unit); return
    end subroutine load_base_tree


    !> A recursive subroutine to print node informations with axis-parallel split.
    !! \param root_node_ptr_axis pointer to root node with axis-parallel split.
    recursive subroutine print_info_axis(this, root_node_ptr_axis)
        implicit none
        class(base_tree) :: this
        type(node_axis), pointer :: root_node_ptr_axis
        type(node_axis), pointer :: node_l_ptr_axis, node_r_ptr_axis
        print*, "print"
        call root_node_ptr_axis%print_node_info_axis()
        if ( allocated(root_node_ptr_axis%node_l) ) then
            node_l_ptr_axis => root_node_ptr_axis%node_l
            node_r_ptr_axis => root_node_ptr_axis%node_r
            call this%print_info_axis(node_l_ptr_axis)
            call this%print_info_axis(node_r_ptr_axis)
        end if
    end subroutine print_info_axis


    !> A recursive subroutine to print node informations with oblique split.
    !! \param root_node_ptr_oblq pointer to root node with oblique split.
    recursive subroutine print_info_oblq(this, root_node_ptr_oblq)
        implicit none
        class(base_tree) :: this
        type(node_oblq), pointer :: root_node_ptr_oblq
        type(node_oblq), pointer :: node_l_ptr_oblq, node_r_ptr_oblq
        print*, "print"
        call root_node_ptr_oblq%print_node_info_oblq()
        if ( allocated(root_node_ptr_oblq%node_l) ) then
            node_l_ptr_oblq => root_node_ptr_oblq%node_l
            node_r_ptr_oblq => root_node_ptr_oblq%node_r
            call this%print_info_oblq(node_l_ptr_oblq)
            call this%print_info_oblq(node_r_ptr_oblq)
        end if
    end subroutine print_info_oblq


    !> A subtouine to check if a 'base_tree' object or its extended objects can grow. 
    !! \return returns tree can grow or not
    !! \param hparam_ptr pointer to hyperparameter of decision_tree.
    !! \param is_stop tree can grow or not
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
            call count_leaf_nodes(this%root_node_oblq_ptr, n_leaf_nodes, is_root=t_)
        end if
        this%n_leaf_nodes_ = n_leaf_nodes
        ! print*, "induction_stop_check, N_LEAF_NODES : ", n_leaf_nodes
        if (hparam_ptr%max_leaf_nodes .ne. -1_8 .and. hparam_ptr%max_leaf_nodes .le. n_leaf_nodes) then
            ! print*, "induction_stop_check, STOP: Max Leaf Node", n_leaf_nodes
            is_stop = t_
            return
        end if

        ! NO SPLITTABLE LEAF
        exist_splittable_leaf = f_
        if (associated(this%root_node_axis_ptr)) then
            call check_splittable_leaf(this%root_node_axis_ptr, exist_splittable_leaf)
        elseif (associated(this%root_node_oblq_ptr)) then
            call check_splittable_leaf(this%root_node_oblq_ptr, exist_splittable_leaf)
        end if
        if (.not. exist_splittable_leaf) then
            ! print*, "induction_stop_check, STOP: NO SPLITTABLE LEAF"
            is_stop = t_
            return
        end if
    end subroutine induction_stop_check


    !> A subroutine to initialize tree, nullify pointers dellocate arrays.
    !! \param data_holder_ptr pointer to 'data_holder'
    subroutine init_base_tree(this, data_holder_ptr)
        implicit none
        class(base_tree)           :: this
        type(data_holder), pointer :: data_holder_ptr

        ! Nullify Node Pointers
        if ( associated(this%root_node_axis_ptr) ) nullify(this%root_node_axis_ptr)
        if ( associated(this%root_node_oblq_ptr) ) nullify(this%root_node_oblq_ptr)

        this%is_trained = f_
        ! Axis-Parallel
        if ( allocated(this%split_features_) ) deallocate(this%split_features_)
        if ( allocated(this%split_thresholds_) ) deallocate(this%split_thresholds_)

        ! Oblique
        if ( allocated(this%coefs_) ) deallocate(this%coefs_)
        if ( allocated(this%intercepts_) ) deallocate(this%intercepts_)

        ! Common
        if ( allocated(this%is_terminals_) ) deallocate(this%is_terminals_)

        ! Train data info
        this%n_samples_ = data_holder_ptr%n_samples
        this%n_columns_ = data_holder_ptr%n_columns
        this%n_outputs_ = data_holder_ptr%n_outputs
    end subroutine init_base_tree


    !> A subroutine to initialize root node pointer (axis-parallel and oblique).
    !! \return returns tree with initialized root node
    !! \param data_holder_ptr pointer to 'data_holder'
    !! \param is_classification classification tree or not
    subroutine init_root_node(this, data_holder_ptr, is_classification)
        implicit none
        class(base_tree)           :: this
        type(data_holder), pointer :: data_holder_ptr
        logical(kind=4)            :: is_classification
        integer(kind=8)            :: i

        integer(kind=8), ALLOCATABLE :: uniq_labels(:), label_counter(:) ! for classification tree

        this%n_outputs_ = data_holder_ptr%n_outputs
        if (associated(this%root_node_axis_ptr)) then
            if ( .not. this%is_isolation_tree ) this%hparam%max_samples = this%n_samples_
            allocate(this%root_node_axis_ptr%indices(this%hparam%max_samples))
            allocate(this%root_node_axis_ptr%is_used(this%n_columns_))
            allocate(this%root_node_axis_ptr%is_useless(this%n_columns_))
            allocate(this%root_node_axis_ptr%is_useless_center(this%n_clusters_))
            allocate(this%root_node_axis_ptr%sum_p(this%n_outputs_))
            this%root_node_axis_ptr%is_used(:) = f_
            this%root_node_axis_ptr%is_useless(:) = f_
            this%root_node_axis_ptr%is_useless_center(:) = f_
            this%root_node_axis_ptr%n_clusters = this%n_clusters_
            this%root_node_axis_ptr%is_hist = this%is_hist
            
            if ( this%hparam%boot_strap ) then
                call rand_integer(1_8, this%n_samples_, this%root_node_axis_ptr%indices, this%hparam%max_samples)
                call quick_sort(this%root_node_axis_ptr%indices, this%hparam%max_samples)
            else
                do i=1, this%n_samples_, 1
                    this%root_node_axis_ptr%indices(i) = i
                end do
            end if
            
            if (is_classification) then
                call groupby_count(uniq_labels, label_counter, data_holder_ptr%y_ptr%y_i8_ptr(:,1), this%n_samples_)
                this%root_node_axis_ptr%n_labels = size(uniq_labels)
                this%root_node_axis_ptr%uniq_label = uniq_labels
                this%root_node_axis_ptr%label_counter = label_counter
                this%root_node_axis_ptr%label_ = uniq_labels(maxloc(label_counter, dim=1))
                this%root_node_axis_ptr%label_proba = label_counter / dble(this%n_samples_)
            elseif ( this%is_isolation_tree ) then
                this%root_node_axis_ptr%sum_p    = (/0d0/)
                this%root_node_axis_ptr%response = (/0d0/)
                this%root_node_axis_ptr%impurity = huge(0d0)
            else ! regression
                this%root_node_axis_ptr%sum_p = sum(data_holder_ptr%y_ptr%y_r8_ptr, dim=1)
                this%root_node_axis_ptr%response = mean(data_holder_ptr%y_ptr%y_r8_ptr, this%n_samples_, this%n_outputs_)
                this%root_node_axis_ptr%impurity = sum(variance(data_holder_ptr%y_ptr%y_r8_ptr, this%n_samples_, this%n_outputs_, &
                                                        this%root_node_axis_ptr%response)) / dble(this%n_outputs_)
            end if
            this%root_node_axis_ptr%depth = 0_8 
            this%root_node_axis_ptr%n_columns = this%n_columns_
            this%root_node_axis_ptr%n_samples = this%hparam%max_samples
        else
            allocate(this%root_node_oblq_ptr%indices(this%n_samples_))
            allocate(this%root_node_oblq_ptr%is_used(this%n_columns_))
            allocate(this%root_node_oblq_ptr%is_useless(this%n_columns_))
            allocate(this%root_node_oblq_ptr%sum_p(this%n_outputs_))
            this%root_node_oblq_ptr%is_used = f_
            this%root_node_oblq_ptr%is_useless = f_
            allocate(this%root_node_oblq_ptr%coef_(this%n_columns_))

            if ( this%hparam%boot_strap ) then
                call rand_integer(1_8, this%n_samples_, this%root_node_oblq_ptr%indices, this%n_samples_)
                call quick_sort(this%root_node_oblq_ptr%indices, this%n_samples_)
            else
                do i=1, this%n_samples_, 1
                    this%root_node_oblq_ptr%indices(i) = i
                end do
            end if

            if (is_classification) then
                stop "Classification Tree Not Implemented!"
            else
                this%root_node_oblq_ptr%sum_p = sum(data_holder_ptr%y_ptr%y_r8_ptr, dim=1)
                this%root_node_oblq_ptr%response = mean(data_holder_ptr%y_ptr%y_r8_ptr, this%n_samples_, this%n_outputs_)
                this%root_node_oblq_ptr%impurity = sum(variance(data_holder_ptr%y_ptr%y_r8_ptr, this%n_samples_, this%n_outputs_, &
                                                            this%root_node_oblq_ptr%response)) / dble(this%n_outputs_)
            end if
            this%root_node_oblq_ptr%depth = 0_8 
            this%root_node_oblq_ptr%n_columns = this%n_columns_
            this%root_node_oblq_ptr%n_samples = this%n_samples_
        end if
    end subroutine init_root_node


    !> A subroutine to extract 'axis-parallel' node pointer(s) to be split.
    !> Which nodes are extracted is determined by hyperparameter 'fashion'. 
    !! \return returns 'axis-parallel' node pointer(s) to be split
    !! \param selected_node_ptrs selected 'axis-parallel' node pointer(s)
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


    !> A subroutine to extract 'axis-parallel' node pointer(s) to be split.
    !> Which nodes are extracted is determined by hyperparameter 'fashion'. 
    !! \return returns 'axis-parallel' node pointer(s) to be split
    !! \param selected_node_ptrs selected 'axis-parallel' node pointer(s)
    !! \param depth depth of nodes to be extracted for level-wise fashion.
    subroutine extract_split_node_ptrs_oblq(this, selected_node_ptrs, depth)
        implicit none
        class(base_tree)                                :: this
        type(node_oblq_ptr), allocatable, intent(inout) :: selected_node_ptrs(:)
        integer(kind=8), optional                       :: depth
        select case(this%hparam%fashion_int)
            case(1_8) ! best-first
                call extract_unsplit_node_ptrs_oblq(this%root_node_oblq_ptr, selected_node_ptrs)
            case(2_8) ! depth-first
                call extract_most_left_unsplit_node_ptr_oblq(this%root_node_oblq_ptr, selected_node_ptrs)
            case(3_8) ! level-wise
                call extract_specific_depth_node_ptrs_oblq(this%root_node_oblq_ptr, depth-1, selected_node_ptrs)
            case(4_8) ! impurity-first
                call extract_largetst_impurity_node_ptr_oblq(this%root_node_oblq_ptr, selected_node_ptrs)
            case(5_8) ! sample-first
                call extract_largetst_sample_node_ptr_oblq(this%root_node_oblq_ptr, selected_node_ptrs)
        end select
    end subroutine extract_split_node_ptrs_oblq


    !> A subroutine to create child nodes for axis-parallel node. 
    !! \return returns adopted 'axis-parallel' node pointer(s)
    !! \param node_ptrs 'axis-parallel' node pointer(s) to be adopted
    !! \param data_holder_ptr pointer of 'data_holder'
    !! \param hparam_ptr pointer of decision tree hyperparameter
    !! \param is_classification classification tree or not
    !! \param lr_layer learning rate per layer for 'lawu_regressor' only
    !! \param is_hist use binned array or not.
    subroutine adopt_node_ptrs_axis(this, node_ptrs, data_holder_ptr, hparam_ptr, &
        is_classification, is_threshold_tree, lr_layer, is_hist)
        implicit none
        class(base_tree) :: this
        type(node_axis_ptr), allocatable, intent(inout) :: node_ptrs(:)
        type(data_holder), pointer                      :: data_holder_ptr
        type(hparam_decisiontree), pointer              :: hparam_ptr
        logical(kind=4)                                 :: is_classification
        logical(kind=4)                                 :: is_threshold_tree
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
                if (.not. associated(selected_node_ptr)) return
                call adopting_twins_axis(selected_node_ptr, data_holder_ptr, hparam_ptr, is_classification, &
                        is_threshold_tree, &
                        this%lr_layer, is_hist=is_hist_optional)
            case(2_8:5_8) ! others
                do n=1, size(node_ptrs), 1
                    call adopting_twins_axis(node_ptrs(n)%node_ptr, data_holder_ptr, hparam_ptr, & 
                            is_classification, &
                            is_threshold_tree, this%lr_layer, is_hist=is_hist_optional)
                end do
        end select
    end subroutine adopt_node_ptrs_axis


    !> A subroutine to create child nodes for oblique node. 
    !! \return returns adopted 'oblique' node pointer(s)
    !! \param node_ptrs 'oblique' node pointer(s) to be adopted
    !! \param data_holder_ptr pointer of 'data_holder'
    !! \param hparam_ptr pointer of decision tree hyperparameter
    !! \param is_classification classification tree or not
    !! \param lr_layer learning rate per layer for 'lawu_regressor' only
    !! \param is_hist use binned array or not.
    subroutine adopt_node_ptrs_oblq(this, node_ptrs, data_holder_ptr, hparam_ptr, is_classification, lr_layer, is_hist)
        implicit none
        class(base_tree) :: this
        type(node_oblq_ptr), allocatable, intent(inout) :: node_ptrs(:)
        type(data_holder), pointer                      :: data_holder_ptr
        type(hparam_decisiontree), pointer              :: hparam_ptr
        logical(kind=4)                                 :: is_classification
        real(kind=8)                                    :: lr_layer
        logical(kind=4), optional                       :: is_hist

        logical(kind=4)          :: is_hist_optional
        type(node_oblq), pointer :: selected_node_ptr
        integer(kind=8)          :: n, n_nodes

        is_hist_optional = f_
        if (present(is_hist)) is_hist_optional = is_hist

        select case(this%hparam%fashion_int)
            case(1_8) ! best
                ! print*, '============================================================='
                ! call count_all_nodes_oblq(this%root_node_oblq_ptr, n_nodes, is_root=.true.)
                ! print*, "No. Nodes: ", n_nodes
                if ( associated(selected_node_ptr) ) nullify(selected_node_ptr)
                call extract_best_split_node_oblq(this%root_node_oblq_ptr, selected_node_ptr)
                if (.not. associated(selected_node_ptr)) return
                call adopting_twins_oblq(selected_node_ptr, data_holder_ptr, hparam_ptr, is_classification, &
                    this%lr_layer, is_hist=is_hist_optional)
            case(2_8:5_8) ! others
                do n=1, size(node_ptrs), 1
                    call adopting_twins_oblq(node_ptrs(n)%node_ptr, data_holder_ptr, hparam_ptr, & 
                        is_classification, this%lr_layer, is_hist=is_hist_optional)
                end do
        end select
    end subroutine adopt_node_ptrs_oblq


    !> A subroutine to create child nodes when 'isolation_tree'. 
    !! \param node_ptrs 'axis-parallel' node pointer(s) to be adopted
    !! \param data_holder_ptr pointer of 'data_holder'
    !! \param hparam_ptr pointer of decision tree hyperparameter
    subroutine adopt_node_ptrs_axis_for_isolation_tree(this, node_ptrs, data_holder_ptr, hparam_ptr)
        implicit none
        class(base_tree) :: this
        type(node_axis_ptr), allocatable, intent(inout) :: node_ptrs(:)
        type(data_holder), pointer                      :: data_holder_ptr
        type(hparam_decisiontree), pointer              :: hparam_ptr

        type(node_axis), pointer :: selected_node_ptr
        integer(kind=8)          :: n, n_nodes

        do n=1, size(node_ptrs), 1
            call adopting_twins_axis_for_isolation_tree(node_ptrs(n)%node_ptr, data_holder_ptr, hparam_ptr)
        end do
    end subroutine adopt_node_ptrs_axis_for_isolation_tree


    !> A function to predict responses.
    !> If classification tree, returns class probabiities. **NOT IMPLEMENTD**
    !> If regression tree, returns responses.
    !! \param x explanatory array
    !! \param return_depth **optional** return sample depth. If terminal node contains multiple instances, add 'avg_depth(n_samples in node)'. Use isolation_tree and isolation_forest only.
    function predict_response(this, x, return_depth)
        implicit none
        class(base_tree)                  :: this
        real(kind=8), target, intent(in)  :: x(:,:)
        real(kind=8), allocatable         :: predict_response(:,:)
        logical(kind=4), optional         :: return_depth

        real(kind=8), pointer        :: x_ptr(:,:)
        integer(kind=8), allocatable :: indices(:)
        integer(kind=8)              :: x_shape(2), n_samples, n_columns, i
        logical(kind=4)              :: return_depth_opt


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
                lr_layer=this%lr_layer, is_isolation_tree=this%is_isolation_tree)
        else
            call predict_response_oblq(this%results, x_ptr, indices, & 
                predict_response, n_samples, this%n_outputs_, is_root=t_, & 
                is_layer_wise_sum=this%is_layer_wise_sum, &
                lr_layer=this%lr_layer)
        end if

        if (this%is_isolation_tree) then
            return_depth_opt = f_
            if (present(return_depth)) return_depth_opt = return_depth
            if (.not. return_depth_opt) then
                predict_response(:,1) = 2d0**(-predict_response(:,1)/avg_depth(n_samples))
            end if
        end if

        return
        990 continue
        stop "Number of feature mismatch."
    end function predict_response


    !> A function to predict responses.
    !> If classification tree, returns class probabiities. **NOT IMPLEMENTD**
    !> If regression tree, returns responses.
    !! \param x explanatory array
    !! \param return_depth **optional** return sample depth. If terminal node contains multiple instances, add 'avg_depth(n_samples in node)'. Use isolation_tree and isolation_forest only.
    function predict_labels(this, x, return_depth)
        implicit none
        class(base_tree)                  :: this
        real(kind=8), target, intent(in)  :: x(:,:)
        integer(kind=8), allocatable         :: predict_labels(:,:)
        logical(kind=4), optional         :: return_depth

        real(kind=8), pointer        :: x_ptr(:,:)
        integer(kind=8), allocatable :: indices(:)
        integer(kind=8)              :: x_shape(2), n_samples, n_columns, i
        logical(kind=4)              :: return_depth_opt

        x_shape = shape(x)
        n_samples  = x_shape(1)
        n_columns  = x_shape(2)

        if ( n_columns .ne. this%n_columns_ ) goto 990

        allocate(predict_labels(n_samples, 1))
        allocate(indices(n_samples))
        do i=1, n_samples, 1
            indices(i) = i
        end do

        x_ptr => x
        predict_labels = 0d0
        if (this%is_layer_wise_sum) then
            do i=1, n_samples, 1
                predict_labels(i,:) = this%mean_y(:)
            end do
        end if
        if (this%is_axis_parallel) then
            call predict_labels_axis(this%results, x_ptr, indices, & 
                predict_labels, n_samples, this%n_labels_, is_root=t_)
        else
            stop "Predict Labels for Oblique Tree is Not Implemented."
        end if

        return
        990 continue
        stop "Number of feature mismatch."
    end function predict_labels


    !> A subroutine to predict responses for tree with 'axis-parallel' node.
    !! \param x_ptr pointer to explanatory variable
    !! \param indices sample indices in current path.
    !! \param responses sample responses
    !! \param n_samples n_samplesber of samples in current path
    !! \param n_outputs n_samplesber of outputs
    !! \param is_root is root node or not
    !! \param is_layer_wise_sum performs response summation for each layer, instead of returning only the response of the terminal node.
    !! \param lr_layer learing rate per layer
    !! \param is_isolation_tree is 'isolation_tree' or not
    recursive subroutine predict_labels_axis(result, x_ptr, indices, responses, n_samples, n_outputs, &
        is_root)
        implicit none
        type(train_results)   :: result
        real(kind=8), pointer :: x_ptr(:,:)
        integer(kind=8)       :: indices(n_samples)
        integer(kind=8)          :: responses(:,:)
        integer(kind=8)       :: n_samples, n_outputs
        logical(kind=4)       :: is_root

        integer(kind=8), save        :: node_id
        integer(kind=8)              :: idx, i, fid, lbl
        integer(kind=8)              :: count_l, count_r, factor
        real(kind=8)                 :: threshold
        real(kind=8), allocatable    :: tmp_f(:)
        logical(kind=4), allocatable :: lt_thresholds(:)
        integer(kind=8), allocatable :: indices_l(:), indices_r(:)

        if (is_root) node_id = 0_8
        node_id = node_id + 1_8
        
        ! print*, size(result%is_terminals_), node_id
        if (n_samples .eq. 0 .and. result%is_terminals_(node_id)) return
        if (n_samples .eq. 0) goto 999
        
        if ( result%is_terminals_(node_id) ) then
            lbl = result%labels_(node_id)
            do i=1, n_samples
                idx = indices(i)
                responses(idx,1) = lbl
            end do
            return
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

        call predict_labels_axis(result, x_ptr, indices_l, responses, count_l-1, n_outputs, is_root=f_)
        call predict_labels_axis(result, x_ptr, indices_r, responses, count_r-1, n_outputs, is_root=f_)
        deallocate(indices_l, indices_r, tmp_f, lt_thresholds)
    end subroutine predict_labels_axis




    !> A subroutine to predict responses for tree with 'axis-parallel' node.
    !! \param x_ptr pointer to explanatory variable
    !! \param indices sample indices in current path.
    !! \param responses sample responses
    !! \param n_samples n_samplesber of samples in current path
    !! \param n_outputs n_samplesber of outputs
    !! \param is_root is root node or not
    !! \param is_layer_wise_sum performs response summation for each layer, instead of returning only the response of the terminal node.
    !! \param lr_layer learing rate per layer
    !! \param is_isolation_tree is 'isolation_tree' or not
    recursive subroutine predict_response_axis(result, x_ptr, indices, responses, n_samples, n_outputs, &
        is_root, is_layer_wise_sum, lr_layer, is_isolation_tree)
        implicit none
        type(train_results)   :: result
        real(kind=8), pointer :: x_ptr(:,:)
        integer(kind=8)       :: indices(n_samples)
        real(kind=8)          :: responses(:,:)
        integer(kind=8)       :: n_samples, n_outputs
        logical(kind=4)       :: is_root
        logical(kind=4)       :: is_layer_wise_sum
        real(kind=8)          :: lr_layer
        logical(kind=4)       :: is_isolation_tree

        integer(kind=8), save        :: node_id
        integer(kind=8)              :: idx, i, fid
        integer(kind=8)              :: count_l, count_r, factor
        real(kind=8)                 :: threshold
        real(kind=8), allocatable    :: res(:), tmp_f(:)
        logical(kind=4), allocatable :: lt_thresholds(:)
        integer(kind=8), allocatable :: indices_l(:), indices_r(:)

        if (is_root) node_id = 0_8
        node_id = node_id + 1_8
        
        ! print*, size(result%is_terminals_), node_id
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
                if (is_isolation_tree) res = res + avg_depth(n_samples)
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
            is_root=f_, is_layer_wise_sum=is_layer_wise_sum, lr_layer=lr_layer, & 
            is_isolation_tree=is_isolation_tree)
        call predict_response_axis(result, x_ptr, indices_r, responses, count_r-1, n_outputs, &
            is_root=f_, is_layer_wise_sum=is_layer_wise_sum, lr_layer=lr_layer, & 
            is_isolation_tree=is_isolation_tree)
        ! deallocate(indices_l, indices_r, tmp_f, lt_thresholds)
    end subroutine predict_response_axis


    !> A subroutine to predict responses for tree with 'obluque' node.
    !! \param x_ptr pointer to explanatory variable
    !! \param indices sample indices in current path.
    !! \param responses sample responses
    !! \param n_samples n_samplesber of samples in current path
    !! \param n_outputs n_samplesber of outputs
    !! \param is_root is root node or not
    !! \param is_layer_wise_sum performs response summation for each layer, instead of returning only the response of the terminal node.
    !! \param lr_layer learing rate per layer
    !! \param is_isolation_tree is 'isolation_tree' or not
    recursive subroutine predict_response_oblq(result, x_ptr, indices, responses, n_samples, n_outputs, &
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
        real(kind=8), allocatable    :: res(:), tmp_x(:,:), tmp_r(:)
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

        allocate(tmp_x(n_samples, result%n_columns_))
        allocate(tmp_r(n_samples))
        do i=1, n_samples, 1
            idx = indices(i)
            tmp_x(i,:) = x_ptr(idx, :)
        end do
        threshold = result%split_thresholds_(node_id)
        call multi_mat_vec(tmp_x, result%coefs_(node_id,:), tmp_r, n_samples, result%n_columns_)

        allocate(lt_thresholds(n_samples))
        lt_thresholds = tmp_r .le. threshold
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

        call predict_response_oblq(result, x_ptr, indices_l, responses, count_l-1, n_outputs, & 
            is_root=f_, is_layer_wise_sum=is_layer_wise_sum, lr_layer=lr_layer)
        call predict_response_oblq(result, x_ptr, indices_r, responses, count_r-1, n_outputs, &
            is_root=f_, is_layer_wise_sum=is_layer_wise_sum, lr_layer=lr_layer)
        ! deallocate(indices_l, indices_r, tmp_f, lt_thresholds)
    end subroutine predict_response_oblq


    !> A subroutine to perform postprocess of decision tree algorithms.
    !> Extract training results, nullify node pointers, and deallocate arrays
    !! \param is_classification is classification tree or not
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
            ! print*, "Count #Nodes"
            node_axis_ptr => this%root_node_axis_ptr
            call count_all_nodes(node_axis_ptr, n_nodes, is_root=t_)
            allocate(this%split_features_(n_nodes))
        else
            ! print*, "Count #Nodes"
            node_oblq_ptr => this%root_node_oblq_ptr
            call count_all_nodes(node_oblq_ptr, n_nodes, is_root=t_)
            allocate(this%coefs_(n_nodes, this%n_columns_))
        end if

        call results%alloc(n_nodes, this%n_columns_, this%n_outputs_, is_classification)
        if ( associated(this%root_node_axis_ptr) ) then
            ! print*, "Extract Train Results"
            call extract_train_results_axis(node_axis_ptr, results, node_id, & 
                is_classification=is_classification, is_root=t_)
        else
            ! print*, "Extract Train Results"
            call extract_train_results_oblq(node_oblq_ptr, results, node_id, & 
                is_classification=is_classification, is_root=t_)
        end if

        if ( associated(this%root_node_axis_ptr) ) then
            ! print*, "Split Feature"
            this%split_features_ = results%split_features_
        else
            ! print*, "Split Feature"
            this%coefs_ = results%coefs_
        end if

        ! print*, "get", results%labels_
        this%split_thresholds_ = results%split_thresholds_
        this%is_terminals_ = results%is_terminals_
        if (is_classification) then
            this%labels_ = results%labels_
        else
            this%responses_ = results%responses_
        end if

        ! print*, "store"
        this%results = results
        if ( associated(this%root_node_axis_ptr) ) nullify(this%root_node_axis_ptr)
        if ( associated(this%root_node_oblq_ptr) ) nullify(this%root_node_oblq_ptr)
    end subroutine postprocess


end module mod_base_tree
