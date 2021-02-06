module mod_woodworking_tools
    use mod_node
    use mod_timer
    implicit none
    
    !> A type to store training results.
    type train_results
        integer(kind=8)           :: n_columns_
        integer(kind=8)           :: n_outputs_
        integer(kind=8), allocatable :: split_features_(:)
        real(kind=8), allocatable :: coefs_(:,:)
        real(kind=8), allocatable :: split_thresholds_(:)
        real(kind=8), allocatable :: intercepts_(:)
        logical(kind=4), allocatable :: is_terminals_(:)
        real(kind=8), allocatable :: responses_(:,:)
    contains
        procedure :: alloc
    end type train_results

    interface extract_train_resutls
        module procedure :: extract_train_results_axis
        ! module procedure :: extract_train_results_oblq
    end interface extract_train_resutls

    interface extract_best_split_node
        module procedure :: extract_best_split_node_axis
        ! module procedure :: extract_best_split_node_oblq
    end interface extract_best_split_node

    ! interface extract_most_left_unsplit_node
    !     module procedure :: extract_most_left_unsplit_node_axis
    !     module procedure :: extract_most_left_unsplit_node_oblq
    ! end interface extract_most_left_unsplit_node

    interface count_leaf_nodes
        module procedure :: count_leaf_nodes_axis
        ! module procedure :: count_leaf_nodes_oblq
    end interface count_leaf_nodes

    interface check_splittable_leaf
        module procedure :: check_splittable_leaf_axis
        ! module procedure :: check_splittable_leaf_oblq
    end interface check_splittable_leaf

    interface count_all_nodes
        module procedure :: count_all_nodes_axis
        ! module procedure :: count_all_nodes_oblq
    end interface count_all_nodes

    interface adopting_twins
        module procedure :: adopting_twins_axis
        ! module procedure :: adopting_twins_oblq
    end interface adopting_twins

contains

    !> A subroutine to allocate training result arrays.
    !! \return returns allocated arrays
    !! \param n_nodes n_samplesber of nodes
    !! \param n_features n_samplesber of features
    !! \param n_outpus n_samplesber of output dimension
    !! \param is_classification classification task or not
    subroutine alloc(this, n_nodes, n_features, n_outputs, is_classification)
        implicit none
        class(train_results) :: this
        integer(kind=8), intent(in) :: n_nodes, n_features, n_outputs
        logical(kind=4), intent(in) :: is_classification
        if (allocated(this%split_features_)) deallocate(this%split_features_)
        if (allocated(this%coefs_)) deallocate(this%coefs_)
        if (allocated(this%split_thresholds_)) deallocate(this%split_thresholds_)
        if (allocated(this%intercepts_)) deallocate(this%intercepts_)
        if (allocated(this%is_terminals_)) deallocate(this%is_terminals_)
        if (allocated(this%responses_)) deallocate(this%responses_)

        allocate(this%split_features_(n_nodes))
        allocate(this%coefs_(n_nodes, n_features))
        allocate(this%split_thresholds_(n_nodes))
        allocate(this%intercepts_(n_nodes))
        allocate(this%is_terminals_(n_nodes))
        allocate(this%responses_(n_nodes,n_outputs))
    end subroutine alloc


    !> A subroutine to extract training results from 'node_axis' to 'train_results'.
    !! \return returns 
    !! \param root_node_ptr root node pointer
    !! \param results training results
    !! \param node_id current node id
    !! \param is_classification classification task or not
    !! \param is_root is root node or not
    recursive subroutine extract_train_results_axis(root_node_ptr, results, node_id, is_classification, is_root)
        implicit none
        type(node_axis), pointer, intent(in) :: root_node_ptr
        type(train_results) :: results
        integer(kind=8), intent(inout) :: node_id
        logical(kind=4), intent(in) :: is_classification, is_root

        if (is_root) node_id=0_8
        node_id = node_id + 1_8

        results%split_features_(node_id) = root_node_ptr%feature_id_
        results%split_thresholds_(node_id) = root_node_ptr%threshold_
        results%is_terminals_(node_id) = root_node_ptr%is_terminal
        results%responses_(node_id,:) = root_node_ptr%response
        if (.not. root_node_ptr%is_terminal) then
            call extract_train_results_axis(root_node_ptr%node_l, results, node_id, is_classification, is_root=f_)
            call extract_train_results_axis(root_node_ptr%node_r, results, node_id, is_classification, is_root=f_)
        end if
    end subroutine extract_train_results_axis


    !> A subtoutine to count leaf(terminal) node.
    !! \return returns number of leaf nodes
    !! \param root_node_ptr root node pointer
    !! \param n_leaf_nodes number of leaf nodes
    !! \param is_root is root node or not
    recursive subroutine count_leaf_nodes_axis(root_node_ptr, n_leaf_nodes, is_root)
        implicit none
        type(node_axis), pointer, intent(in) :: root_node_ptr
        integer(kind=8), intent(inout) :: n_leaf_nodes
        logical(kind=4), intent(in) :: is_root

        if (is_root) n_leaf_nodes=0_8

        if ( allocated(root_node_ptr%node_l) ) then
            call count_leaf_nodes_axis(root_node_ptr%node_l, n_leaf_nodes, is_root=f_)
            call count_leaf_nodes_axis(root_node_ptr%node_r, n_leaf_nodes, is_root=f_)
        else
            n_leaf_nodes = n_leaf_nodes + 1
            return
        end if
    end subroutine count_leaf_nodes_axis


    !> A subtoutine to check existence of splittable node
    !! \return returns exist splittable node or not
    !! \param root_node_ptr root node pointer
    !! \param exist_splittable_leaf exist splittable node or not
    recursive subroutine check_splittable_leaf_axis(root_node_ptr, exist_splittable_leaf)
        implicit none
        type(node_axis), pointer, intent(in) :: root_node_ptr
        logical(kind=4), intent(inout)       :: exist_splittable_leaf
        if ( allocated(root_node_ptr%node_l) ) then
            call check_splittable_leaf_axis(root_node_ptr%node_l, exist_splittable_leaf)
            call check_splittable_leaf_axis(root_node_ptr%node_r, exist_splittable_leaf)
        else
            if ( .not. root_node_ptr%is_terminal ) then
                exist_splittable_leaf = t_
                return
            end if
        end if
    end subroutine check_splittable_leaf_axis


    !> A subtoutine to count all(root, internal, leaf) nodes.
    !! \return returns n_samplesber of leaf nodes
    !! \param root_node_ptr root node pointer
    !! \param n_nodes n_samplesber of all nodes
    !! \param is_root is root node or not
    recursive subroutine count_all_nodes_axis(root_node_ptr, n_nodes, is_root)
        implicit none
        type(node_axis), pointer, intent(in) :: root_node_ptr
        integer(kind=8), intent(inout) :: n_nodes
        logical(kind=4), intent(in) :: is_root

        if (is_root) n_nodes=0_8
        n_nodes = n_nodes + 1
        if ( allocated(root_node_ptr%node_l) ) then
            call count_all_nodes_axis(root_node_ptr%node_l, n_nodes, is_root=f_)
            call count_all_nodes_axis(root_node_ptr%node_r, n_nodes, is_root=f_)
        end if
    end subroutine count_all_nodes_axis


    !> A subroutine to adopting child nodes to 'node_ptr'.
    !! \return returns adopted node pointer
    !! \param node_ptr node pointer
    !! \param data_holder_ptr data_holder pointer
    !! \param hparam_ptr decision tree hyperparameter pointer
    !! \param is_classification classification task or not
    subroutine adopting_twins_axis(node_ptr, data_holder_ptr, hparam_ptr, is_classification, is_hist)
        implicit none
        type(node_axis), pointer :: node_ptr
        type(data_holder), pointer :: data_holder_ptr
        type(hparam_decisiontree), pointer :: hparam_ptr
        logical(kind=4)           :: is_classification
        logical(kind=4), optional :: is_hist

        integer(kind=8) :: date_value1(8), date_value2(8)
        type(node_axis), target :: node_axis_l, node_axis_r
        integer(kind=8) :: i, cnt_l, cnt_r, cls, idx, fid
        real(kind=8), allocatable :: f(:), tmp_y(:,:)
        real(kind=8) :: avg, imp
        logical(kind=4) :: is_hist_optional
        integer(kind=8) :: n_samples_unroll, n_columns_unroll, j, jk, ik, k, row_idx, bin_idx, row_idx_next
        integer(kind=4) :: counter, factor
        integer(kind=8), save :: tot_time=0
        real(kind=8), ALLOCATABLE :: tmp_r(:)
        integer(kind=4), ALLOCATABLE :: bin_indices(:)
        if (node_ptr%depth .eq. 0_8) tot_time = 0

        if ( node_ptr%is_terminal ) return

        if (allocated(node_ptr%node_l)) return

        is_hist_optional = f_
        if (present(is_hist)) is_hist_optional = is_hist

        node_ptr%is_used(node_ptr%feature_id_) = t_

        allocate(node_axis_l%is_used(data_holder_ptr%n_columns))
        allocate(node_axis_l%is_useless(data_holder_ptr%n_columns))
        node_axis_l%is_used = node_ptr%is_used
        node_axis_l%is_useless = node_ptr%is_useless
        node_axis_l%is_hist = node_ptr%is_hist
        node_axis_l%depth = node_ptr%depth+1
        node_axis_l%n_columns = node_ptr%n_columns
        ! node_axis_l%n_classes = node_ptr%n_classes
        node_axis_l%impurity = node_ptr%impurity_l
        node_axis_l%n_samples = node_ptr%n_samples_l
        node_axis_l%sum_p = node_ptr%sum_l
        call node_axis_l%hparam_check(hparam_ptr)
        allocate(node_axis_l%indices(node_axis_l%n_samples))

        allocate(node_axis_r%is_used(data_holder_ptr%n_columns))
        allocate(node_axis_r%is_useless(data_holder_ptr%n_columns))
        node_axis_r%is_used = node_ptr%is_used
        node_axis_r%is_useless = node_ptr%is_useless
        node_axis_r%is_hist = node_ptr%is_hist
        node_axis_r%depth = node_ptr%depth+1
        node_axis_r%n_columns = node_ptr%n_columns
        ! node_axis_r%n_classes = node_ptr%n_classes
        node_axis_r%impurity = node_ptr%impurity_r
        node_axis_r%n_samples = node_ptr%n_samples_r
        node_axis_r%sum_p = node_ptr%sum_r
        call node_axis_r%hparam_check(hparam_ptr)
        allocate(node_axis_r%indices(node_axis_r%n_samples))

        cnt_l=1
        cnt_r=1
        fid = node_ptr%feature_id_
        if (is_hist_optional) then
            do i=1, node_ptr%n_samples
                idx = node_ptr%indices(i)
                if ( data_holder_ptr%x_hist(idx, fid) .le. node_ptr%threshold_ ) then
                    node_axis_l%indices(cnt_l) = idx
                    cnt_l = cnt_l + 1
                else
                    node_axis_r%indices(cnt_r) = idx
                    cnt_r = cnt_r + 1
                end if
            end do
        else
            do i=1, node_ptr%n_samples
                idx = node_ptr%indices(i)
                if ( data_holder_ptr%x_ptr%x_r8_ptr(idx, fid) .le. node_ptr%threshold_ ) then
                    node_axis_l%indices(cnt_l) = idx
                    cnt_l = cnt_l + 1
                else
                    node_axis_r%indices(cnt_r) = idx
                    cnt_r = cnt_r + 1
                end if
            end do
        end if

        node_axis_l%response = node_ptr%response_l
        imp = 0d0
        do i=1, node_axis_l%n_samples
            idx = node_axis_l%indices(i)
            imp = imp + sum((data_holder_ptr%y_ptr%y_r8_ptr(idx,:) - node_axis_l%response) ** 2d0)
        end do
        node_axis_l%impurity = imp / dble(node_axis_l%n_samples) / dble(node_ptr%n_outputs)

        node_axis_r%response = node_ptr%response_r
        imp = 0d0
        do i=1, node_axis_r%n_samples
            idx = node_axis_r%indices(i)
            imp = imp + sum((data_holder_ptr%y_ptr%y_r8_ptr(idx,:) - node_axis_r%response) ** 2d0)
        end do
        node_axis_r%impurity = imp / dble(node_axis_r%n_samples) / dble(node_ptr%n_outputs)

        if (allocated(data_holder_ptr%x_hist) .and. node_ptr%is_hist ) then
            ! --------------------------------------------------------------------------------------
            ! --------------------------------------------------------------------------------------
            ! Transposed
            allocate(bin_indices(node_ptr%n_columns))
            allocate(tmp_y(node_ptr%n_samples, node_ptr%n_outputs))
            allocate(tmp_r(node_ptr%n_outputs))

            allocate(node_axis_l%hist_self_count(node_ptr%n_columns, hparam_ptr%max_bins))
            allocate(node_axis_r%hist_self_count(node_ptr%n_columns, hparam_ptr%max_bins))
            node_axis_l%hist_self_count = 0
            node_axis_l%hist_self_count = 0

            allocate(node_axis_l%hist_self_sum_y(node_ptr%n_columns, hparam_ptr%max_bins, data_holder_ptr%n_outputs))
            allocate(node_axis_r%hist_self_sum_y(node_ptr%n_columns, hparam_ptr%max_bins, data_holder_ptr%n_outputs))
            node_axis_l%hist_self_sum_y = 0d0
            node_axis_l%hist_self_sum_y = 0d0

            call date_and_time(values=date_value1)
            do i=1, node_axis_l%n_samples, 1
                row_idx = node_axis_l%indices(i)
                tmp_r = data_holder_ptr%y_ptr%y_r8_ptr(row_idx,:)
                bin_indices = data_holder_ptr % x_hist_row(row_idx) % i_i4
                do j=1, node_axis_l%n_columns, 1
                    bin_idx = bin_indices(j)
                    node_axis_l%hist_self_sum_y(j,bin_idx,:) = node_axis_l%hist_self_sum_y(j,bin_idx,:) + tmp_r
                    node_axis_l%hist_self_count(j,bin_idx)   = node_axis_l%hist_self_count(j,bin_idx) + 1_4
                end do
            end do
            call date_and_time(values=date_value2)

            do i=1, hparam_ptr%max_bins, 1
                do j=1, node_ptr%n_columns, 1
                    node_axis_r%hist_self_sum_y(j,i,:) = node_ptr%hist_self_sum_y(j,i,:) - node_axis_l%hist_self_sum_y(j,i,:)
                    node_axis_r%hist_self_count(j,i)   = node_ptr%hist_self_count(j,i)   - node_axis_l%hist_self_count(j,i)
                end do
            end do

            ! --------------------------------------------------------------------------------------
            ! --------------------------------------------------------------------------------------
            ! Normal
            ! allocate(bin_indices(node_ptr%n_columns))
            ! allocate(tmp_y(node_ptr%n_samples, node_ptr%n_outputs))
            ! allocate(tmp_r(node_ptr%n_outputs))

            ! allocate(node_axis_l%hist_self_count(hparam_ptr%max_bins, node_ptr%n_columns))
            ! allocate(node_axis_r%hist_self_count(hparam_ptr%max_bins, node_ptr%n_columns))
            ! node_axis_l%hist_self_count = 0
            ! node_axis_l%hist_self_count = 0

            ! allocate(node_axis_l%hist_self_sum_y(hparam_ptr%max_bins, node_ptr%n_columns, data_holder_ptr%n_outputs))
            ! allocate(node_axis_r%hist_self_sum_y(hparam_ptr%max_bins, node_ptr%n_columns, data_holder_ptr%n_outputs))
            ! node_axis_l%hist_self_sum_y = 0d0
            ! node_axis_l%hist_self_sum_y = 0d0

            ! call date_and_time(values=date_value1)
            ! do i=1, node_axis_l%n_samples, 1
            !     row_idx = node_axis_l%indices(i)
            !     tmp_r = data_holder_ptr%y_ptr%y_r8_ptr(row_idx,:)
            !     bin_indices = data_holder_ptr % x_hist_row(row_idx) % i_i4
            !     do j=1, node_axis_l%n_columns, 1
            !         bin_idx = bin_indices(j)
            !         node_axis_l%hist_self_sum_y(bin_idx,j,:) = node_axis_l%hist_self_sum_y(bin_idx,j,:) + tmp_r
            !         node_axis_l%hist_self_count(bin_idx,j)   = node_axis_l%hist_self_count(bin_idx,j) + 1_4
            !     end do
            ! end do
            ! call date_and_time(values=date_value2)

            ! do j=1, node_ptr%n_columns, 1
            !     do i=1, hparam_ptr%max_bins, 1
            !         node_axis_r%hist_self_sum_y(i,j,:) = node_ptr%hist_self_sum_y(i,j,:) - node_axis_l%hist_self_sum_y(i,j,:)
            !         node_axis_r%hist_self_count(i,j)   = node_ptr%hist_self_count(i,j)   - node_axis_l%hist_self_count(i,j)
            !     end do
            ! end do

            tot_time = tot_time + time_diff(date_value1, date_value2)
        end if
        ! print*, tot_time, "[msec]"

        node_ptr%node_l = node_axis_l
        node_ptr%node_r = node_axis_r
    end subroutine adopting_twins_axis


    !> A subtoutine to extract maximum gain node (already split and not terminal).
    !! \return returns maximum gain split node pointer
    !! \param root_node_ptr root node pointer
    !! \param best_split_node_ptr maximum gain split node pointer
    recursive subroutine extract_best_split_node_axis(root_node_ptr, best_split_node_ptr)
        implicit none
        type(node_axis), pointer, intent(in)    :: root_node_ptr
        type(node_axis), pointer, intent(inout) :: best_split_node_ptr

        if ( allocated(root_node_ptr%node_l) ) then
            call extract_best_split_node_axis(root_node_ptr%node_l, best_split_node_ptr)
            call extract_best_split_node_axis(root_node_ptr%node_r, best_split_node_ptr)
        else
            if ( .not. root_node_ptr%is_terminal .and. .not. associated(best_split_node_ptr) ) then
                best_split_node_ptr => root_node_ptr
            end if

            if ( .not. root_node_ptr%is_terminal .and. root_node_ptr%gain_best .gt. best_split_node_ptr%gain_best & 
                .and. root_node_ptr%is_trained ) then
                best_split_node_ptr => root_node_ptr
            end if
        end if
    end subroutine extract_best_split_node_axis


    !> A subroutine to extract all unsplit node pointers, for best-first fashion.
    !! \return returns all unsplit node pointers
    !! \param root_node_ptr root node pointer
    !! \param unsplit_node_ptrs all unsplit node pointers
    recursive subroutine extract_unsplit_node_ptrs_axis(root_node_ptr, unsplit_node_ptrs)
        implicit none
        type(node_axis), pointer, intent(in)            :: root_node_ptr
        type(node_axis_ptr), allocatable, intent(inout) :: unsplit_node_ptrs(:)
        type(node_axis_ptr)                             :: node_ptr
        if ( allocated(root_node_ptr%node_l) ) then
            call extract_unsplit_node_ptrs_axis(root_node_ptr%node_l, unsplit_node_ptrs)
            call extract_unsplit_node_ptrs_axis(root_node_ptr%node_r, unsplit_node_ptrs)
        else
            if ( .not. root_node_ptr%is_terminal .and. .not. root_node_ptr%is_trained ) then
                node_ptr%node_ptr => root_node_ptr
                unsplit_node_ptrs = [unsplit_node_ptrs, node_ptr]
            end if
        end if
    end subroutine extract_unsplit_node_ptrs_axis


    !> A subroutine to extract most left node pointer, for depth-first fashion.
    !! \return returns most left node pointer
    !! \param root_node_ptr root node pointer
    !! \param most_left_node_ptr most left node pointer
    recursive subroutine extract_most_left_unsplit_node_ptr_axis(root_node_ptr, most_left_node_ptrs)
        implicit none
        type(node_axis), pointer, intent(in)    :: root_node_ptr
        type(node_axis_ptr), allocatable, intent(inout) :: most_left_node_ptrs(:)
        type(node_axis_ptr) :: node_ptr

        if ( size(most_left_node_ptrs) .ge. 1 ) return

        if ( allocated(root_node_ptr%node_l) ) then
            call extract_most_left_unsplit_node_ptr_axis(root_node_ptr%node_l, most_left_node_ptrs)
            call extract_most_left_unsplit_node_ptr_axis(root_node_ptr%node_r, most_left_node_ptrs)
        else
            if ( .not. root_node_ptr%is_terminal .and. .not. root_node_ptr%is_trained ) then
                node_ptr%node_ptr => root_node_ptr
                most_left_node_ptrs = [most_left_node_ptrs, node_ptr]
            end if
        end if
    end subroutine extract_most_left_unsplit_node_ptr_axis


    !> A subroutine to extract specific depht node pointers, for level-wise fashion.
    !! \return returns most left node pointer
    !! \param root_node_ptr root node pointer
    !! \param most_left_node_ptr most left node pointer
    recursive subroutine extract_specific_depth_node_ptrs_axis(root_node_ptr, specific_depth, specific_depth_node_ptrs)
        implicit none
        type(node_axis), pointer, intent(in)            :: root_node_ptr
        integer(kind=8), intent(in)                     :: specific_depth
        type(node_axis_ptr), allocatable, intent(inout) :: specific_depth_node_ptrs(:)
        type(node_axis_ptr)                             :: node_ptr

        if ( allocated(root_node_ptr%node_l) ) then
            call extract_specific_depth_node_ptrs_axis(root_node_ptr%node_l, specific_depth, specific_depth_node_ptrs)
            call extract_specific_depth_node_ptrs_axis(root_node_ptr%node_r, specific_depth, specific_depth_node_ptrs)
        else
            if ( .not. root_node_ptr%is_terminal & 
                .and. .not. root_node_ptr%is_trained & 
                .and. root_node_ptr%depth .eq. specific_depth) then
                node_ptr%node_ptr => root_node_ptr
                specific_depth_node_ptrs = [specific_depth_node_ptrs, node_ptr]
            end if
        end if
    end subroutine extract_specific_depth_node_ptrs_axis


    !> A subroutine to extract largest impurity node pointer, for impurity-first fashion.
    !! \return returns largest impurity node pointer
    !! \param root_node_ptr root node pointer
    !! \param most_left_node_ptr most left node pointer
    recursive subroutine extract_largetst_impurity_node_ptr_axis(root_node_ptr, largest_impurity_node_ptrs)
        implicit none
        type(node_axis), pointer, intent(in)    :: root_node_ptr
        type(node_axis_ptr), allocatable, intent(inout) :: largest_impurity_node_ptrs(:)
        type(node_axis_ptr)  :: node_ptr

        if ( allocated(root_node_ptr%node_l) ) then
            call extract_largetst_impurity_node_ptr_axis(root_node_ptr%node_l, largest_impurity_node_ptrs)
            call extract_largetst_impurity_node_ptr_axis(root_node_ptr%node_r, largest_impurity_node_ptrs)
        else
            
            if (  size(largest_impurity_node_ptrs) .eq. 0 ) then
                node_ptr%node_ptr => root_node_ptr
                largest_impurity_node_ptrs = [largest_impurity_node_ptrs, node_ptr]
            end if

            if (      .not. root_node_ptr%is_terminal & 
                .and. .not. root_node_ptr%is_trained & 
                .and. root_node_ptr%impurity .gt. largest_impurity_node_ptrs(1)%node_ptr%impurity) then
                node_ptr%node_ptr => root_node_ptr
                largest_impurity_node_ptrs(1) = node_ptr
            end if
        end if
    end subroutine extract_largetst_impurity_node_ptr_axis


    !> A subroutine to extract largest sample node pointer, for sample-first fashion.
    !! \return returns largest sample node pointer
    !! \param root_node_ptr root node pointer
    !! \param most_left_node_ptr most left node pointer
    recursive subroutine extract_largetst_sample_node_ptr_axis(root_node_ptr, largest_sample_node_ptrs)
        implicit none
        type(node_axis), pointer, intent(in)    :: root_node_ptr
        type(node_axis_ptr), allocatable, intent(inout) :: largest_sample_node_ptrs(:)
        type(node_axis_ptr)  :: node_ptr

        if ( allocated(root_node_ptr%node_l) ) then
            call extract_largetst_sample_node_ptr_axis(root_node_ptr%node_l, largest_sample_node_ptrs)
            call extract_largetst_sample_node_ptr_axis(root_node_ptr%node_r, largest_sample_node_ptrs)
        else
            
            if ( size(largest_sample_node_ptrs) .eq. 0 ) then
                node_ptr%node_ptr => root_node_ptr
                largest_sample_node_ptrs = [largest_sample_node_ptrs, node_ptr]
            end if

            if (      .not. root_node_ptr%is_terminal & 
                .and. .not. root_node_ptr%is_trained & 
                .and. root_node_ptr%n_samples .gt. largest_sample_node_ptrs(1)%node_ptr%n_samples) then
                node_ptr%node_ptr => root_node_ptr
                largest_sample_node_ptrs(1) = node_ptr
            end if
        end if
    end subroutine extract_largetst_sample_node_ptr_axis


    !> A subroutine to ,,,
    !! \param root_node_ptr root node pointer
    recursive subroutine termination_node_ptr_axis(root_node_ptr)
        implicit none
        type(node_axis), pointer, intent(in) :: root_node_ptr

        if ( allocated(root_node_ptr%node_l) ) then
            root_node_ptr%is_trained = t_
            root_node_ptr%is_terminal = f_
            call termination_node_ptr_axis(root_node_ptr%node_l)
            call termination_node_ptr_axis(root_node_ptr%node_r)
        else
            root_node_ptr%is_trained = t_
            root_node_ptr%is_terminal = t_
            root_node_ptr%feature_id_ = -2
            root_node_ptr%threshold_ = 0d0
            root_node_ptr%n_samples_l = 0_8
            root_node_ptr%n_samples_r = 0_8
        end if
    end subroutine termination_node_ptr_axis


end module mod_woodworking_tools
