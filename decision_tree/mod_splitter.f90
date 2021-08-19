module mod_splitter
    !$ use omp_lib
    use mod_const
    use mod_hyperparameter
    use mod_data_holder
    use mod_sort
    use mod_timer
    use mod_stats
    use mod_discretizer
    use mod_node
    implicit none
    
    !> A Node Spliiter Class
    !> Tree Class collects splittable nodes(s) and Node Splitter splits them.
    type node_splitter
    contains
        procedure :: split_decision_tree_regressor
        procedure :: split_decision_tree_regressor_indivisuals

        procedure :: split_extra_tree_regressor
        procedure :: split_extra_tree_regressor_indivisuals

        ! procedure :: split_extra_tree_regressor_speed_up
        ! procedure :: split_extra_tree_regressor_speed_up_indivisuals

        procedure :: split_extra_tree_regressor_faster
        procedure :: split_extra_tree_regressor_faster_indivisuals

        procedure :: split_clouds_regressor
        procedure :: split_clouds_regressor_indivisuals

        ! procedure :: split_random_rotation_tree_regressor
        ! procedure :: split_random_rotation_tree_regressor_indivisuals

        ! procedure :: split_random_rotation_extra_tree_regressor
        ! procedure :: split_random_rotation_extra_tree_regressor_indivisuals
        
        ! procedure :: split_oblivious_extra_tree_regressor
        ! procedure :: split_oblivious_extra_tree_regressor_all

        procedure :: split_weighted_oblique_decision_tree_classifier
        procedure :: split_weighted_oblique_decision_tree_classifier_indivisuals
    end type node_splitter


    ! type wodt_objective
    !     type(data_holder), pointer :: dholder_ptr
    ! contains
    !     procedure :: loss => loss_wodt_objective
    ! end type wodt_objective


contains


    !> A subroutine to split node by very tradisional way.
    !> L. Breiman, J. Friedman, R. Olshen, and C. Stone, “Classification and Regression Trees”, Wadsworth, Belmont, CA, 1984.
    subroutine split_decision_tree_regressor(this, node_ptrs, data_holder_ptr, hparam_ptr, & 
        n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
        implicit none
        class(node_splitter)               :: this
        type(node_axis_ptr), intent(inout) :: node_ptrs(:)
        type(data_holder), pointer         :: data_holder_ptr
        type(hparam_decisiontree), pointer :: hparam_ptr
        integer(kind=8), intent(in)        :: n_columns
        integer(kind=8), intent(inout)     :: feature_indices(n_columns)
        integer(kind=8), intent(in)        :: feature_indices_scanning_range(2)
        logical(kind=4), intent(in)        :: is_permute_per_node

        integer(kind=8) :: n

        if ( size(node_ptrs) .eq. 1 ) then
            call this%split_decision_tree_regressor_indivisuals(node_ptrs(1)%node_ptr, data_holder_ptr, hparam_ptr, &
                n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
        else
            do n=1, size(node_ptrs), 1
                call this%split_decision_tree_regressor_indivisuals(node_ptrs(n)%node_ptr, data_holder_ptr, hparam_ptr, &
                    n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
            end do
        end if
    end subroutine split_decision_tree_regressor


    !> A subroutine to split node by very tradisional way.
    !> L. Breiman, J. Friedman, R. Olshen, and C. Stone, “Classification and Regression Trees”, Wadsworth, Belmont, CA, 1984.
    subroutine split_decision_tree_regressor_indivisuals(this, node_ptr, data_holder_ptr, hparam_ptr, &
        n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
        implicit none
        class(node_splitter)               :: this
        type(node_axis), intent(inout)     :: node_ptr
        type(data_holder), pointer         :: data_holder_ptr
        type(hparam_decisiontree), pointer :: hparam_ptr
        integer(kind=8), intent(in)        :: n_columns
        integer(kind=8), intent(inout)     :: feature_indices(n_columns)
        integer(kind=8), intent(in)        :: feature_indices_scanning_range(2)
        logical(kind=4), intent(in)        :: is_permute_per_node

        real(kind=8) :: n_rows_inv, n_outs_inv
        real(kind=8), allocatable :: res_l(:), res_r(:), sum_l(:), sum_r(:), avg_l(:), avg_r(:)
        real(kind=8), allocatable :: tot_p(:), tot_l(:), tot_r(:)
        real(kind=8) :: gain, gain_best, best_threshold
        integer(kind=8) :: i_start, i_stop, fid, f, i, j, best_fid
        integer(kind=8) :: count_l, count_r, idx, count_eval
        integer(kind=8) :: n_samples_l, n_samples_r
        integer(kind=8) :: ini_f_idx, fin_f_idx
        real(kind=8), allocatable :: tmp_f(:), tmp_y(:,:), tmp_f_copy(:), tmp_y_copy(:,:)
        integer(kind=8), allocatable :: sorted_indices(:)

        allocate(tot_p(data_holder_ptr%n_outputs))
        allocate(tot_l(data_holder_ptr%n_outputs))
        allocate(tot_r(data_holder_ptr%n_outputs))
        allocate(res_l(data_holder_ptr%n_outputs))
        allocate(res_r(data_holder_ptr%n_outputs))
        allocate(sum_l(data_holder_ptr%n_outputs))
        allocate(sum_r(data_holder_ptr%n_outputs))
        allocate(avg_l(data_holder_ptr%n_outputs))
        allocate(avg_r(data_holder_ptr%n_outputs))
        node_ptr%n_outputs = data_holder_ptr%n_outputs

        gain_best = - huge(0d0)
        n_rows_inv = 1d0 / dble(node_ptr%n_samples)
        n_outs_inv = 1d0 / dble(node_ptr%n_outputs)
        tot_p = node_ptr%sum_p
        count_eval = 0_8

        i_start = hparam_ptr%min_samples_leaf
        i_stop  = node_ptr%n_samples-i_start

        allocate(tmp_y_copy(node_ptr%n_samples, node_ptr%n_outputs))
        allocate(tmp_y(node_ptr%n_samples, node_ptr%n_outputs))
        allocate(tmp_f(node_ptr%n_samples))
        allocate(tmp_f_copy(node_ptr%n_samples))
        allocate(sorted_indices(node_ptr%n_samples))

        do i=1, node_ptr%n_samples, 1
            idx = node_ptr%indices(i)
            tmp_y(i,:) = data_holder_ptr%y_ptr%y_r8_ptr(idx,:)
        end do
        if (is_permute_per_node) call permutation(feature_indices, n_columns)

        ini_f_idx = feature_indices_scanning_range(1)
        fin_f_idx = feature_indices_scanning_range(2)
        do f=ini_f_idx, fin_f_idx, 1
            fid = feature_indices(f)
            ! Useless or Used Feature Skip
            if ( node_ptr%is_useless(fid) ) cycle
            if ( node_ptr%is_used(fid) .and. hparam_ptr%skip_used_features ) cycle

            ! Collect Data
            do i=1, node_ptr%n_samples, 1
                idx = node_ptr%indices(i)
                tmp_f(i) = data_holder_ptr%x_ptr%x_r8_ptr(idx, fid)
                sorted_indices(i) = i
            end do

            ! Sort response by feature
            ! call quick_argsort(tmp_f, sorted_indices, node_ptr%n_samples)
            call pbucket_argsort(tmp_f, sorted_indices, node_ptr%n_samples)
            do i=1, node_ptr%n_samples
                idx = sorted_indices(i)
                tmp_y_copy(i,:) = tmp_y(idx,:)
            end do

            ! Useless Feature Case
            if (tmp_f(1) .eq. tmp_f(node_ptr%n_samples)) then
                node_ptr%is_useless(fid) = t_
                cycle
            end if

            ! Search All Possible Split Points
            tot_l   = 0d0
            count_l = 0
            do i=1, i_start, 1
                tot_l   = tot_l   + tmp_y_copy(i,:)
                count_l = count_l + 1
            end do

            do i=i_start+1, i_stop, 1
                tot_l   = tot_l   + tmp_y_copy(i,:)
                count_l = count_l + 1
                if ( tmp_f(i) .ne. tmp_f(i+1) ) then
                    tot_r   = tot_p - tot_l
                    count_r = node_ptr%n_samples - count_l
                    avg_l   = tot_l / dble(count_l)
                    avg_r   = tot_r / dble(count_r)
                    gain    = dble(count_l*count_r)*n_rows_inv * sum( (avg_l-avg_r)**2d0 ) * n_outs_inv
                    if (gain_best .lt. gain) then
                        gain_best = gain
                        best_fid = fid
                        best_threshold = (tmp_f(i) + tmp_f(i+1)) * .5d0
                        res_l = avg_l
                        res_r = avg_r
                        n_samples_l = count_l
                        n_samples_r = count_r
                        sum_l = tot_l
                        sum_r = tot_r
                        count_eval = count_eval+1
                    end if
                end if
            end do

            if ( hparam_ptr%max_features .ne. -1 ) then
                if ( hparam_ptr%max_features .le. f .and. count_eval .ge. 1 ) then
                    exit
                end if
            end if
        end do

        node_ptr%is_trained = t_
        node_ptr%eval_counter = count_eval
        node_ptr%gain_best   = gain_best
        node_ptr%gain_best_w = gain_best * dble(node_ptr%n_samples) / dble(data_holder_ptr%n_samples)

        if ( count_eval .eq. 0 ) then
            node_ptr%is_terminal = t_
            call node_ptr%hparam_check(hparam_ptr)
            return
        end if

        node_ptr%feature_id_ = best_fid
        node_ptr%threshold_ = best_threshold

        if (allocated(node_ptr%sum_l))      deallocate(node_ptr%sum_l)
        if (allocated(node_ptr%sum_r))      deallocate(node_ptr%sum_r)
        if (allocated(node_ptr%response_l)) deallocate(node_ptr%response_l)
        if (allocated(node_ptr%response_r)) deallocate(node_ptr%response_r)

        allocate(node_ptr%sum_l(node_ptr%n_outputs))
        allocate(node_ptr%sum_r(node_ptr%n_outputs))
        allocate(node_ptr%response_l(node_ptr%n_outputs))
        allocate(node_ptr%response_r(node_ptr%n_outputs))

        node_ptr%sum_l = sum_l
        node_ptr%sum_r = sum_r
        node_ptr%n_samples_l = n_samples_l
        node_ptr%n_samples_r = n_samples_r
        node_ptr%response_l = res_l
        node_ptr%response_r = res_r
        call node_ptr%hparam_check(hparam_ptr)
    end subroutine split_decision_tree_regressor_indivisuals


    ! !> A subroutine to split node by extremely randomized way.
    ! subroutine split_extra_tree_regressor(this, node_ptrs, data_holder_ptr, hparam_ptr, & 
    !     n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
    !     implicit none
    !     class(node_splitter)               :: this
    !     type(node_axis_ptr), intent(inout) :: node_ptrs(:)
    !     type(data_holder), pointer         :: data_holder_ptr
    !     type(hparam_decisiontree), pointer :: hparam_ptr
    !     integer(kind=8), intent(in)        :: n_columns
    !     integer(kind=8), intent(inout)     :: feature_indices(n_columns)
    !     integer(kind=8), intent(in)        :: feature_indices_scanning_range(2)
    !     logical(kind=4), intent(in)        :: is_permute_per_node
    !     integer(kind=8) :: n

    !     if ( size(node_ptrs) .eq. 1 ) then
    !         call this%split_extra_tree_regressor_indivisuals(node_ptrs(1)%node_ptr, data_holder_ptr, hparam_ptr, &
    !             n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
    !     else
    !         do n=1, size(node_ptrs), 1
    !             call this%split_extra_tree_regressor_indivisuals(node_ptrs(n)%node_ptr, data_holder_ptr, hparam_ptr, &
    !                 n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
    !         end do
    !     end if
    ! end subroutine split_extra_tree_regressor


    ! !> A subroutine to split node by extremely randomized way.
    ! subroutine split_extra_tree_regressor_indivisuals(this, node_ptr, data_holder_ptr, hparam_ptr, &
    !     n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
    !     implicit none
    !     class(node_splitter)               :: this
    !     type(node_axis), pointer           :: node_ptr
    !     type(data_holder), pointer         :: data_holder_ptr
    !     type(hparam_decisiontree), pointer :: hparam_ptr
    !     integer(kind=8), intent(in)        :: n_columns
    !     integer(kind=8), intent(inout)     :: feature_indices(n_columns)
    !     integer(kind=8), intent(in)        :: feature_indices_scanning_range(2)
    !     logical(kind=4), intent(in)        :: is_permute_per_node

    !     real(kind=8) :: n_rows_inv, n_outs_inv
    !     real(kind=8), allocatable :: res_l(:), res_r(:), sum_l(:), sum_r(:), avg_l(:), avg_r(:)
    !     real(kind=8), allocatable :: tot_p(:), tot_l(:), tot_r(:)
    !     real(kind=8) :: gain, gain_best, best_threshold
    !     integer(kind=8) :: i_start, i_stop, fid, f, i, j, best_fid, r
    !     integer(kind=8) :: count_l, count_r, idx, count_eval
    !     integer(kind=8) :: n_samples_l, n_samples_r
    !     real(kind=8), allocatable :: tmp_f(:), tmp_y(:,:), tmp_f_copy(:)
    !     integer(kind=8), allocatable :: feature_ids(:), indices(:)
    !     integer(kind=8) :: factor
    !     integer(kind=8) :: ini_f_idx, fin_f_idx
    !     real(kind=8) :: min_val, max_val, rand_val, threshold
    !     integer(kind=8), save :: tot_time_minmax=0_8
    !     integer(kind=8), save :: tot_time_sum_up=0_8
    !     integer(kind=8), save :: tot_time_collect=0_8
    !     integer(kind=8) :: date_value1(8), date_value2(8)

    !     allocate(tot_p(data_holder_ptr%n_outputs))
    !     allocate(tot_l(data_holder_ptr%n_outputs))
    !     allocate(tot_r(data_holder_ptr%n_outputs))
    !     allocate(res_l(data_holder_ptr%n_outputs))
    !     allocate(res_r(data_holder_ptr%n_outputs))
    !     allocate(sum_l(data_holder_ptr%n_outputs))
    !     allocate(sum_r(data_holder_ptr%n_outputs))
    !     allocate(avg_l(data_holder_ptr%n_outputs))
    !     allocate(avg_r(data_holder_ptr%n_outputs))
    !     node_ptr%n_outputs = data_holder_ptr%n_outputs

    !     gain_best = - huge(0d0)
    !     n_rows_inv = 1d0 / dble(node_ptr%n_samples)
    !     n_outs_inv = 1d0 / dble(node_ptr%n_outputs)
    !     tot_p = node_ptr%sum_p
    !     count_eval = 0_8

    !     allocate(tmp_y(node_ptr%n_samples, node_ptr%n_outputs))
    !     allocate(tmp_f(node_ptr%n_samples))
    !     allocate(indices(node_ptr%n_samples))
    !     allocate(feature_ids(node_ptr%n_columns))
    !     if (is_permute_per_node) call permutation(feature_indices, n_columns)


    !     do i=1, node_ptr%n_samples, 1
    !         idx = node_ptr%indices(i)
    !         tmp_y(i,:) = data_holder_ptr%y_ptr%y_r8_ptr(idx,:)
    !     end do

    !     ini_f_idx = feature_indices_scanning_range(1)
    !     fin_f_idx = feature_indices_scanning_range(2)
    !     do f=ini_f_idx, fin_f_idx, 1
    !         fid = feature_indices(f)
    !         ! Useless or Used Feature Skip
    !         if ( node_ptr%is_useless(fid) ) cycle
    !         if ( node_ptr%is_used(fid) .and. hparam_ptr%skip_used_features ) cycle

    !         ! Collect Data
    !         call date_and_time(values=date_value1)
    !         do i=1, node_ptr%n_samples, 1
    !             idx = node_ptr%indices(i)
    !             tmp_f(i) = data_holder_ptr%x_ptr%x_r8_ptr(idx, fid)
    !         end do
    !         call date_and_time(values=date_value2)
    !         tot_time_collect = tot_time_collect + time_diff(date_value1, date_value2)

    !         ! Extract Min-Max Values and generate threshold
    !         call date_and_time(values=date_value1)
    !         if (hparam_ptr%min_samples_leaf .eq. 1_8) then
    !             call get_minmax_r8(min_val, max_val, tmp_f, node_ptr%n_samples)
    !         else
    !             allocate(tmp_f_copy(node_ptr%n_samples))
    !             do i=1, node_ptr%n_samples, 1
    !                 tmp_f_copy(i) = tmp_f(i)
    !             end do
    !             call quick_select_lower(min_val, tmp_f_copy, node_ptr%n_samples, hparam_ptr%min_samples_leaf)
    !             call quick_select_upper(max_val, tmp_f_copy, node_ptr%n_samples, hparam_ptr%min_samples_leaf)
    !             deallocate(tmp_f_copy)
    !         end if
    !         call date_and_time(values=date_value2)
    !         tot_time_minmax = tot_time_minmax + time_diff(date_value1, date_value2)

    !         ! Useless Feature Case
    !         if (min_val .eq. max_val) then
    !             node_ptr%is_useless(fid) = t_
    !             cycle
    !         end if

    !         do r=1, hparam_ptr%n_repeats
    !             ! Random Split
    !             call random_number(rand_val)
    !             threshold = (max_val-min_val) * rand_val + min_val

    !             tot_l   = 0d0
    !             count_l = 0
    !             call date_and_time(values=date_value1)
    !             do i=1, node_ptr%n_samples, 1
    !                 factor = tmp_f(i) .le. threshold
    !                 tot_l = tot_l + tmp_y(i,:) * factor
    !                 count_l = count_l + factor
    !             end do
    !             call date_and_time(values=date_value2)
    !             tot_time_sum_up = tot_time_sum_up + time_diff(date_value1, date_value2)

    !             count_r = node_ptr%n_samples - count_l
    !             if (count_l .eq. 0_8 .or. count_r .eq. 0_8) cycle

    !             tot_r   = tot_p - tot_l
    !             avg_l = tot_l / dble(count_l)
    !             avg_r = tot_r / dble(count_r)
    !             gain = dble(count_l*count_r)*n_rows_inv * sum( (avg_l-avg_r)**2d0 ) * n_outs_inv
    !             if (gain_best .lt. gain) then
    !                 gain_best = gain
    !                 best_fid = fid
    !                 best_threshold = threshold
    !                 res_l = avg_l
    !                 res_r = avg_r
    !                 n_samples_l = count_l
    !                 n_samples_r = count_r
    !                 sum_l = tot_l
    !                 sum_r = tot_r
    !                 count_eval = count_eval+1
    !             end if
    !         end do
    !         if ( hparam_ptr%max_features .ne. -1 ) then
    !             if ( hparam_ptr%max_features .le. f .and. count_eval .ge. 1 ) then
    !                 exit
    !             end if
    !         end if
    !     end do

    !     node_ptr%is_trained = t_
    !     node_ptr%eval_counter = count_eval
    !     node_ptr%gain_best = gain_best

    !     if ( count_eval .eq. 0 ) then
    !         node_ptr%is_terminal = t_
    !         return
    !     end if

    !     node_ptr%feature_id_ = best_fid
    !     node_ptr%threshold_ = best_threshold

    !     allocate(node_ptr%sum_l(node_ptr%n_outputs))
    !     allocate(node_ptr%sum_r(node_ptr%n_outputs))
    !     allocate(node_ptr%response_l(node_ptr%n_outputs))
    !     allocate(node_ptr%response_r(node_ptr%n_outputs))

    !     node_ptr%sum_l = sum_l
    !     node_ptr%sum_r = sum_r
    !     node_ptr%n_samples_l = n_samples_l
    !     node_ptr%n_samples_r = n_samples_r
    !     node_ptr%response_l = res_l
    !     node_ptr%response_r = res_r
    !     call node_ptr%hparam_check(hparam_ptr)

    !     ! print*, "======================================================"
    !     ! print*, "Collect: ", tot_time_collect
    !     ! print*, "MinMax : ", tot_time_minmax
    !     ! print*, "Sum_Up : ", tot_time_sum_up
    ! end subroutine split_extra_tree_regressor_indivisuals


    !> A subroutine to split node by extremely randomized way.
    subroutine split_extra_tree_regressor(this, node_ptrs, data_holder_ptr, hparam_ptr, & 
        n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
        implicit none
        class(node_splitter)               :: this
        type(node_axis_ptr), intent(inout) :: node_ptrs(:)
        type(data_holder), pointer         :: data_holder_ptr
        type(hparam_decisiontree), pointer :: hparam_ptr
        integer(kind=8), intent(in)        :: n_columns
        integer(kind=8), intent(inout)     :: feature_indices(n_columns)
        integer(kind=8), intent(in)        :: feature_indices_scanning_range(2)
        logical(kind=4), intent(in)        :: is_permute_per_node
        integer(kind=8) :: n

        if ( size(node_ptrs) .eq. 1 ) then
            call this%split_extra_tree_regressor_indivisuals(node_ptrs(1)%node_ptr, data_holder_ptr, hparam_ptr, &
                n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
        else
            do n=1, size(node_ptrs), 1
                call this%split_extra_tree_regressor_indivisuals(node_ptrs(n)%node_ptr, data_holder_ptr, hparam_ptr, &
                    n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
            end do
        end if
    end subroutine split_extra_tree_regressor


    !> A subroutine to split node by extremely randomized way.
    subroutine split_extra_tree_regressor_indivisuals(this, node_ptr, data_holder_ptr, hparam_ptr, &
        n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
        implicit none
        class(node_splitter)               :: this
        type(node_axis), pointer           :: node_ptr
        type(data_holder), pointer         :: data_holder_ptr
        type(hparam_decisiontree), pointer :: hparam_ptr
        integer(kind=8), intent(in)        :: n_columns
        integer(kind=8), intent(inout)     :: feature_indices(n_columns)
        integer(kind=8), intent(in)        :: feature_indices_scanning_range(2)
        logical(kind=4), intent(in)        :: is_permute_per_node

        real(kind=8) :: n_rows_inv, n_outs_inv
        real(kind=8), allocatable :: res_l(:), res_r(:), sum_l(:), sum_r(:), avg_l(:), avg_r(:)
        real(kind=8), allocatable :: tot_p(:), tot_l(:), tot_r(:)
        real(kind=8) :: gain, gain_best, best_threshold
        integer(kind=8) :: i_start, i_stop, fid, f, i, j, best_fid, r
        integer(kind=8) :: count_l, count_r, idx, count_eval
        integer(kind=8) :: n_samples_l, n_samples_r
        real(kind=8), allocatable :: tmp_f(:), tmp_y(:,:), tmp_f_copy(:)
        integer(kind=8), allocatable :: feature_ids(:), indices(:)
        integer(kind=8) :: factor
        integer(kind=8) :: ini_f_idx, fin_f_idx
        real(kind=8) :: min_val, max_val, rand_val, threshold
        integer(kind=8), save :: tot_time_minmax=0_8
        integer(kind=8), save :: tot_time_sum_up=0_8
        integer(kind=8), save :: tot_time_collect=0_8
        integer(kind=8) :: date_value1(8), date_value2(8)

        allocate(tot_p(data_holder_ptr%n_outputs))
        allocate(tot_l(data_holder_ptr%n_outputs))
        allocate(tot_r(data_holder_ptr%n_outputs))
        allocate(res_l(data_holder_ptr%n_outputs))
        allocate(res_r(data_holder_ptr%n_outputs))
        allocate(sum_l(data_holder_ptr%n_outputs))
        allocate(sum_r(data_holder_ptr%n_outputs))
        allocate(avg_l(data_holder_ptr%n_outputs))
        allocate(avg_r(data_holder_ptr%n_outputs))
        node_ptr%n_outputs = data_holder_ptr%n_outputs

        gain_best = - huge(0d0)
        n_rows_inv = 1d0 / dble(node_ptr%n_samples)
        n_outs_inv = 1d0 / dble(node_ptr%n_outputs)
        tot_p = node_ptr%sum_p
        count_eval = 0_8

        allocate(tmp_y(node_ptr%n_samples, node_ptr%n_outputs))
        allocate(tmp_f(node_ptr%n_samples))
        allocate(indices(node_ptr%n_samples))
        allocate(feature_ids(node_ptr%n_columns))
        if (is_permute_per_node) call permutation(feature_indices, n_columns)


        do i=1, node_ptr%n_samples, 1
            idx = node_ptr%indices(i)
            tmp_y(i,:) = data_holder_ptr%y_ptr%y_r8_ptr(idx,:)
        end do

        ini_f_idx = feature_indices_scanning_range(1)
        fin_f_idx = feature_indices_scanning_range(2)
        do f=ini_f_idx, fin_f_idx, 1
            fid = feature_indices(f)
            ! Useless or Used Feature Skip
            if ( node_ptr%is_useless(fid) ) cycle
            if ( node_ptr%is_used(fid) .and. hparam_ptr%skip_used_features ) cycle

            ! Collect Data
            call date_and_time(values=date_value1)
            do i=1, node_ptr%n_samples, 1
                idx = node_ptr%indices(i)
                tmp_f(i) = data_holder_ptr%x_ptr%x_r8_ptr(idx, fid)
            end do
            call date_and_time(values=date_value2)
            tot_time_collect = tot_time_collect + time_diff(date_value1, date_value2)

            ! Extract Min-Max Values and generate threshold
            call date_and_time(values=date_value1)
            if (hparam_ptr%min_samples_leaf .eq. 1_8) then
                call get_minmax_r8(min_val, max_val, tmp_f, node_ptr%n_samples)
            else
                allocate(tmp_f_copy(node_ptr%n_samples))
                do i=1, node_ptr%n_samples, 1
                    tmp_f_copy(i) = tmp_f(i)
                end do
                call quick_select_lower(min_val, tmp_f_copy, node_ptr%n_samples, hparam_ptr%min_samples_leaf)
                call quick_select_upper(max_val, tmp_f_copy, node_ptr%n_samples, hparam_ptr%min_samples_leaf)
                deallocate(tmp_f_copy)
            end if
            call date_and_time(values=date_value2)
            tot_time_minmax = tot_time_minmax + time_diff(date_value1, date_value2)

            ! Useless Feature Case
            if (min_val .eq. max_val) then
                node_ptr%is_useless(fid) = t_
                cycle
            end if

            do r=1, hparam_ptr%n_repeats
                ! Random Split
                call random_number(rand_val)
                threshold = (max_val-min_val) * rand_val + min_val

                tot_l   = 0d0
                count_l = 0
                call date_and_time(values=date_value1)
                ! do i=1, node_ptr%n_samples, 1
                !     factor = tmp_f(i) .le. threshold
                !     tot_l = tot_l + tmp_y(i,:) * factor
                !     count_l = count_l + factor
                ! end do
                call count_and_sum_up_gt_r8(tot_r(1), count_r, tmp_y(:,1), tmp_f, threshold, node_ptr%n_samples)
                call date_and_time(values=date_value2)
                tot_time_sum_up = tot_time_sum_up + time_diff(date_value1, date_value2)

                count_l = node_ptr%n_samples - count_r
                if (count_l .eq. 0_8 .or. count_r .eq. 0_8) cycle

                tot_l   = tot_p - tot_r
                avg_l = tot_l / dble(count_l)
                avg_r = tot_r / dble(count_r)
                gain = dble(count_l*count_r)*n_rows_inv * sum( (avg_l-avg_r)**2d0 ) * n_outs_inv
                if (gain_best .lt. gain) then
                    gain_best = gain
                    best_fid = fid
                    best_threshold = threshold
                    res_l = avg_l
                    res_r = avg_r
                    n_samples_l = count_l
                    n_samples_r = count_r
                    sum_l = tot_l
                    sum_r = tot_r
                    count_eval = count_eval+1
                end if
            end do
            if ( hparam_ptr%max_features .ne. -1 ) then
                if ( hparam_ptr%max_features .le. f .and. count_eval .ge. 1 ) then
                    exit
                end if
            end if
        end do

        node_ptr%is_trained = t_
        node_ptr%eval_counter = count_eval
        node_ptr%gain_best = gain_best

        if ( count_eval .eq. 0 ) then
            node_ptr%is_terminal = t_
            return
        end if

        node_ptr%feature_id_ = best_fid
        node_ptr%threshold_ = best_threshold

        allocate(node_ptr%sum_l(node_ptr%n_outputs))
        allocate(node_ptr%sum_r(node_ptr%n_outputs))
        allocate(node_ptr%response_l(node_ptr%n_outputs))
        allocate(node_ptr%response_r(node_ptr%n_outputs))

        node_ptr%sum_l = sum_l
        node_ptr%sum_r = sum_r
        node_ptr%n_samples_l = n_samples_l
        node_ptr%n_samples_r = n_samples_r
        node_ptr%response_l = res_l
        node_ptr%response_r = res_r
        call node_ptr%hparam_check(hparam_ptr)

        ! print*, "======================================================"
        ! print*, "Collect: ", tot_time_collect
        ! print*, "MinMax : ", tot_time_minmax
        ! print*, "Sum_Up : ", tot_time_sum_up
    end subroutine split_extra_tree_regressor_indivisuals


    subroutine split_extra_tree_regressor_faster(this, node_ptrs, data_holder_ptr, hparam_ptr, & 
        n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
        implicit none
        class(node_splitter)               :: this
        type(node_axis_ptr), intent(inout) :: node_ptrs(:)
        type(data_holder), pointer         :: data_holder_ptr
        type(hparam_decisiontree), pointer :: hparam_ptr
        integer(kind=8), intent(in)        :: n_columns
        integer(kind=8), intent(inout)     :: feature_indices(n_columns)
        integer(kind=8), intent(in)        :: feature_indices_scanning_range(2)
        logical(kind=4), intent(in)        :: is_permute_per_node
        integer(kind=8) :: n

        if ( size(node_ptrs) .eq. 1 ) then
            call this%split_extra_tree_regressor_faster_indivisuals(node_ptrs(1)%node_ptr, data_holder_ptr, hparam_ptr, &
                n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
        else
            do n=1, size(node_ptrs), 1
                call this%split_extra_tree_regressor_faster_indivisuals(node_ptrs(n)%node_ptr, data_holder_ptr, hparam_ptr, &
                    n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
            end do
        end if
    end subroutine split_extra_tree_regressor_faster


    !> A subroutine to split node by extremely randomized way.
    subroutine split_extra_tree_regressor_faster_indivisuals(this, node_ptr, data_holder_ptr, hparam_ptr, &
        n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
        implicit none
        class(node_splitter)               :: this
        type(node_axis), pointer           :: node_ptr
        type(data_holder), pointer         :: data_holder_ptr
        type(hparam_decisiontree), pointer :: hparam_ptr
        integer(kind=8), intent(in)        :: n_columns
        integer(kind=8), intent(inout)     :: feature_indices(n_columns)
        integer(kind=8), intent(in)        :: feature_indices_scanning_range(2)
        logical(kind=4), intent(in)        :: is_permute_per_node

        real(kind=8) :: n_rows_inv, n_outs_inv
        real(kind=8), allocatable :: res_l(:), res_r(:), sum_l(:), sum_r(:), avg_l(:), avg_r(:)
        real(kind=8), allocatable :: tot_p(:), tot_l(:), tot_r(:)
        real(kind=8), allocatable :: min_vals(:), max_vals(:), sum_vals_r(:), sum_vals_l(:), sum_vals(:), thr_vals(:)
        real(kind=8), allocatable :: gain_vals(:)
        integer(kind=8), allocatable :: cnt_vals_r(:), cnt_vals_l(:), cnt_vals(:)
        real(kind=8) :: gain, gain_best, best_threshold
        real(kind=8) :: sum0_l, sum0_r
        integer(kind=8) :: i_start, i_stop, fid, f, i, j, best_fid, r
        integer(kind=8) :: count_l, count_r, idx, count_eval
        integer(kind=8) :: n_samples_l, n_samples_r
        real(kind=8), allocatable :: tmp_f(:), tmp_y(:,:), tmp_f_copy(:)
        integer(kind=8), allocatable :: feature_ids(:), indices(:)
        integer(kind=8) :: factor
        integer(kind=8) :: ini_f_idx, fin_f_idx
        real(kind=8) :: min_val, max_val, rand_val, threshold, max_feat_idx
        integer(kind=8), save :: tot_time_minmax=0_8
        integer(kind=8), save :: tot_time_sum_up=0_8
        integer(kind=8), save :: tot_time_collect=0_8
        integer(kind=8) :: date_value1(8), date_value2(8)

        allocate(tot_p(data_holder_ptr%n_outputs))
        allocate(tot_l(data_holder_ptr%n_outputs))
        allocate(tot_r(data_holder_ptr%n_outputs))
        allocate(res_l(data_holder_ptr%n_outputs))
        allocate(res_r(data_holder_ptr%n_outputs))
        allocate(sum_l(data_holder_ptr%n_outputs))
        allocate(sum_r(data_holder_ptr%n_outputs))
        allocate(avg_l(data_holder_ptr%n_outputs))
        allocate(avg_r(data_holder_ptr%n_outputs))
        node_ptr%n_outputs = data_holder_ptr%n_outputs

        gain_best = - huge(0d0)
        n_rows_inv = 1d0 / dble(node_ptr%n_samples)
        n_outs_inv = 1d0 / dble(node_ptr%n_outputs)
        tot_p = node_ptr%sum_p
        count_eval = 0_8

        allocate(tmp_y(node_ptr%n_samples, node_ptr%n_outputs))
        allocate(tmp_f(node_ptr%n_samples))
        allocate(indices(node_ptr%n_samples))
        allocate(feature_ids(node_ptr%n_columns))
        if (is_permute_per_node) call permutation(feature_indices, n_columns)

        do i=1, node_ptr%n_samples, 1
            idx = node_ptr%indices(i)
            tmp_y(i,:) = data_holder_ptr%y_ptr%y_r8_ptr(idx,:)
        end do

        ini_f_idx = feature_indices_scanning_range(1)
        fin_f_idx = feature_indices_scanning_range(2)

        allocate(min_vals(data_holder_ptr%n_columns))
        allocate(max_vals(data_holder_ptr%n_columns))
        call date_and_time(values=date_value1)
        call get_matrix_minmax(min_vals, max_vals, data_holder_ptr%x_t_ptr%x_r8_ptr, & 
            node_ptr%indices, node_ptr%n_samples, data_holder_ptr%n_samples, data_holder_ptr%n_columns)
        call date_and_time(values=date_value2)
        tot_time_minmax = tot_time_minmax + time_diff(date_value1, date_value2)
        ! print*, min_vals-minval(data_holder_ptr%x_t_ptr%x_r8_ptr, dim=2)
        ! print*, max_vals-maxval(data_holder_ptr%x_t_ptr%x_r8_ptr, dim=2)

        allocate(gain_vals(data_holder_ptr%n_columns))
        allocate(thr_vals(data_holder_ptr%n_columns))
        allocate(sum_vals_r(data_holder_ptr%n_columns), sum_vals_l(data_holder_ptr%n_columns), sum_vals(data_holder_ptr%n_columns))
        allocate(cnt_vals_r(data_holder_ptr%n_columns), cnt_vals_l(data_holder_ptr%n_columns), cnt_vals(data_holder_ptr%n_columns))        

        sum_vals(:) = node_ptr%sum_p(1)
        cnt_vals(:) = node_ptr%n_samples

        do r=1, hparam_ptr%n_repeats
            call random_number(thr_vals)
            thr_vals = (max_vals-min_vals)*thr_vals + min_vals
            sum_vals_r = 0d0
            cnt_vals_r = 0_8
            call date_and_time(values=date_value1)
            call get_matrix_count_and_sum_up_gt(sum_vals_r, cnt_vals_r, thr_vals, data_holder_ptr%x_t_ptr%x_r8_ptr, & 
                data_holder_ptr%y_ptr%y_r8_ptr(:,1), node_ptr%indices, node_ptr%n_samples, & 
                data_holder_ptr%n_samples, data_holder_ptr%n_columns)
            call date_and_time(values=date_value2)
            tot_time_sum_up = tot_time_sum_up + time_diff(date_value1, date_value2)

            cnt_vals_l = cnt_vals - cnt_vals_r
            sum_vals_l = sum_vals - sum_vals_r

            gain_vals = - huge(0d0)
            do i=1, data_holder_ptr%n_columns, 1
                count_l = cnt_vals_l(i)
                count_r = cnt_vals_r(i)
                sum0_l = sum_vals_l(i)
                sum0_r = sum_vals_r(i)
                if (count_l*count_r .eq. 0_8) cycle
                gain_vals(i) = dble(count_l*count_r)*n_rows_inv * (sum0_l/count_l-sum0_r/count_r)**2d0
            end do

            max_feat_idx = maxloc(gain_vals, dim=1)
            gain = gain_vals(max_feat_idx)
            if (gain_best .lt. gain) then
                gain_best = gain
                best_fid = max_feat_idx
                best_threshold = thr_vals(best_fid)
                n_samples_l = cnt_vals_l(best_fid)
                n_samples_r = cnt_vals_r(best_fid)
                sum_l = sum_vals_l(best_fid)
                sum_r = sum_vals_r(best_fid)
                res_l = sum_l/n_samples_l
                res_r = sum_r/n_samples_r
                count_eval = count_eval+1
            end if
        end do

        node_ptr%is_trained = t_
        node_ptr%eval_counter = count_eval
        node_ptr%gain_best = gain_best

        if ( count_eval .eq. 0 ) then
            node_ptr%is_terminal = t_
            return
        end if

        node_ptr%feature_id_ = best_fid
        node_ptr%threshold_ = best_threshold

        allocate(node_ptr%sum_l(node_ptr%n_outputs))
        allocate(node_ptr%sum_r(node_ptr%n_outputs))
        allocate(node_ptr%response_l(node_ptr%n_outputs))
        allocate(node_ptr%response_r(node_ptr%n_outputs))

        node_ptr%sum_l = sum_l
        node_ptr%sum_r = sum_r
        node_ptr%n_samples_l = n_samples_l
        node_ptr%n_samples_r = n_samples_r
        node_ptr%response_l = res_l
        node_ptr%response_r = res_r
        call node_ptr%hparam_check(hparam_ptr)

        ! print*, "======================================================"
        ! print*, "Collect: ", tot_time_collect
        ! print*, "MinMax : ", tot_time_minmax
        ! print*, "Sum_Up : ", tot_time_sum_up
    end subroutine split_extra_tree_regressor_faster_indivisuals


    !> A subroutine to split node by 'clouds'.
    subroutine split_clouds_regressor(this, node_ptrs, data_holder_ptr, hparam_ptr, &
        n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
        implicit none
        class(node_splitter)               :: this
        type(node_axis_ptr), intent(inout) :: node_ptrs(:)
        type(data_holder), pointer         :: data_holder_ptr
        type(hparam_decisiontree), pointer :: hparam_ptr
        integer(kind=8), intent(in)        :: n_columns
        integer(kind=8), intent(inout)     :: feature_indices(n_columns)
        integer(kind=8), intent(in)        :: feature_indices_scanning_range(2)
        logical(kind=4), intent(in)        :: is_permute_per_node
        integer(kind=8) :: n

        if ( size(node_ptrs) .eq. 1 ) then
            call this%split_clouds_regressor_indivisuals(node_ptrs(1)%node_ptr, data_holder_ptr, hparam_ptr, &
                n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
        else
            do n=1, size(node_ptrs), 1
                call this%split_clouds_regressor_indivisuals(node_ptrs(n)%node_ptr, data_holder_ptr, hparam_ptr, &
                n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
            end do
        end if
    end subroutine split_clouds_regressor


    !> A subroutine to split node by 'clouds_regressor'.
    subroutine split_clouds_regressor_indivisuals(this, node_ptr, data_holder_ptr, hparam_ptr, &
        n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
        implicit none
        class(node_splitter)               :: this
        type(node_axis), pointer           :: node_ptr
        type(data_holder), pointer         :: data_holder_ptr
        type(hparam_decisiontree), pointer :: hparam_ptr
        integer(kind=8), intent(in)        :: n_columns
        integer(kind=8), intent(inout)     :: feature_indices(n_columns)
        integer(kind=8), intent(in)        :: feature_indices_scanning_range(2)
        logical(kind=4), intent(in)        :: is_permute_per_node

        integer(kind=8) :: date_value1(8), date_value2(8), time1, time2, time3

        real(kind=8) :: n_rows_inv, n_outs_inv
        real(kind=8), allocatable :: res_l(:), res_r(:), sum_l(:), sum_r(:), avg_l(:), avg_r(:)
        real(kind=8), allocatable :: tot_p(:), tot_l(:), tot_r(:)
        real(kind=8) :: gain, gain_best, best_threshold
        integer(kind=8) :: i_start, i_stop, fid, f, i, j, k, best_fid, r
        integer(kind=8) :: count_l, count_r, idx, count_eval
        integer(kind=8) :: num_l, num_r
        integer(kind=8) :: ini_f_idx, fin_f_idx
        real(kind=8), allocatable :: tmp_y(:,:), tmp_y_idx(:), tmp_f_copy(:)
        integer(kind=8), allocatable :: feature_ids(:), indices(:)
        integer(kind=8) :: factor, b
        real(kind=8) :: min_val, max_val, rand_val
        
        real(kind=8), allocatable    :: sum_hist_y(:,:), tmp_r(:)
        integer(kind=8), allocatable :: tmp_f(:), count_hist(:), bin_indices(:)

        integer(kind=8) :: buffer_idx(15), buffer_len=15, n_samples_unroll
        integer(kind=8) :: row_idx, bin_idx
        integer(kind=8), save :: tot_time=0, tot_time2=0
        if (node_ptr%depth .eq. 0_8) tot_time=0
        if (node_ptr%depth .eq. 0_8) tot_time2=0

        time1 = 0
        time2 = 0
        time3 = 0
        allocate(tot_p(data_holder_ptr%n_outputs))
        allocate(tot_l(data_holder_ptr%n_outputs))
        allocate(tot_r(data_holder_ptr%n_outputs))
        allocate(res_l(data_holder_ptr%n_outputs))
        allocate(res_r(data_holder_ptr%n_outputs))
        allocate(sum_l(data_holder_ptr%n_outputs))
        allocate(sum_r(data_holder_ptr%n_outputs))
        allocate(avg_l(data_holder_ptr%n_outputs))
        allocate(avg_r(data_holder_ptr%n_outputs))
        node_ptr%n_outputs = data_holder_ptr%n_outputs

        gain_best = - huge(0d0)
        n_rows_inv = 1d0 / dble(node_ptr%n_samples)
        n_outs_inv = 1d0 / dble(node_ptr%n_outputs)
        ! tot_p = node_ptr%sum_p
        ! tot_p = sum(data_holder_ptr%y_ptr%y_r8_ptr(node_ptr%indices,:), dim=1)
        count_eval = 0_8

        i_start = hparam_ptr%min_samples_leaf
        i_stop = node_ptr%n_samples-i_start

        allocate(tmp_y(node_ptr%n_samples, node_ptr%n_outputs))
        allocate(tmp_f(node_ptr%n_samples))
        allocate(tmp_f_copy(node_ptr%n_samples))
        allocate(indices(node_ptr%n_samples))
        allocate(feature_ids(node_ptr%n_columns))
        allocate(sum_hist_y(hparam_ptr%max_bins, data_holder_ptr%n_outputs))
        allocate(count_hist(hparam_ptr%max_bins))
        if (is_permute_per_node) call permutation(feature_indices, n_columns)

        do i=1, node_ptr%n_samples, 1
            idx = node_ptr%indices(i)
            tmp_y(i,:) = data_holder_ptr%y_ptr%y_r8_ptr(idx,:)
        end do
        tot_p(:) = sum(tmp_y, dim=1)

        if ( .not. allocated(node_ptr%hist_self_sum_y) ) then
            ! ---------------------------------------------------------------------------------------------
            ! ---------------------------------------------------------------------------------------------
            ! Transposed
            allocate(node_ptr%hist_self_sum_y(node_ptr%n_columns, hparam_ptr%max_bins, node_ptr%n_outputs))
            allocate(node_ptr%hist_self_count(node_ptr%n_columns, hparam_ptr%max_bins))
            allocate(tmp_r(node_ptr%n_outputs))
            allocate(bin_indices(node_ptr%n_columns))
            node_ptr%hist_self_sum_y(:,:,:) = 0d0
            node_ptr%hist_self_count(:,:)   = 0_4
            n_samples_unroll = node_ptr%n_samples - mod(node_ptr%n_samples, 7)
            call date_and_time(values=date_value1)
            do i=1, node_ptr%n_samples, 1
                row_idx = node_ptr%indices(i)
                tmp_r = data_holder_ptr%y_ptr%y_r8_ptr(row_idx,:)
                bin_indices = data_holder_ptr % x_hist_row(row_idx) % i_i4
                do j=1, node_ptr%n_columns, 1
                    bin_idx = bin_indices(j)
                    node_ptr%hist_self_sum_y(j,bin_idx,:) = node_ptr%hist_self_sum_y(j,bin_idx,:) + tmp_r
                    node_ptr%hist_self_count(j,bin_idx)   = node_ptr%hist_self_count(j,bin_idx) + 1_4
                end do
            end do
            call date_and_time(values=date_value2)

            ! ---------------------------------------------------------------------------------------------
            ! ---------------------------------------------------------------------------------------------
            ! Normal
            ! allocate(node_ptr%hist_self_sum_y(hparam_ptr%max_bins, node_ptr%n_columns, node_ptr%n_outputs))
            ! allocate(node_ptr%hist_self_count(hparam_ptr%max_bins, node_ptr%n_columns))
            ! allocate(tmp_r(node_ptr%n_outputs))
            ! allocate(bin_indices(node_ptr%n_columns))
            ! node_ptr%hist_self_sum_y(:,:,:) = 0d0
            ! node_ptr%hist_self_count(:,:)   = 0_4
            ! n_samples_unroll = node_ptr%n_samples - mod(node_ptr%n_samples, 7)
            ! call date_and_time(values=date_value1)
            ! do i=1, node_ptr%n_samples, 1
            !     row_idx = node_ptr%indices(i)
            !     tmp_r = data_holder_ptr%y_ptr%y_r8_ptr(row_idx,:)
            !     bin_indices = data_holder_ptr % x_hist_row(row_idx) % i_i4
            !     do j=1, node_ptr%n_columns, 1
            !         bin_idx = bin_indices(j)
            !         node_ptr%hist_self_sum_y(bin_idx,j,:) = node_ptr%hist_self_sum_y(bin_idx,j,:) + tmp_r
            !         node_ptr%hist_self_count(bin_idx,j)   = node_ptr%hist_self_count(bin_idx,j) + 1_4
            !     end do
            ! end do
            ! call date_and_time(values=date_value2)


            ! print*, time_diff(date_value1, date_value2), "[msec]"
        end if

        ini_f_idx = feature_indices_scanning_range(1)
        fin_f_idx = feature_indices_scanning_range(2)
        n_samples_unroll = node_ptr%n_samples - mod(node_ptr%n_samples, buffer_len)
        do f=ini_f_idx, fin_f_idx, 1
            fid = feature_indices(f)
            ! print*, '============================================================='
            ! print*, "FeatureID: ", fid
            ! Useless or Used Feature Skip
            if ( node_ptr%is_useless(fid) ) cycle
            if ( node_ptr%is_used(fid) .and. hparam_ptr%skip_used_features ) cycle

            tot_l = 0d0
            count_l = 0
            do b=1, hparam_ptr%max_bins-1, 1
                ! Transposed
                tot_l = tot_l + node_ptr%hist_self_sum_y(fid,b,:)
                count_l = count_l + node_ptr%hist_self_count(fid,b)
                if (count_l .eq. 0_8) cycle

                ! Normal
                ! tot_l = tot_l + node_ptr%hist_self_sum_y(b,fid,:)
                ! count_l = count_l + node_ptr%hist_self_count(b,fid)
                if (count_l .ge. node_ptr%n_samples) exit

                tot_r = tot_p - tot_l
                count_r = node_ptr%n_samples - count_l
                if (count_r .eq. 0_8) exit

                avg_l = tot_l / dble(count_l)
                avg_r = tot_r / dble(count_r)
                gain = dble(count_l*count_r)*n_rows_inv * sum( (avg_l-avg_r)**2d0 ) * n_outs_inv
                ! print*,    real(tot_p), " : ", &
                !     real(avg_l), real(count_l), &
                !     " : ", real(avg_r), real(count_r), &
                !     " : ", real(gain), real(gain_best), &
                !     " : ", real(res_l), real(res_r), &
                !     " : ", tot_l, tot_r

                if (gain_best .lt. gain) then
                    gain_best = gain
                    best_fid = fid
                    best_threshold = b
                    res_l = avg_l
                    res_r = avg_r
                    num_l = count_l
                    num_r = count_r
                    sum_l = tot_l
                    sum_r = tot_r
                    count_eval = count_eval+1
                end if
            end do
            if ( hparam_ptr%max_features .ne. -1 ) then
                if ( hparam_ptr%max_features .le. f .and. count_eval .ge. 1 ) then
                    exit
                end if
            end if
        end do

        node_ptr%is_trained = t_
        node_ptr%eval_counter = count_eval
        node_ptr%gain_best = gain_best

        if ( count_eval .eq. 0 ) then
            node_ptr%is_terminal = t_
            return
        end if

        node_ptr%feature_id_ = best_fid
        node_ptr%threshold_ = best_threshold

        allocate(node_ptr%sum_l(node_ptr%n_outputs))
        allocate(node_ptr%sum_r(node_ptr%n_outputs))
        allocate(node_ptr%response_l(node_ptr%n_outputs))
        allocate(node_ptr%response_r(node_ptr%n_outputs))

        node_ptr%sum_l = sum_l
        node_ptr%sum_r = sum_r
        node_ptr%n_samples_l = num_l
        node_ptr%n_samples_r = num_r
        node_ptr%response_l = res_l
        node_ptr%response_r = res_r
        call node_ptr%hparam_check(hparam_ptr)
        ! print*, node_ptr%n_samples, num_l, num_r
    end subroutine split_clouds_regressor_indivisuals

    !> A subroutine to split node by 'clouds'.
    subroutine split_weighted_oblique_decision_tree_classifier(this, node_ptrs, data_holder_ptr, hparam_ptr, &
        n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
        implicit none
        class(node_splitter)               :: this
        type(node_axis_ptr), intent(inout) :: node_ptrs(:)
        type(data_holder), pointer         :: data_holder_ptr
        type(hparam_decisiontree), pointer :: hparam_ptr
        integer(kind=8), intent(in)        :: n_columns
        integer(kind=8), intent(inout)     :: feature_indices(n_columns)
        integer(kind=8), intent(in)        :: feature_indices_scanning_range(2)
        logical(kind=4), intent(in)        :: is_permute_per_node
        integer(kind=8) :: n

        if ( size(node_ptrs) .eq. 1 ) then
            call this%split_weighted_oblique_decision_tree_classifier_indivisuals(node_ptrs(1)%node_ptr, data_holder_ptr, & 
                hparam_ptr, n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
        else
            do n=1, size(node_ptrs), 1
                call this%split_weighted_oblique_decision_tree_classifier_indivisuals(node_ptrs(n)%node_ptr, & 
                data_holder_ptr, hparam_ptr, n_columns, feature_indices, &
                feature_indices_scanning_range, is_permute_per_node)
            end do
        end if
    end subroutine split_weighted_oblique_decision_tree_classifier


    !> A subroutine to split node by 'clouds_regressor'.
    subroutine split_weighted_oblique_decision_tree_classifier_indivisuals(this, node_ptr, data_holder_ptr, hparam_ptr, &
        n_columns, feature_indices, feature_indices_scanning_range, is_permute_per_node)
        implicit none
        class(node_splitter)               :: this
        type(node_axis), pointer           :: node_ptr
        type(data_holder), pointer         :: data_holder_ptr
        type(hparam_decisiontree), pointer :: hparam_ptr
        integer(kind=8), intent(in)        :: n_columns
        integer(kind=8), intent(inout)     :: feature_indices(n_columns)
        integer(kind=8), intent(in)        :: feature_indices_scanning_range(2)
        logical(kind=4), intent(in)        :: is_permute_per_node

        integer(kind=8) :: date_value1(8), date_value2(8), time1, time2, time3

        real(kind=8) :: n_rows_inv, n_outs_inv
        real(kind=8), allocatable :: res_l(:), res_r(:), sum_l(:), sum_r(:), avg_l(:), avg_r(:)
        real(kind=8), allocatable :: tot_p(:), tot_l(:), tot_r(:)
        real(kind=8) :: gain, gain_best, best_threshold
        integer(kind=8) :: i_start, i_stop, fid, f, i, j, k, best_fid, r
        integer(kind=8) :: count_l, count_r, idx, count_eval
        integer(kind=8) :: num_l, num_r
        integer(kind=8) :: ini_f_idx, fin_f_idx
        real(kind=8), allocatable :: tmp_y(:,:), tmp_y_idx(:), tmp_f_copy(:)
        integer(kind=8), allocatable :: feature_ids(:), indices(:)
        integer(kind=8) :: factor, b
        real(kind=8) :: min_val, max_val, rand_val
        
        real(kind=8), allocatable    :: sum_hist_y(:,:), tmp_r(:)
        integer(kind=8), allocatable :: tmp_f(:), count_hist(:), bin_indices(:)

        integer(kind=8) :: buffer_idx(15), buffer_len=15, n_samples_unroll
        integer(kind=8) :: row_idx, bin_idx
        integer(kind=8), save :: tot_time=0, tot_time2=0
        if (node_ptr%depth .eq. 0_8) tot_time=0
        if (node_ptr%depth .eq. 0_8) tot_time2=0

        time1 = 0
        time2 = 0
        time3 = 0
        allocate(tot_p(data_holder_ptr%n_outputs))
        allocate(tot_l(data_holder_ptr%n_outputs))
        allocate(tot_r(data_holder_ptr%n_outputs))
        allocate(res_l(data_holder_ptr%n_outputs))
        allocate(res_r(data_holder_ptr%n_outputs))
        allocate(sum_l(data_holder_ptr%n_outputs))
        allocate(sum_r(data_holder_ptr%n_outputs))
        allocate(avg_l(data_holder_ptr%n_outputs))
        allocate(avg_r(data_holder_ptr%n_outputs))
        node_ptr%n_outputs = data_holder_ptr%n_outputs

        gain_best = - huge(0d0)
        n_rows_inv = 1d0 / dble(node_ptr%n_samples)
        n_outs_inv = 1d0 / dble(node_ptr%n_outputs)
        ! tot_p = node_ptr%sum_p
        ! tot_p = sum(data_holder_ptr%y_ptr%y_r8_ptr(node_ptr%indices,:), dim=1)
        count_eval = 0_8

        i_start = hparam_ptr%min_samples_leaf
        i_stop = node_ptr%n_samples-i_start

        allocate(tmp_y(node_ptr%n_samples, node_ptr%n_outputs))
        allocate(tmp_f(node_ptr%n_samples))
        allocate(tmp_f_copy(node_ptr%n_samples))
        allocate(indices(node_ptr%n_samples))
        allocate(feature_ids(node_ptr%n_columns))
        allocate(sum_hist_y(hparam_ptr%max_bins, data_holder_ptr%n_outputs))
        allocate(count_hist(hparam_ptr%max_bins))
        if (is_permute_per_node) call permutation(feature_indices, n_columns)

        do i=1, node_ptr%n_samples, 1
            idx = node_ptr%indices(i)
            tmp_y(i,:) = data_holder_ptr%y_ptr%y_r8_ptr(idx,:)
        end do
        tot_p(:) = sum(tmp_y, dim=1)

        if ( .not. allocated(node_ptr%hist_self_sum_y) ) then
            ! ---------------------------------------------------------------------------------------------
            ! ---------------------------------------------------------------------------------------------
            ! Transposed
            allocate(node_ptr%hist_self_sum_y(node_ptr%n_columns, hparam_ptr%max_bins, node_ptr%n_outputs))
            allocate(node_ptr%hist_self_count(node_ptr%n_columns, hparam_ptr%max_bins))
            allocate(tmp_r(node_ptr%n_outputs))
            allocate(bin_indices(node_ptr%n_columns))
            node_ptr%hist_self_sum_y(:,:,:) = 0d0
            node_ptr%hist_self_count(:,:)   = 0_4
            n_samples_unroll = node_ptr%n_samples - mod(node_ptr%n_samples, 7)
            call date_and_time(values=date_value1)
            do i=1, node_ptr%n_samples, 1
                row_idx = node_ptr%indices(i)
                tmp_r = data_holder_ptr%y_ptr%y_r8_ptr(row_idx,:)
                bin_indices = data_holder_ptr % x_hist_row(row_idx) % i_i4
                do j=1, node_ptr%n_columns, 1
                    bin_idx = bin_indices(j)
                    node_ptr%hist_self_sum_y(j,bin_idx,:) = node_ptr%hist_self_sum_y(j,bin_idx,:) + tmp_r
                    node_ptr%hist_self_count(j,bin_idx)   = node_ptr%hist_self_count(j,bin_idx) + 1_4
                end do
            end do
            call date_and_time(values=date_value2)

            ! ---------------------------------------------------------------------------------------------
            ! ---------------------------------------------------------------------------------------------
            ! Normal
            ! allocate(node_ptr%hist_self_sum_y(hparam_ptr%max_bins, node_ptr%n_columns, node_ptr%n_outputs))
            ! allocate(node_ptr%hist_self_count(hparam_ptr%max_bins, node_ptr%n_columns))
            ! allocate(tmp_r(node_ptr%n_outputs))
            ! allocate(bin_indices(node_ptr%n_columns))
            ! node_ptr%hist_self_sum_y(:,:,:) = 0d0
            ! node_ptr%hist_self_count(:,:)   = 0_4
            ! n_samples_unroll = node_ptr%n_samples - mod(node_ptr%n_samples, 7)
            ! call date_and_time(values=date_value1)
            ! do i=1, node_ptr%n_samples, 1
            !     row_idx = node_ptr%indices(i)
            !     tmp_r = data_holder_ptr%y_ptr%y_r8_ptr(row_idx,:)
            !     bin_indices = data_holder_ptr % x_hist_row(row_idx) % i_i4
            !     do j=1, node_ptr%n_columns, 1
            !         bin_idx = bin_indices(j)
            !         node_ptr%hist_self_sum_y(bin_idx,j,:) = node_ptr%hist_self_sum_y(bin_idx,j,:) + tmp_r
            !         node_ptr%hist_self_count(bin_idx,j)   = node_ptr%hist_self_count(bin_idx,j) + 1_4
            !     end do
            ! end do
            ! call date_and_time(values=date_value2)


            ! print*, time_diff(date_value1, date_value2), "[msec]"
        end if

        ini_f_idx = feature_indices_scanning_range(1)
        fin_f_idx = feature_indices_scanning_range(2)
        n_samples_unroll = node_ptr%n_samples - mod(node_ptr%n_samples, buffer_len)
        do f=ini_f_idx, fin_f_idx, 1
            fid = feature_indices(f)
            ! print*, '============================================================='
            ! print*, "FeatureID: ", fid
            ! Useless or Used Feature Skip
            if ( node_ptr%is_useless(fid) ) cycle
            if ( node_ptr%is_used(fid) .and. hparam_ptr%skip_used_features ) cycle

            tot_l = 0d0
            count_l = 0
            do b=1, hparam_ptr%max_bins-1, 1
                ! Transposed
                tot_l = tot_l + node_ptr%hist_self_sum_y(fid,b,:)
                count_l = count_l + node_ptr%hist_self_count(fid,b)
                if (count_l .eq. 0_8) cycle

                ! Normal
                ! tot_l = tot_l + node_ptr%hist_self_sum_y(b,fid,:)
                ! count_l = count_l + node_ptr%hist_self_count(b,fid)
                if (count_l .ge. node_ptr%n_samples) exit

                tot_r = tot_p - tot_l
                count_r = node_ptr%n_samples - count_l
                if (count_r .eq. 0_8) exit

                avg_l = tot_l / dble(count_l)
                avg_r = tot_r / dble(count_r)
                gain = dble(count_l*count_r)*n_rows_inv * sum( (avg_l-avg_r)**2d0 ) * n_outs_inv
                ! print*,    real(tot_p), " : ", &
                !     real(avg_l), real(count_l), &
                !     " : ", real(avg_r), real(count_r), &
                !     " : ", real(gain), real(gain_best), &
                !     " : ", real(res_l), real(res_r), &
                !     " : ", tot_l, tot_r

                if (gain_best .lt. gain) then
                    gain_best = gain
                    best_fid = fid
                    best_threshold = b
                    res_l = avg_l
                    res_r = avg_r
                    num_l = count_l
                    num_r = count_r
                    sum_l = tot_l
                    sum_r = tot_r
                    count_eval = count_eval+1
                end if
            end do
            if ( hparam_ptr%max_features .ne. -1 ) then
                if ( hparam_ptr%max_features .le. f .and. count_eval .ge. 1 ) then
                    exit
                end if
            end if
        end do

        node_ptr%is_trained = t_
        node_ptr%eval_counter = count_eval
        node_ptr%gain_best = gain_best

        if ( count_eval .eq. 0 ) then
            node_ptr%is_terminal = t_
            return
        end if

        node_ptr%feature_id_ = best_fid
        node_ptr%threshold_ = best_threshold

        allocate(node_ptr%sum_l(node_ptr%n_outputs))
        allocate(node_ptr%sum_r(node_ptr%n_outputs))
        allocate(node_ptr%response_l(node_ptr%n_outputs))
        allocate(node_ptr%response_r(node_ptr%n_outputs))

        node_ptr%sum_l = sum_l
        node_ptr%sum_r = sum_r
        node_ptr%n_samples_l = num_l
        node_ptr%n_samples_r = num_r
        node_ptr%response_l = res_l
        node_ptr%response_r = res_r
        call node_ptr%hparam_check(hparam_ptr)
        ! print*, node_ptr%n_samples, num_l, num_r
    end subroutine split_weighted_oblique_decision_tree_classifier_indivisuals

end module mod_splitter
