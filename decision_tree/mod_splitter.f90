module mod_splitter
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

        ! procedure :: split_extra_tree_regressor
        ! procedure :: split_extra_tree_regressor_indivisuals

        ! procedure :: split_clouds_regressor
        ! procedure :: split_clouds_regressor_indivisuals

        ! procedure :: split_random_rotation_tree_regressor
        ! procedure :: split_random_rotation_tree_regressor_indivisuals

        ! procedure :: split_random_rotation_extra_tree_regressor
        ! procedure :: split_random_rotation_extra_tree_regressor_indivisuals
        
        ! procedure :: split_oblivious_extra_tree_regressor
        ! procedure :: split_oblivious_extra_tree_regressor_all
    end type node_splitter


contains

    !> A subroutine to split node by very tradisional way.
    !> L. Breiman, J. Friedman, R. Olshen, and C. Stone, “Classification and Regression Trees”, Wadsworth, Belmont, CA, 1984.
    subroutine split_decision_tree_regressor(this, node_ptrs, data_holder_ptr, hparam_ptr)
        implicit none
        class(node_splitter)               :: this
        type(node_axis_ptr), intent(inout) :: node_ptrs(:)
        type(data_holder), pointer         :: data_holder_ptr
        type(hparam_decisiontree), pointer :: hparam_ptr
        integer(kind=8) :: n

        if ( size(node_ptrs) .eq. 1 ) then
            call this%split_decision_tree_regressor_indivisuals(node_ptrs(1)%node_ptr, data_holder_ptr, hparam_ptr)
        else
            do n=1, size(node_ptrs), 1
                call this%split_decision_tree_regressor_indivisuals(node_ptrs(n)%node_ptr, data_holder_ptr, hparam_ptr)
            end do
        end if
    end subroutine split_decision_tree_regressor


    !> A subroutine to split node by very tradisional way.
    !> L. Breiman, J. Friedman, R. Olshen, and C. Stone, “Classification and Regression Trees”, Wadsworth, Belmont, CA, 1984.
    subroutine split_decision_tree_regressor_indivisuals(this, node_ptr, data_holder_ptr, hparam_ptr)
        implicit none
        class(node_splitter)               :: this
        type(node_axis), intent(inout)     :: node_ptr
        type(data_holder), pointer         :: data_holder_ptr
        type(hparam_decisiontree), pointer :: hparam_ptr

        real(kind=8) :: n_rows_inv, n_outs_inv
        real(kind=8), allocatable :: res_l(:), res_r(:), sum_l(:), sum_r(:), avg_l(:), avg_r(:)
        real(kind=8), allocatable :: tot_p(:), tot_l(:), tot_r(:)
        real(kind=8) :: gain, gain_best, best_threshold
        integer(kind=8) :: i_start, i_stop, fid, f, i, j, best_fid
        integer(kind=8) :: count_l, count_r, idx, count_eval
        integer(kind=8) :: n_samples_l, n_samples_r
        real(kind=8), allocatable :: tmp_f(:), tmp_y(:,:), tmp_f_copy(:), tmp_y_copy(:,:)
        integer(kind=8), allocatable :: feature_ids(:), sorted_indices(:)

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
        allocate(feature_ids(node_ptr%n_columns))

        do i=1, node_ptr%n_columns, 1
            feature_ids(i) = i
        end do
        call permutation(feature_ids, node_ptr%n_columns)

        do i=1, node_ptr%n_samples, 1
            idx = node_ptr%indices(i)
            tmp_y(i,:) = data_holder_ptr%y_ptr%y_r8_ptr(idx,:)
        end do

        do f=1, node_ptr%n_columns, 1
            fid = feature_ids(f)
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


end module mod_splitter
