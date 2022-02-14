module mod_node
    use mod_const
    use mod_hyperparameter
    use mod_data_holder
    use mod_sort
    use mod_timer
    use mod_stats
    use mod_discretizer
    implicit none

    type node_original
        ! Basic Information
        logical(kind=4)              :: is_trained        = f_
        logical(kind=4)              :: is_terminal       = f_
        logical(kind=4)              :: is_hist
        integer(kind=8)              :: n_outputs         = 0   ! Number of outputs
        integer(kind=8)              :: eval_counter      = 0   ! If there is no valid split(eval_counter=0), STOP Partition!
        integer(kind=8)              :: depth             = 0  ! If max_depth <= depth, STOP Partition!
        integer(kind=8)              :: n_columns         = -1
        integer(kind=8)              :: n_samples         = 0 ! Self
        integer(kind=8)              :: n_samples_l       = 0 ! Left child
        integer(kind=8)              :: n_samples_r       = 0 ! Right child
        integer(kind=8), allocatable :: n_samples_c(:) ! Multi-way Split
        integer(kind=8), allocatable :: indices(:) ! Sample Indices in current node

        logical(kind=8), allocatable :: is_used(:) ! Use Features
        logical(kind=8), allocatable :: is_useless(:) ! Useless(no validate split) Features
        logical(kind=8), allocatable :: is_useless_center(:) ! Useless Centroid
        real(kind=8)   :: gain_best   = - huge(0.0d0)
        real(kind=8)   :: gain_best_w = - huge(0.0d0) ! weighted gain = gain * (n_samples_in_current_node) / (all training samples)
        real(kind=8)   :: impurity    =   huge(0.0d0) ! Self
        real(kind=8)   :: impurity_l  =   huge(0.0d0) ! Left child
        real(kind=8)   :: impurity_r  =   huge(0.0d0) ! Right child
        real(kind=8), allocatable :: impurity_c(:) ! Multi-way Split

        ! Responses
        ! variance of y's
        real(kind=8), allocatable :: vars(:) ! variance
        ! sum of y's
        real(kind=8), allocatable :: sum_p(:), sum_l(:), sum_r(:)
        ! average of y's
        ! Clustering: threshold_tree
        integer(kind=8) :: n_clusters
        ! Classification: class probability
        integer(kind=8) :: n_labels
        integer(kind=8), allocatable :: uniq_label(:)
        integer(kind=8) :: label, label_l, label_r
        integer(kind=8), allocatable :: label_counter(:), label_counter_l(:), label_counter_r(:)
        real(kind=8), allocatable :: label_proba(:), label_proba_l(:), label_proba_r(:)
        ! Regression: node responses
        real(kind=8), allocatable :: response(:)   ! Self
        real(kind=8), allocatable :: response_l(:) ! Left child
        real(kind=8), allocatable :: response_r(:) ! Right child
        real(kind=8), allocatable :: response_c(:,:) ! Multi-way Split (n_childlen, n_outputs)
    contains
        procedure :: hparam_check
    end type node_original

    !> Node for AXIS-PARALLEL decision tree
    type, extends(node_original) :: node_axis
        integer(kind=8) :: feature_id_ = -2
        real(kind=8)    :: threshold_  = 0d0
        integer(kind=8) :: label_      = -1
        type(node_axis), allocatable :: node_l
        type(node_axis), allocatable :: node_r
        type(node_axis), ALLOCATABLE :: node_c(:)
        ! Histogram Pointers
        real(kind=8), ALLOCATABLE :: hist_self_sum_y(:,:,:)
        integer(kind=4), ALLOCATABLE :: hist_self_count(:,:)
    contains
        procedure :: print_node_info_axis
    end type node_axis

    !> Node for OBLIQUE decision tree
    type, extends(node_original) :: node_oblq
        real(kind=8), allocatable :: coef_(:)
        real(kind=8)              :: intercept_ = 0d0
        real(kind=8)              :: threshold_ = 0d0
        type(node_oblq), allocatable :: node_l
        type(node_oblq), allocatable :: node_r
        type(node_oblq), ALLOCATABLE :: node_c(:)

        real(kind=8)                 :: tot_sum
        integer(kind=8)              :: tot_cnt
        real(kind=8), allocatable    :: tmp_x(:,:)
        real(kind=8), allocatable    :: tmp_y(:,:)
    contains
        procedure :: print_node_info_oblq
        procedure :: loss_mse
    end type node_oblq

    !> A type for pointer array of node_axis
    type node_axis_ptr
        type(node_axis), pointer :: node_ptr
    end type node_axis_ptr

    !> A type for pointer array of node_oblq
    type node_oblq_ptr
        type(node_oblq), pointer :: node_ptr
    end type node_oblq_ptr

contains

    function loss_mse(this, coefficients)
        class(node_oblq)         :: this
        real(kind=8), intent(in) :: coefficients(:)
        real(kind=8)             :: loss_mse

        real(kind=8), allocatable :: tmp_r(:)
        real(kind=8)    :: tmp_avg_l, tmp_avg_r
        real(kind=8)    :: tmp_sum_l, tmp_sum_r, tot_sum
        integer(kind=8) :: tmp_cnt_l, tmp_cnt_r, tot_cnt, factor, j

        allocate(tmp_r(this%n_samples))
        call multi_mat_vec(this%tmp_x, coefficients, tmp_r, this%n_samples, this%n_columns+1)
        ! print*, this%n_samples, this%n_columns+1
        ! print*, sum(tmp_r)
        tmp_sum_l = 0d0
        tmp_cnt_l = 0_8
        do j=1, this % n_samples, 1
            factor = tmp_r(j) .le. 0d0
            tmp_sum_l = tmp_sum_l + factor * this%tmp_y(j,1)
            tmp_cnt_l = tmp_cnt_l + factor
        end do
        tmp_sum_r = this%tot_sum - tmp_sum_l
        tmp_cnt_r = this%tot_cnt - tmp_cnt_l
        ! print*, loss_mse
        ! print*, tmp_sum_l, tmp_cnt_l
        ! print*, tmp_sum_r, tmp_cnt_r
        tmp_avg_l = tmp_sum_l / dble(tmp_cnt_l)
        tmp_avg_r = tmp_sum_r / dble(tmp_cnt_r)
        loss_mse = - tmp_cnt_l * tmp_cnt_r / dble(this%tot_cnt) * (tmp_avg_l - tmp_avg_r)**2d0
        if (tmp_cnt_l * tmp_cnt_r .eq. 0_8) loss_mse = 0d0
    end function loss_mse

    !> A subroutine to check stop growinng or not.
    !> 1. Pure Node, only one class or no variance.
    !> 2. Number of samples in current node is greater than 'min_samples_split'.
    !> 3. Number of samples in current node is greater equal 2*'min_samples_leaf'.
    !> 4. Reach 'max_depth'.
    subroutine hparam_check(this, hparam_ptr)
        implicit none
        class(node_original) :: this
        type(hparam_decisiontree), pointer :: hparam_ptr

        if ( this%impurity .eq. 0d0 ) then
            ! print*, "impurity"
            this%is_trained = t_
            this%is_terminal = t_
            return
        end if

        if ( hparam_ptr%min_samples_split .gt. this%n_samples ) then
            ! print*, "min_samples_split", this%n_samples
            this%is_trained = t_
            this%is_terminal = t_
            return
        end if

        if ( hparam_ptr%min_samples_leaf*2_8 .ge. this%n_samples ) then
            ! print*, "min_samples_leaf", this%n_samples
            this%is_trained = t_
            this%is_terminal = t_
            return
        end if

        if ( hparam_ptr%max_depth .le. this%depth .and. hparam_ptr%max_depth .ne. -1_8 ) then
            ! print*, "max_depth"
            this%is_trained = t_
            this%is_terminal = t_
            return
        end if
    end subroutine hparam_check


    !> A subroutine to print node informations
    subroutine print_node_info_axis(this)
        implicit none
        class(node_axis) :: this

        ! if ( .not. this%is_terminal ) return

        print*, "==========================================================================================="
        print*, "==========================================================================================="
        print*, "Depth:            ", this%depth
        print*, "Is Terminal:      ", this%is_terminal
        print*, "Is Trained:       ", this%is_trained
        print*, "Split Feature ID: ", this%feature_id_
        print*, "Split threshold:  ", this%threshold_
        print*, "Node Label:       ", this%label_
        print*, "Label_Counter:    ", this%label_counter
        print*, "Response:         ", this%response
        print*, "Impurity:         ", this%impurity
        print*, "Gain:             ", this%gain_best

        print*, "No. Sample:       ", this%n_samples
        print*, "No. Sample_left:  ", this%n_samples_l
        print*, "No. Sample_right: ", this%n_samples_r
        print*, "P = L + R:        ", this%n_samples .eq. this%n_samples_l + this%n_samples_r
        print*, "Allocate L:       ", allocated(this%node_l)
        print*, "Allocate R:       ", allocated(this%node_r)
    end subroutine print_node_info_axis

    subroutine print_node_info_oblq(this)
        implicit none
        class(node_oblq) :: this

        ! if ( .not. this%is_terminal ) return

        print*, "==========================================================================================="
        print*, "==========================================================================================="
        print*, "Depth:            ", this%depth
        print*, "Is Terminal:      ", this%is_terminal
        print*, "Is Trained:       ", this%is_trained
        print*, "Split Coefficient:", this%coef_
        print*, "Split Intercept  :", this%intercept_
        print*, "Split threshold:  ", this%threshold_
        ! print*, "Node Label:       ", this%label_
        print*, "Response:         ", this%response
        print*, "Impurity:         ", this%impurity
        print*, "Gain:             ", this%gain_best

        print*, "No. Sample:       ", this%n_samples
        print*, "No. Sample_left:  ", this%n_samples_l
        print*, "No. Sample_right: ", this%n_samples_r
        print*, "P = L + R:        ", this%n_samples .eq. this%n_samples_l + this%n_samples_r
    end subroutine print_node_info_oblq


end module mod_node
