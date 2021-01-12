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
        ! Classification: class probability
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
    end type node_axis

    !> Node for OBLIQUE decision tree
    type, extends(node_original) :: node_oblq
        real(kind=8), allocatable :: coef_(:)
        real(kind=8)              :: intercept_ = 0d0
        real(kind=8)              :: threshold_ = 0d0
        type(node_oblq), allocatable :: node_l
        type(node_oblq), allocatable :: node_r
        type(node_oblq), ALLOCATABLE :: node_c(:)
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


end module mod_node
