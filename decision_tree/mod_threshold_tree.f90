module mod_threshold_tree
    use mod_const
    use mod_splitter
    use mod_node
    use mod_base_tree
    use mod_kmeans
    use mod_breathing_kmeans
    use mod_hyperparameter
    use mod_data_holder
    implicit none

    type, extends(base_tree) :: threshold_tree
        type(kmeans)    :: km
        type(breathing_kmeans)    :: bkm
    contains
        procedure :: fit     => fit_threshold_tree
        procedure :: predict => predict_threshold_tree
    end type threshold_tree
    
    !> An interface to create new 'threshold_tree'
    interface threshold_tree
        procedure :: new_threshold_tree
    end interface threshold_tree

contains

    function new_threshold_tree(n_clusters)
        implicit none
        type(threshold_tree) :: new_threshold_tree
        integer(kind=8), optional     :: n_clusters

        if (present(n_clusters)) new_threshold_tree%hparam%n_clusters = n_clusters

        ! Default
        new_threshold_tree%is_classification = t_
        new_threshold_tree%hparam%min_samples_leaf = 1_8
        new_threshold_tree%hparam%min_samples_split = 2_8
        new_threshold_tree%hparam%fashion_int = 2_8
    end function new_threshold_tree


    subroutine fit_threshold_tree(this, x, trained_kmeans)
        implicit none
        class(threshold_tree)    :: this
        real(kind=8), intent(in) :: x(:,:)
        type(kmeans), OPTIONAL   :: trained_kmeans

        type(data_holder), target  :: dholder
        type(data_holder), pointer :: dholder_ptr

        integer(kind=8), ALLOCATABLE       :: labels(:,:)
        type(node_axis), target            :: root_node
        type(hparam_decisiontree), target  :: hparam
        type(hparam_decisiontree), pointer :: hparam_ptr
        logical(kind=4)                    :: is_stop

        integer(kind=8)                    :: depth
        type(node_axis_ptr), allocatable   :: selected_node_ptrs(:)
        type(node_splitter)                :: splitter

        this%n_labels_ = this%hparam%n_clusters

        this%km = kmeans(n_clusters=this%hparam%n_clusters, tolerance=1d-6)
        call this%km%fit(x)
        allocate( labels(this%km%n_samples,1) )
        labels(:,1) = this%km%predict(x)

        ! this%bkm = breathing_kmeans(n_clusters=this%hparam%n_clusters, n_clusters_breathing_in=2_8)
        ! call this%bkm%fit(x)
        ! allocate( labels(this%bkm%n_samples,1) )
        ! labels(:,1) = this%bkm%predict(x)

        dholder = data_holder(x, labels)
        dholder_ptr => dholder

        call this%init(dholder_ptr)

        if (associated(this%root_node_axis_ptr)) nullify(this%root_node_axis_ptr)
        this%root_node_axis_ptr => root_node
        call this%init_root_node(dholder_ptr, is_classification=this%is_classification)

        hparam = this%hparam
        hparam_ptr => hparam
        call this%root_node_axis_ptr%hparam_check(hparam_ptr)
        call this%induction_stop_check(hparam_ptr, is_stop)
        if ( is_stop ) return

        depth = 0_8
        do while (t_)
            is_stop = f_
            if (allocated(selected_node_ptrs)) deallocate(selected_node_ptrs)
            allocate(selected_node_ptrs(0))

            call this%extract_split_node_ptrs_axis(selected_node_ptrs, depth)
            call splitter%split_threshold_tree(selected_node_ptrs, dholder_ptr, hparam_ptr, &
                dholder_ptr%n_columns, this%hparam%n_clusters)
            call this%adopt_node_ptrs_axis(selected_node_ptrs, dholder_ptr, hparam_ptr, this%is_classification, &
                this%lr_layer)

            call this%induction_stop_check(hparam_ptr, is_stop)
            if (is_stop) exit

            depth = depth + 1
            if (this%hparam%n_clusters .eq. 2_8) then
                this%root_node_axis_ptr%node_l%is_terminal = t_
                this%root_node_axis_ptr%node_r%is_terminal = t_
                exit ! 2-means 
            end if
        end do

        call this%root_node_axis_ptr%print_node_info_axis()

        call this%postprocess(this%is_classification)
        this%is_trained = t_
    end subroutine fit_threshold_tree


    function predict_threshold_tree(this, x)
        implicit none
        class(threshold_tree)    :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), ALLOCATABLE :: predict_threshold_tree(:,:)
        predict_threshold_tree = this%predict_labels(x)
    end function predict_threshold_tree

end module mod_threshold_tree
