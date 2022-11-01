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

    !> Type of 'thresold_tree'
    !> https://arxiv.org/pdf/2111.03193.pdf
    type, extends(base_tree) :: threshold_tree
        type(kmeans) :: km
    contains
        procedure :: fit     => fit_threshold_tree
        procedure :: predict => predict_threshold_tree
        procedure :: dump => dump_threshold_tree
        procedure :: load => load_threshold_tree
    end type threshold_tree
    
    !> An interface to create new 'threshold_tree'
    interface threshold_tree
        procedure :: new_threshold_tree
    end interface threshold_tree

contains

    !> A function to create new 'threshold_tree'.
    !! \param n_clusters number of clusters. must be greater equal 2
    function new_threshold_tree(n_clusters)
        implicit none
        type(threshold_tree) :: new_threshold_tree
        integer(kind=8), optional     :: n_clusters

        if (present(n_clusters)) new_threshold_tree%hparam%n_clusters = n_clusters

        ! Default
        new_threshold_tree%is_classification = t_
        new_threshold_tree%hparam%min_samples_leaf = 1_8
        new_threshold_tree%hparam%min_samples_split = 2_8
        new_threshold_tree%hparam%max_leaf_nodes = n_clusters
        new_threshold_tree%hparam%fashion_int = 2_8

        ! Threshold Tree
        new_threshold_tree%is_threshold_tree = t_
    end function new_threshold_tree


    !> A subtouine to fit 'threshold_tree'. 
    !! \return returns fitted 'threshold_tree'
    !! \param x input data
    !! \param trained_kmeans ***OPTIONAL*** if input, the cluster center is chosen as the initial value.
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

        real(kind=8), allocatable, target  :: cluster_centers(:,:)

        integer(kind=8)                    :: depth
        type(node_axis_ptr), allocatable   :: selected_node_ptrs(:)
        type(node_splitter)                :: splitter

        this%n_labels_ = this%hparam%n_clusters
        this%km = kmeans(n_clusters=this%hparam%n_clusters, tolerance=1d-6)
        call this%km%fit(x)
        allocate( labels(this%km%n_samples,1) )
        cluster_centers = this%km%cluster_centers(:,:)

        labels(:,1) = this%km%predict(x)

        if (maxval( labels ) .eq. 1_8) then
            do depth=1, this%hparam%n_clusters, 1
                print*, cluster_centers(:,depth)
            end do
        end if

        this%n_clusters_ = this%hparam%n_clusters

        dholder = data_holder(x, labels)
        dholder%cluster_centers_ptr => cluster_centers
        dholder_ptr => dholder

        call this%init(dholder_ptr)

        if (associated(this%root_node_axis_ptr)) nullify(this%root_node_axis_ptr)
        this%root_node_axis_ptr => root_node
        call this%init_root_node(dholder_ptr, is_classification=this%is_classification)

        hparam = this%hparam
        hparam_ptr => hparam
        call this%root_node_axis_ptr%hparam_check(hparam_ptr)
        is_stop = this%induction_stop_check(hparam_ptr)
        if ( is_stop ) return

        depth = 0_8
        do while (t_)
            is_stop = f_
            if (allocated(selected_node_ptrs)) deallocate(selected_node_ptrs)
            allocate(selected_node_ptrs(0))

            call this%extract_split_node_ptrs_axis(selected_node_ptrs, depth)
            call splitter%split_threshold_tree(selected_node_ptrs, dholder_ptr, hparam_ptr, &
                dholder_ptr%n_columns, this%hparam%n_clusters, this%km%cluster_centers)
            call this%adopt_node_ptrs_axis(selected_node_ptrs, dholder_ptr, hparam_ptr, &
                this%is_classification, this%is_threshold_tree, &
                this%lr_layer)

                is_stop = this%induction_stop_check(hparam_ptr)
            if (is_stop) exit

            depth = depth + 1
            if (this%hparam%n_clusters .eq. 2_8) then
                this%root_node_axis_ptr%node_l%is_terminal = t_
                this%root_node_axis_ptr%node_r%is_terminal = t_
                exit ! 2-means 
            end if
        end do

        ! print*, "***** END *****"
        ! call this%print_info(this%root_node_axis_ptr)
        call this%postprocess(this%is_classification)
        this%is_trained = t_
    end subroutine fit_threshold_tree


    !> A function to predict class for 'x'.
    !! \return predicted class
    !! \param x input
    function predict_threshold_tree(this, x)
        implicit none
        class(threshold_tree)    :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), ALLOCATABLE :: predict_threshold_tree(:,:)
        predict_threshold_tree = this%predict_labels(x)
    end function predict_threshold_tree


    !> A subroutine to dump trained model.
    !! \param file_name output file name.
    subroutine dump_threshold_tree(this, file_name)
        implicit none
        class(threshold_tree)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        stop "NotImplementedError: 'dump_threshold_tree' is not implemented." 
    end subroutine dump_threshold_tree


    !> A subroutine to load trained model.
    !! \param file_name load file name.
    subroutine load_threshold_tree(this, file_name)
        implicit none
        class(threshold_tree)      :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit
        stop "NotImplementedError: 'load_threshold_tree' is not implemented." 
    end subroutine load_threshold_tree  
end module mod_threshold_tree
