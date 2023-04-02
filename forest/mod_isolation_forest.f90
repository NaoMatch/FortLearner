module mod_isolation_forest
    !$ use omp_lib
    use mod_const
    use mod_isolation_tree
    use mod_metric
    implicit none
    
    !> A type for 'isolation_forest'
    type isolation_forest
        character(len=256) :: algo_name                !< alogorithm name
        logical(kind=4) :: is_axis_parallel=t_         !< axis-parallel split or not
        integer(kind=8) :: n_estimators_               !< number of base estimators
        integer(kind=8) :: n_outputs_               !< number of base estimators
        type(hparam_decisiontree) :: hparam            !< decision tree hyperparameter
        type(isolation_tree), allocatable :: trees(:)  !< array of 'isolation_tree'
    contains
        procedure :: fit     => fit_isolation_forest
        procedure :: predict => predict_isolation_forest
        procedure :: thinning => thinning_isolation_forest
        procedure :: thinning_new  => thinning_new_isolation_forest
        procedure :: dump => dump_isolation_forest
        procedure :: load => load_isolation_forest
    end type isolation_forest
    
    !> An interface to create new 'isolation_forest'
    interface isolation_forest
        procedure :: new_isolation_forest
    end interface isolation_forest

contains

    !> A function to create new 'isolation_forest'
    !! \param n_estimators number of estimators, must be greater equal 2
    !! \param max_samples maximum number of samples per tree. samples are randomly selected. must be greater equal 2
    !! \param contamination contamination ratio
    function new_isolation_forest(n_estimators, max_samples, max_features, &
        thinning_rate, num_threads, feature_selection, split_selection, depth)
        implicit none
        type(isolation_forest) :: new_isolation_forest
        type(isolation_forest) :: tmp

        integer(kind=8), optional :: n_estimators
        integer(kind=8), optional :: max_samples
        integer(kind=8), optional :: max_features
        real(kind=8), optional    :: thinning_rate
        integer(kind=8), optional :: num_threads
        character(len=*), optional :: feature_selection
        character(len=*), optional :: split_selection
        character(len=*), optional :: depth

        tmp%is_axis_parallel = t_
        tmp%hparam%algo_name = "isolation_forest"
        tmp%algo_name = tmp%hparam%algo_name

        if ( present(n_estimators) )   tmp%hparam%n_estimators   = n_estimators
        if ( present(max_samples) )   tmp%hparam%max_samples   = max_samples
        if ( present(max_features) )   tmp%hparam%max_features   = max_features
        if ( present(thinning_rate) ) tmp%hparam%thinning_rate = thinning_rate
        if ( present(num_threads) ) tmp%hparam%num_threads = num_threads
        if ( present(feature_selection) ) tmp%hparam%feature_selection = feature_selection
        if ( present(split_selection) ) tmp%hparam%split_selection = split_selection
        if ( present(depth) ) tmp%hparam%depth = depth

        call tmp%hparam%validate_int_range("n_estimators", tmp%hparam%n_estimators, 1_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_samples", tmp%hparam%max_samples, 2_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_features", tmp%hparam%max_samples, 2_8, huge(1_8))
        call tmp%hparam%validate_real_range("thinning_rate", tmp%hparam%thinning_rate, 0d0, 1d0)
        call tmp%hparam%validate_int_range("num_threads", tmp%hparam%num_threads, 1_8, huge(1_8))
        call tmp%hparam%validate_char_list("feature_selection", tmp%hparam%feature_selection, feature_selection_list)
        call tmp%hparam%validate_char_list("split_selection", tmp%hparam%split_selection, split_selection_list)
        call tmp%hparam%validate_char_list("depth", tmp%hparam%depth, depth_list)

        tmp%hparam%feature_selection_int = tmp%hparam%convert_char_to_int(tmp%hparam%feature_selection, feature_selection_list)
        tmp%hparam%split_selection_int = tmp%hparam%convert_char_to_int(tmp%hparam%split_selection, split_selection_list)
        tmp%hparam%depth_int = tmp%hparam%convert_char_to_int(tmp%hparam%depth, depth_list)

        new_isolation_forest = tmp
    end function new_isolation_forest


    !> A subroutine to fit 'isolation_forest'
    !! \param data_holder_ptr pointer to 'data_holder'
    subroutine fit_isolation_forest(this, data_holder_ptr)
        implicit none
        class(isolation_forest)    :: this
        type(data_holder), pointer :: data_holder_ptr

        integer(kind=8) :: n
        type(isolation_tree) :: itree

        if (allocated(this%trees)) deallocate(this%trees)
        allocate(this%trees(this%hparam%n_estimators))
        this%n_estimators_ = this%hparam%n_estimators

        CALL OMP_SET_NUM_THREADS(this%hparam%num_threads)
        !$omp parallel
        !$omp do private(n, itree)
        do n=1, this%hparam%n_estimators, 1
            ! print*, "tree: ", n
            itree = isolation_tree(& 
                max_samples = this%hparam%max_samples, &
                max_features = this%hparam%max_features, &
                feature_selection = this%hparam%feature_selection)
            call itree%fit(data_holder_ptr)
            this%trees(n) = itree
        end do
        !$omp end do
        !$omp end parallel
    end subroutine fit_isolation_forest


    !> A function to predict anomaly score by fitted 'isolation_forest'
    !! \param x an input explanatory variable to be predicted
    function predict_isolation_forest(this, x, parallel)
        implicit none
        class(isolation_forest)          :: this
        real(kind=8), target, intent(in) :: x(:,:)
        logical(kind=4), optional :: parallel
        logical(kind=4) :: parallel_opt

        real(kind=8), allocatable        :: predict_isolation_forest(:,:)
        real(kind=8), allocatable        :: probas(:,:), tmp_proba(:,:)
        integer(kind=8) :: shape_x(2), n_samples, e, n, i, j, thread_id

        type(isolation_tree) :: itree
        integer(kind=8), allocatable :: indices(:)
        real(kind=8), pointer :: x_ptr(:,:)

        parallel_opt = f_
        if (present(parallel)) parallel_opt = parallel

        x_ptr => x
        shape_x = shape(x)
        n_samples = shape_x(1)

        allocate(predict_isolation_forest(n_samples, 1)); predict_isolation_forest=0d0
        
        if (parallel_opt) then
            allocate(probas(n_samples, 1))
            probas(:,:) = 0d0
            allocate(tmp_proba(n_samples, 1))
            CALL OMP_SET_NUM_THREADS(this%hparam%num_threads)
            !$omp parallel shared(this, x_ptr, probas)
            !$omp do private(e, itree) reduction(+:probas)
            do e=1, this%n_estimators_, 1
                thread_id = omp_get_thread_num()+1
                itree = this%trees(e)
                probas(:,:) = probas(:,:) + itree%predict(x_ptr, return_depth=t_, parallel=t_)
            end do
            !$omp end do
            !$omp end parallel
            predict_isolation_forest(:,:) = probas
        else
            do n=1, this%n_estimators_, 1
                predict_isolation_forest = &
                    predict_isolation_forest & 
                    + this%trees(n)%predict(x, return_depth=t_, parallel=f_)
            end do
        end if
        
        predict_isolation_forest = predict_isolation_forest / dble(this%n_estimators_)
        predict_isolation_forest(:,1) = 2d0**(-predict_isolation_forest(:,1)/avg_depth(n_samples))
    end function predict_isolation_forest


    subroutine thinning_isolation_forest(this, x)
        implicit none
        class(isolation_forest)  :: this
        real(kind=8), intent(in) :: x(:,:)
        

        real(kind=8), allocatable :: samples_depth(:,:)
        real(kind=8), allocatable :: avg_depth_per_tree(:)
        integer(kind=8), allocatable :: indices(:)
        integer(kind=8) :: e, n_samples, n_columns
        integer(kind=8) :: n_estimators_thinning
        type(isolation_tree), allocatable :: tmp_trees(:)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)

        n_estimators_thinning = this%n_estimators_ * this%hparam%thinning_rate

        allocate(avg_depth_per_tree(this%n_estimators_))
        allocate(indices(this%n_estimators_))
        allocate(samples_depth(n_samples,1))
        do e=1, this%n_estimators_, 1
            samples_depth = this%trees(e)%predict(x, return_depth=t_)
            avg_depth_per_tree(e) = sum(samples_depth) / dble(n_samples)
            indices(e) = e
        end do
        
        call quick_argsort(avg_depth_per_tree, indices, this%n_estimators_)
        allocate(tmp_trees(this%n_estimators_))
        tmp_trees(:) = this%trees(indices)
        this%n_estimators_ = this%n_estimators_ - n_estimators_thinning
        deallocate(this%trees)
        allocate(this%trees(this%n_estimators_))
        this%trees = tmp_trees(n_estimators_thinning+1:)
        deallocate(tmp_trees)
    end subroutine thinning_isolation_forest


    subroutine thinning_new_isolation_forest(this, x, y)
        implicit none
        class(isolation_forest) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), intent(in) :: y(:,:)

        real(kind=8), allocatable :: samples_depth(:,:), avg_depth_(:,:), scores(:,:)
        real(kind=8), allocatable :: avg_depth_per_tree(:)
        integer(kind=8), allocatable :: indices(:)
        integer(kind=8) :: e, n_samples, n_columns
        integer(kind=8) :: n_estimators_thinning, counter
        type(isolation_tree), allocatable :: tmp_trees(:)
        real(kind=8), allocatable :: aucs(:)
        type(metrics) :: metric


        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)

        n_estimators_thinning = this%n_estimators_ * this%hparam%thinning_rate

        allocate(avg_depth_per_tree(this%n_estimators_))
        allocate(aucs(this%n_estimators_))
        allocate(indices(this%n_estimators_))
        allocate(samples_depth(n_samples,1))
        allocate(avg_depth_(n_samples,1))
        allocate(scores(n_samples,1))
        do e=1, this%n_estimators_, 1
            samples_depth = this%trees(e)%predict(x, return_depth=t_)
            avg_depth_per_tree(e) = sum(samples_depth) / dble(n_samples)
            indices(e) = e
        end do
        
        call quick_argsort(avg_depth_per_tree, indices, this%n_estimators_)
        allocate(tmp_trees(this%n_estimators_))
        tmp_trees(:) = this%trees(indices)

        counter = 1
        samples_depth = 0d0
        do e=this%n_estimators_, 1, -1
            samples_depth = samples_depth + tmp_trees(e)%predict(x, return_depth=t_)
            avg_depth_ = samples_depth / dble(counter)
            scores(:,1) = 2d0**(-avg_depth_(:,1)/avg_depth(n_samples))

            aucs(e) = metric%auc(y(:,1), scores(:,1))
            counter = counter + 1
        end do
        this%n_estimators_ = this%n_estimators_ - maxloc(aucs, dim=1)
        ! this%n_estimators_ = maxloc(aucs, dim=1)
        ! print*, "this%n_estimators_: ", minval(aucs), this%n_estimators_
        deallocate(this%trees)
        allocate(this%trees(this%n_estimators_))
        ! print*, "this%n_estimators_: ", minval(aucs), size(this%trees)
        this%trees = tmp_trees(maxloc(aucs, dim=1)+1:)
        ! print*, "hogehoge: ", size(this%trees), size(tmp_trees(n_estimators_thinning+1:))
        deallocate(tmp_trees)
    end subroutine thinning_new_isolation_forest


    subroutine dump_isolation_forest(this, file_name)
        implicit none
        class(isolation_forest) :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit, i
        open(newunit=newunit, file=file_name, form="unformatted", status="replace")
        write(newunit) this%hparam%n_estimators ! dump fail
        write(newunit) this%n_outputs_ ! dump fail
        do i=1, size(this%trees(:)), 1
            call this%trees(i)%dump_base_tree(newunit)
        end do
        close(newunit)
    end subroutine dump_isolation_forest


    subroutine load_isolation_forest(this, file_name)
        implicit none
        class(isolation_forest) :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit, i
        open(newunit=newunit, file=file_name, form="unformatted")
        read(newunit) this%n_estimators_; allocate(this%trees(this%n_estimators_))
        read(newunit) this%n_outputs_
        do i=1, size(this%trees(:)), 1
            call this%trees(i)%load_base_tree(newunit)
        end do
        close(newunit)
    end subroutine load_isolation_forest

end module mod_isolation_forest
