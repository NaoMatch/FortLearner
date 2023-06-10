module mod_random_forest
    !$ use omp_lib
    use iso_c_binding
    use mod_const
    use mod_common
    use mod_forest_jitter
    use mod_forest_packing_
    ! use mod_forest_packing
    use mod_decision_tree
    implicit none

    !> A type for 'random_forest_regressor' whose base estimators is 'decision_tree_regressor'
    type random_forest_regressor
        character(len=256) :: algo_name                        !< alogorithm name
        logical(kind=4) :: is_axis_parallel                    !< axis-parallel split or not
        integer(kind=8) :: n_estimators_                       !< number of base estimators 
        integer(kind=8) :: n_outputs_                          !< number of dimension of objective variable
        type(hparam_decisiontree) :: hparam                    !< decision tree hyperparameter
        type(decision_tree_regressor), allocatable :: trees(:) !< array of 'decision_tree_regressor'

        type(forest_jitter) :: jitter
        type(c_ptr) :: handle
        type(forest_packing) :: packer
    contains
        procedure :: fit => fit_random_forest_regressor
        procedure :: predict => predict_random_forest_regressor
        procedure :: dump => dump_random_forest_regressor
        procedure :: load => load_random_forest_regressor
    end type random_forest_regressor
    
    !> An interface to create new 'random_forest_regressor'
    interface random_forest_regressor
        procedure :: new_random_forest_regressor
    end interface random_forest_regressor

contains

    !> A function to create new 'random_forest_regressor'
    !! \param n_estimators number of estimators, must be greater equal 2
    !! \param max_depth max depth. must be greater equal 1
    !! \param boot_strap with or without bootstrap sampling. default value is .false.
    !! \param max_leaf_nodes maximum number of leaf node. must be greater equal 2
    !! \param min_samples_leaf minimum number of samples in node. must be greater than 1
    !! \param fashion how to split node. 'best': split the node with largest information gain, 'depth': split the deepest splittable(not leaf) node, 
    !!        'level': split all specific depth nodes at a time, 'impurity': split the node with largest impurity, 
    !!        'sample': split the node with largest sample size
    !! \param max_features maximum number of features in node split phase. must be greater equal 1
    function new_random_forest_regressor(&
        n_estimators, &
        max_depth, boot_strap, max_leaf_nodes, min_samples_leaf, fashion, max_features, &
        num_threads&
        )
        implicit none
        type(random_forest_regressor) :: new_random_forest_regressor
        type(random_forest_regressor) :: tmp
        integer(kind=8), optional :: n_estimators
        integer(kind=8), optional :: max_depth
        logical(kind=4), optional :: boot_strap
        integer(kind=8), optional :: max_leaf_nodes
        integer(kind=8), optional :: min_samples_leaf
        character(len=*), optional :: fashion
        integer(kind=8), optional :: max_features
        integer(kind=8), optional :: num_threads
        character(len=256) :: fashion_list(5)

        tmp%is_axis_parallel = t_
        tmp%hparam%algo_name = "random_forest_regressor"

        fashion_list(1) = "best"
        fashion_list(2) = "depth"
        fashion_list(3) = "level"
        fashion_list(4) = "impurity"
        fashion_list(5) = "sample"

        ! '============================================================='
        tmp%hparam%boot_strap = t_

        if ( present(n_estimators) ) tmp%hparam%n_estimators = n_estimators
        if ( present(max_depth) ) tmp%hparam%max_depth = max_depth
        if ( present(boot_strap) ) tmp%hparam%boot_strap = boot_strap
        if ( present(max_leaf_nodes) ) tmp%hparam%max_leaf_nodes = max_leaf_nodes
        if ( present(min_samples_leaf) ) tmp%hparam%min_samples_leaf = min_samples_leaf
        if ( present(fashion) ) tmp%hparam%fashion = fashion
        if ( present(max_features) ) tmp%hparam%max_features = max_features
        if ( present(num_threads) ) tmp%hparam%num_threads = num_threads

        call tmp%hparam%validate_int_range("n_estimators",     tmp%hparam%n_estimators,     1_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_depth",        tmp%hparam%max_depth,        1_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_leaf_nodes",   tmp%hparam%max_leaf_nodes,   2_8, huge(1_8))
        call tmp%hparam%validate_int_range("min_samples_leaf", tmp%hparam%min_samples_leaf, 1_8, huge(1_8))
        call tmp%hparam%validate_char_list("fashion",          tmp%hparam%fashion,          fashion_list)
        call tmp%hparam%validate_int_range("max_features",     tmp%hparam%max_features,     1_8, huge(1_8), exception=-1_8)
        call tmp%hparam%validate_int_range("num_threads",     tmp%hparam%num_threads,     1_8, huge(1_8), exception=-1_8)

        tmp%hparam%fashion_int = tmp%hparam%convert_char_to_int(tmp%hparam%fashion, fashion_list)
        new_random_forest_regressor = tmp
    end function new_random_forest_regressor


    !> A subtouine to fit 'random_forest_regressor'. 
    !! \return returns fitted 'random_forest_regressor'
    !! \param data_holder_ptr pointer of data_holder 
    !! \param feature_indices ***OPTIONAL*** Order of features given by hand for 'DeepForest'
    !! \param feature_indices_scanning_range ***OPTIONAL*** The index of the range to be used in the "Tree" when "feature_indices" is given.
    subroutine fit_random_forest_regressor(this, data_holder_ptr, &
        feature_indices, feature_indices_scanning_range)
        implicit none
        class(random_forest_regressor) :: this
        type(data_holder), pointer     :: data_holder_ptr
        integer(kind=8), optional      :: feature_indices(:)
        integer(kind=8), optional      :: feature_indices_scanning_range(2)

        integer(kind=8) :: n, n_columns
        logical(kind=4) :: is_permute_per_node
        integer(kind=8), allocatable :: feature_indices_(:), feature_indices_scanning_range_(:)
        type(decision_tree_regressor) :: dt

        include "../decision_tree/include/set_feature_indices_and_scanning_range.f90"

        if (allocated(this%trees)) deallocate(this%trees)
        allocate(this%trees(this%hparam%n_estimators))

        if (this%hparam%max_features .eq. huge(0_8)) then
            this%hparam%max_features = int(sqrt(data_holder_ptr%n_columns+0d0), kind=8)
        end if

        this%n_estimators_ = this%hparam%n_estimators
        this%n_outputs_ = data_holder_ptr%n_outputs
        CALL OMP_SET_NUM_THREADS(this%hparam%num_threads)
        !$omp parallel private(n, dt) shared(this, data_holder_ptr)
        !$omp do 
        do n=1, this%hparam%n_estimators, 1
            this % trees(n) = decision_tree_regressor( &
                    max_depth = this%hparam%max_depth, &
                    boot_strap = this%hparam%boot_strap, &
                    max_leaf_nodes = this%hparam%max_leaf_nodes, &
                    min_samples_leaf = this%hparam%min_samples_leaf, &
                    fashion = this%hparam%fashion, &
                    max_features = this%hparam%max_features &
                )
            call this % trees(n)%fit(data_holder_ptr)
            ! call dt%fit(data_holder_ptr, &
            !     feature_indices = feature_indices_, & 
            !     feature_indices_scanning_range = feature_indices_scanning_range_&
            !     )
        end do
        !$omp end do
        !$omp end parallel
    end subroutine fit_random_forest_regressor


    !> A function to predict fitted 'random_forest_regressor'.
    !! \return returns predict average response per samples (#samples, 1)
    !! \param x input explanatory array
    function predict_random_forest_regressor(this, x) result(response)
        implicit none
        class(random_forest_regressor) :: this
        real(kind=8), intent(in)       :: x(:,:)
        real(kind=8), allocatable      :: response(:,:)
        integer(kind=8) :: shape_x(2), n_samples, n, i
        procedure(called_branchless_predict), bind(c), pointer :: proc_branchless
        type(decision_tree_regressor) :: tree
        logical(kind=4) :: parallel

        shape_x = shape(x)
        n_samples = shape_x(1)

        allocate(response(n_samples, this%n_outputs_))
        response = 0d0
        if (this%packer%is_packed) then
            CALL OMP_SET_NUM_THREADS(this%hparam%num_threads)
            !$omp parallel private(i)
            !$omp do 
            do i=1, n_samples, 1
                call this%packer%predict_(x(i,:), &
                    this%packer%features_pack, this%packer%thresholds_pack, this%packer%responses_pack, &
                    response(i,:))
            end do
            !$omp end do
            !$omp end parallel
        else
            CALL OMP_SET_NUM_THREADS(this%hparam%num_threads)
            parallel = f_
            if (this%hparam%num_threads>1_8) parallel = t_
            !$omp parallel
            !$omp do private(n, tree) reduction(+:response)
            do n=1, this%n_estimators_, 1
                tree = this%trees(n)
                response = response + tree%predict(x, parallel=parallel)
            end do    
            !$omp end do
            !$omp end parallel
        end if
        response = response / dble(this%n_estimators_)
    end function predict_random_forest_regressor


    subroutine dump_random_forest_regressor(this, file_name)
        implicit none
        class(random_forest_regressor) :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit, i
        open(newunit=newunit, file=file_name, form="unformatted", status="replace")
        write(newunit) this%hparam%n_estimators ! dump fail
        write(newunit) this%n_outputs_ ! dump fail
        do i=1, size(this%trees(:)), 1
            call this%trees(i)%dump_base_tree(newunit)
        end do
        close(newunit)
    end subroutine dump_random_forest_regressor


    subroutine load_random_forest_regressor(this, file_name)
        implicit none
        class(random_forest_regressor) :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8)              :: newunit, i
        open(newunit=newunit, file=file_name, form="unformatted")

        read(newunit) this%n_estimators_
        allocate(this%trees(this%n_estimators_))
        read(newunit) this%n_outputs_
        do i=1, size(this%trees(:)), 1
            call this%trees(i)%load_base_tree(newunit)
        end do
        close(newunit)
    end subroutine load_random_forest_regressor

end module mod_random_forest