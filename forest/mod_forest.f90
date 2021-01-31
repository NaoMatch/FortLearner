module mod_forest
    !$ use omp_lib
    use mod_const
    use mod_decision_tree
    use mod_extra_tree
    implicit none

    type random_forest_regressor
        logical(kind=4) :: is_axis_parallel
        integer(kind=8) :: n_estimators_
        integer(kind=8) :: n_outputs_
        type(hparam_decisiontree) :: hparam
        type(decision_tree_regressor), allocatable :: trees(:)
    contains
        procedure :: fit => fit_random_forest_regressor
        procedure :: predict => predict_random_forest_regressor
    end type random_forest_regressor
    
    interface random_forest_regressor
        procedure :: new_random_forest_regressor
    end interface random_forest_regressor


    type extra_trees_regressor
        logical(kind=4) :: is_axis_parallel
        integer(kind=8) :: n_estimators_
        integer(kind=8) :: n_outputs_
        type(hparam_decisiontree) :: hparam
        type(extra_tree_regressor), allocatable :: trees(:)
    contains
        procedure :: fit => fit_extra_trees_regressor
        procedure :: predict => predict_extra_trees_regressor
    end type extra_trees_regressor
    
    interface extra_trees_regressor
        procedure :: new_extra_trees_regressor
    end interface extra_trees_regressor





contains

    function new_random_forest_regressor(&
        n_estimators, &
        max_depth, boot_strap, max_leaf_nodes, min_samples_leaf, fashion, max_features &
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

        call tmp%hparam%validate_int_range("n_estimators",     tmp%hparam%n_estimators,     1_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_depth",        tmp%hparam%max_depth,        1_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_leaf_nodes",   tmp%hparam%max_leaf_nodes,   2_8, huge(1_8))
        call tmp%hparam%validate_int_range("min_samples_leaf", tmp%hparam%min_samples_leaf, 1_8, huge(1_8))
        call tmp%hparam%validate_char_list("fashion",          tmp%hparam%fashion,          fashion_list)
        call tmp%hparam%validate_int_range("max_features",     tmp%hparam%max_features,     1_8, huge(1_8), exception=-1_8)

        tmp%hparam%fashion_int = tmp%hparam%convert_char_to_int(tmp%hparam%fashion, fashion_list)
        new_random_forest_regressor = tmp
    end function new_random_forest_regressor


    function new_extra_trees_regressor(&
        n_estimators, &
        max_depth, boot_strap, max_leaf_nodes, min_samples_leaf, fashion, max_features &
        )
        implicit none
        type(extra_trees_regressor) :: new_extra_trees_regressor
        type(extra_trees_regressor) :: tmp
        integer(kind=8), optional :: n_estimators
        integer(kind=8), optional :: max_depth
        logical(kind=4), optional :: boot_strap
        integer(kind=8), optional :: max_leaf_nodes
        integer(kind=8), optional :: min_samples_leaf
        character(len=*), optional :: fashion
        integer(kind=8), optional :: max_features
        character(len=256) :: fashion_list(5)

        tmp%is_axis_parallel = t_
        tmp%hparam%algo_name = "extra_trees_regressor"

        fashion_list(1) = "best"
        fashion_list(2) = "depth"
        fashion_list(3) = "level"
        fashion_list(4) = "impurity"
        fashion_list(5) = "sample"

        if ( present(n_estimators) ) tmp%hparam%n_estimators = n_estimators
        if ( present(max_depth) ) tmp%hparam%max_depth = max_depth
        if ( present(boot_strap) ) tmp%hparam%boot_strap = boot_strap
        if ( present(max_leaf_nodes) ) tmp%hparam%max_leaf_nodes = max_leaf_nodes
        if ( present(min_samples_leaf) ) tmp%hparam%min_samples_leaf = min_samples_leaf
        if ( present(fashion) ) tmp%hparam%fashion = fashion
        if ( present(max_features) ) tmp%hparam%max_features = max_features

        call tmp%hparam%validate_int_range("n_estimators",     tmp%hparam%n_estimators,     1_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_depth",        tmp%hparam%max_depth,        1_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_leaf_nodes",   tmp%hparam%max_leaf_nodes,   2_8, huge(1_8))
        call tmp%hparam%validate_int_range("min_samples_leaf", tmp%hparam%min_samples_leaf, 1_8, huge(1_8))
        call tmp%hparam%validate_char_list("fashion",          tmp%hparam%fashion,          fashion_list)
        call tmp%hparam%validate_int_range("max_features",     tmp%hparam%max_features,     1_8, huge(1_8), exception=-1_8)

        tmp%hparam%fashion_int = tmp%hparam%convert_char_to_int(tmp%hparam%fashion, fashion_list)
        new_extra_trees_regressor = tmp
    end function new_extra_trees_regressor


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
        !$omp parallel private(dt) shared(data_holder_ptr)
        !$omp do 
        do n=1, this%hparam%n_estimators, 1
            dt = decision_tree_regressor( &
                max_depth = this%hparam%max_depth, &
                boot_strap = this%hparam%boot_strap, &
                max_leaf_nodes = this%hparam%max_leaf_nodes, &
                min_samples_leaf = this%hparam%min_samples_leaf, &
                fashion = this%hparam%fashion, &
                max_features = this%hparam%max_features &
                )
            ! call dt%fit(data_holder_ptr)
            call dt%fit(data_holder_ptr, &
                feature_indices = feature_indices_, & 
                feature_indices_scanning_range = feature_indices_scanning_range_&
                )
            this % trees(n) = dt
        end do
        !$omp end do
        !$omp end parallel
    end subroutine fit_random_forest_regressor


    subroutine fit_extra_trees_regressor(this, data_holder_ptr, &
        feature_indices, feature_indices_scanning_range)
        implicit none
        class(extra_trees_regressor) :: this
        type(data_holder), pointer     :: data_holder_ptr
        integer(kind=8), optional      :: feature_indices(:)
        integer(kind=8), optional      :: feature_indices_scanning_range(2)

        integer(kind=8) :: n, n_columns
        logical(kind=4) :: is_permute_per_node
        integer(kind=8), allocatable :: feature_indices_(:), feature_indices_scanning_range_(:)
        type(extra_tree_regressor) :: et

        include "../decision_tree/include/set_feature_indices_and_scanning_range.f90"

        if (allocated(this%trees)) deallocate(this%trees)
        allocate(this%trees(this%hparam%n_estimators))

        this%n_estimators_ = this%hparam%n_estimators
        this%n_outputs_ = data_holder_ptr%n_outputs

        !$omp parallel private(et) shared(data_holder_ptr)
        !$omp do 
        do n=1, this%hparam%n_estimators, 1
            et = extra_tree_regressor( &
                max_depth = this%hparam%max_depth, &
                boot_strap = this%hparam%boot_strap, &
                max_leaf_nodes = this%hparam%max_leaf_nodes, &
                min_samples_leaf = this%hparam%min_samples_leaf, &
                fashion = this%hparam%fashion, &
                max_features = this%hparam%max_features &
                )
            ! call this%trees(n)%fit(data_holder_ptr)
            call et%fit(data_holder_ptr, &
                feature_indices = feature_indices_, & 
                feature_indices_scanning_range = feature_indices_scanning_range_&
                )
            this % trees(n) = et
        end do
        !$omp end do
        !$omp end parallel
    end subroutine fit_extra_trees_regressor


    function predict_random_forest_regressor(this, x)
        implicit none
        class(random_forest_regressor) :: this
        real(kind=8), intent(in)       :: x(:,:)
        real(kind=8), allocatable      :: predict_random_forest_regressor(:,:)
        integer(kind=8) :: shape_x(2), n_samples, n

        shape_x = shape(x)
        n_samples = shape_x(1)

        allocate(predict_random_forest_regressor(n_samples, this%n_outputs_))
        predict_random_forest_regressor = 0d0
        do n=1, this%n_estimators_, 1
            predict_random_forest_regressor = &
                predict_random_forest_regressor & 
                + this%trees(n)%predict(x)
        end do
        predict_random_forest_regressor = predict_random_forest_regressor / dble(this%n_estimators_)
    end function predict_random_forest_regressor


    function predict_extra_trees_regressor(this, x)
        implicit none
        class(extra_trees_regressor) :: this
        real(kind=8), intent(in)       :: x(:,:)
        real(kind=8), allocatable      :: predict_extra_trees_regressor(:,:)
        integer(kind=8) :: shape_x(2), n_samples, n

        shape_x = shape(x)
        n_samples = shape_x(1)

        allocate(predict_extra_trees_regressor(n_samples, this%n_outputs_))
        predict_extra_trees_regressor = 0d0
        do n=1, this%n_estimators_, 1
            predict_extra_trees_regressor = &
                predict_extra_trees_regressor & 
                + this%trees(n)%predict(x)
        end do
        predict_extra_trees_regressor = predict_extra_trees_regressor / dble(this%n_estimators_)
    end function predict_extra_trees_regressor



end module mod_forest
