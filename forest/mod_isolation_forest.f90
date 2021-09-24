module mod_isolation_forest
    use mod_const
    use mod_isolation_tree
    implicit none
    
    type isolation_forest
        character(len=256) :: algo_name
        logical(kind=4) :: is_axis_parallel
        integer(kind=8) :: n_estimators_
        integer(kind=8) :: n_outputs_
        type(hparam_decisiontree) :: hparam
        type(isolation_tree), allocatable :: trees(:)
    contains
        procedure :: fit     => fit_isolation_forest
        procedure :: predict => predict_isolation_forest
    end type isolation_forest
    
    interface isolation_forest
        procedure :: new_isolation_forest
    end interface isolation_forest

contains

    function new_isolation_forest(n_estimators, max_samples, contamination)
        implicit none
        type(isolation_forest) :: new_isolation_forest
        type(isolation_forest) :: tmp

        integer(kind=8), optional :: n_estimators
        integer(kind=8), optional :: max_samples
        real(kind=8), optional    :: contamination

        tmp%is_axis_parallel = t_
        tmp%hparam%algo_name = "isolation_forest"
        tmp % algo_name = tmp%hparam%algo_name

        if ( present(n_estimators) )   tmp%hparam%n_estimators   = n_estimators
        if ( present(max_samples) )   tmp%hparam%max_samples   = max_samples
        if ( present(contamination) ) tmp%hparam%contamination = contamination

        call tmp%hparam%validate_int_range("n_estimators", tmp%hparam%n_estimators, 1_8, huge(1_8))
        call tmp%hparam%validate_int_range("max_samples", tmp%hparam%max_samples, 2_8, huge(1_8))
        call tmp%hparam%validate_real_range("contamination", tmp%hparam%contamination, epsilon_, .5d0)

        new_isolation_forest = tmp
    end function new_isolation_forest

    subroutine fit_isolation_forest(this, data_holder_ptr)
        implicit none
        class(isolation_forest)    :: this
        type(data_holder), pointer :: data_holder_ptr

        integer(kind=8) :: n
        type(isolation_tree) :: itree

        if (allocated(this%trees)) deallocate(this%trees)
        allocate(this%trees(this%hparam%n_estimators))
        this%n_estimators_ = this%hparam%n_estimators

        do n=1, this%hparam%n_estimators, 1
            itree = isolation_tree(& 
                max_samples = this%hparam%max_samples, &
                contamination = this%hparam%contamination)
            call itree%fit(data_holder_ptr)
            this%trees(n) = itree
        end do
    end subroutine fit_isolation_forest

    function predict_isolation_forest(this, x)
        implicit none
        class(isolation_forest)        :: this
        real(kind=8), intent(in)       :: x(:,:)
        real(kind=8), allocatable      :: predict_isolation_forest(:,:)
        integer(kind=8) :: shape_x(2), n_samples, n

        shape_x = shape(x)
        n_samples = shape_x(1)

        allocate(predict_isolation_forest(n_samples, 1))
        predict_isolation_forest = 0d0
        do n=1, this%n_estimators_, 1
            predict_isolation_forest = &
                predict_isolation_forest & 
                + this%trees(n)%predict(x, return_depth=t_)
        end do
        predict_isolation_forest = predict_isolation_forest / dble(this%n_estimators_)
        predict_isolation_forest(:,1) = 2d0**(-predict_isolation_forest(:,1)/avg_depth(n_samples))
    end function predict_isolation_forest

end module mod_isolation_forest
