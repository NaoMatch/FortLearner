module mod_feature_interactions
    use mod_const
    use mod_common
    use mod_timer
    use mod_data_holder

    use mod_metric
    use mod_linear_regression
    implicit none

    type feature_interactions
        character(len=256) :: method
    contains
        procedure :: fit => fit_feature_interactions
    end type feature_interactions
    
    interface feature_interactions
        module procedure new_feature_interactions
    end interface feature_interactions

    type stats_container
        real(kind=8), ALLOCATABLE :: means(:)
        real(kind=8), ALLOCATABLE :: vars(:)
    end type stats_container


contains


    function new_feature_interactions(method)
        implicit none
        character(len=*) :: method
        type(feature_interactions) :: new_feature_interactions
        new_feature_interactions%method = method
    end function new_feature_interactions


    function fit_feature_interactions(data_holder_ptr)
        implicit none
        integer(kind=8), allocatable :: fit_feature_interactions(:)
        type(data_holder), pointer   :: data_holder_ptr

        integer(kind=8) :: n_samples, n_columns, f, fid
        integer(kind=8), allocatable :: subsets(:), features(:), use_features(:)
        type(stats_container_x) :: container

        ! Store Simple Stats of x
        n_samples = data_holder_ptr%n_samples
        n_columns = data_holder_ptr%n_columns
        allocate(container%means(n_columns))
        allocate(container%vars(n_columns))
        container%means = mean(data_holder_ptr%x_ptr%x_r8_ptr, n_samples, n_columns)
        container%vars  = variance(data_holder_ptr%x_ptr%x_r8_ptr, n_samples, n_columns, &
                                    mean_of_matrix=container%means)

        ! Feature Interactions
        allocate(subsets(0))
        allocate(features(n_columns))
        do f=1, n_columns, 1
            features(f) = f
        end do

        do while ( size(features) .gt. 0_8)
            do f=1, size(features), 1
                fid = features(f)
                use_features = subsets + (/fid/)
                if ( size(use_features) .eq. 1_8 ) then
                    
                end if
            end do
        end do
    end function fit_feature_interactions






end module mod_feature_interactions
