module mod_hyperparameter
    use mod_const
    use mod_common
    implicit none
    
    type hparam_base
        character(len=256) :: algo_name
    contains
        procedure :: validate_int_range
        procedure :: validate_real_range
        procedure :: validate_char_list
        ! procedure :: warning_int
        ! procedure :: warning_char
        ! procedure :: warning_real
        procedure :: convert_char_to_int
        ! procedure :: true_only
        ! procedure :: false_only
    end type

    type, extends(hparam_base) :: hparam_nipals
        integer(kind=8) :: n_components=5_8
        integer(kind=8) :: max_iteration=100_8
        real(kind=8) :: tolerance=1d-8
    end type hparam_nipals

    type, extends(hparam_base) :: hparam_adaboost
        integer(kind=8)    :: n_estimators=5_8
        character(len=256) :: loss_type="linear"
        integer(kind=8)    :: loss_type_int=1_8
    end type hparam_adaboost

    ! kmeans
    type, extends(hparam_base) :: hparam_dbscan
        integer(kind=8) :: n_neighbors = 10_8
        real(kind=8)    :: radius=1d0
        character(len=256) :: neighbour_algo="kdtree"
        integer(kind=8) :: neighbour_algo_int
        integer(kind=8) :: min_samples_in_leaf = 64_8
    end type hparam_dbscan

    ! kmeans
    type, extends(hparam_base) :: hparam_kmeans
        integer(kind=8) :: n_clusters = 5_8
        integer(kind=8) :: max_iter = 500_8
        real(kind=8)    :: tolerance=1d-6
        integer(kind=8) :: max_samples = 256_8
    end type hparam_kmeans

    ! kmeans
    type, extends(hparam_kmeans) :: hparam_breathing_kmeans
        integer(kind=8) :: n_clusters_breathing_in = 5_8
    end type hparam_breathing_kmeans

    ! Linear Regression
    type, extends(hparam_base) :: hparam_linear_regression
        logical(kind=4) :: fit_intercept=.true.
    end type hparam_linear_regression

    ! Ridge Regression
    type, extends(hparam_linear_regression) :: hparam_lasso_regression
        real(kind=8) :: lambda_l1 = 1d0
        integer(kind=8) :: max_iter = 1000
        real(kind=8) :: tol = 1d-4
    end type hparam_lasso_regression

    ! Lasso Regression
    type, extends(hparam_linear_regression) :: hparam_ridge_regression
        real(kind=8)    :: lambda_l2 = 1d0
    end type hparam_ridge_regression

    ! Tree, Forest, Boosting
    type, extends(hparam_base) :: hparam_discretizer
        integer(kind=8)    :: max_bins = 255_8
        integer(kind=8)    :: max_iteration = 10000_8
        character(len=256) :: strategy
        integer(kind=8)    :: strategy_int = 2_8
        real(kind=8)       :: tolerance=1d-8
    end type hparam_discretizer

    ! Tree, Forest, Boosting
    type, extends(hparam_base) :: hparam_decisiontree
        ! c:cart, s:sadt, g:gradient boost
        integer(kind=8) :: n_estimators             = 100_8
        integer(kind=8) :: criterion_int            = 0 ! c
        integer(kind=8) :: criterion_boost_int      = 0
        integer(kind=8) :: verbose                  = 0
        integer(kind=8) :: max_epoch                = 1000
        integer(kind=8) :: max_patient              = 100
        integer(kind=8) :: max_retry                = 100
        integer(kind=8) :: max_alpha                = 10
        integer(kind=8) :: min_alpha                = 1
        integer(kind=8) :: n_repeats                = 10
        integer(kind=8) :: n_rounds                 = 10
        integer(kind=8) :: max_bins                 = 255 ! c
        integer(kind=8) :: strategy_int             = 1_8
        integer(kind=8) :: max_depth                = huge(0_8) ! c
        integer(kind=8) :: min_samples_split        = 2 ! c
        integer(kind=8) :: min_samples_leaf         = 1 ! c
        integer(kind=8) :: max_features             = huge(0_8) ! c
        integer(kind=8) :: max_feature_use          = huge(0_8)
        integer(kind=8) :: max_leaf_nodes           = huge(0_8) ! c
        integer(kind=8) :: fashion_int              = 10_8
        integer(kind=8) :: print_mod                = 1
        integer(kind=8) :: num_threads_in_node      = 4
        integer(kind=8) :: num_threads_in_forest    = 4
        integer(kind=8) :: step_size_for_multi_grain = 1
        integer(kind=8) :: min_columns_in_grain     = 5
        integer(kind=8) :: n_cascades               = 2
        integer(kind=8) :: n_forest_per_layer       = 2
        integer(kind=8) :: max_samples              = 256
        real(kind=8), ALLOCATABLE :: feature_fractions(:)
        real(kind=8)    :: learning_rate            = .1d0
        real(kind=8)    :: learning_rate_layer      = .9d0
        real(kind=8)    :: drop_rate                = .9
        real(kind=8)    :: update_ratio             = .9
        real(kind=8)    :: momentum                 = .9
        real(kind=8)    :: prunig_threshold         = .9
        real(kind=8)    :: weight_decay             = .9
        real(kind=8)    :: top_ratio                = .1
        real(kind=8)    :: min_weight_fraction_leaf = .0
        real(kind=8)    :: min_impurity_decrease    = .0
        real(kind=8)    :: other_ratio              = .1
        real(kind=8)    :: lambda1                  = 0d0
        real(kind=8)    :: lambda2                  = 1d0
        real(kind=8)    :: row_sampling             = 1d0
        real(kind=8)    :: initial_temperature      = 1000d0
        real(kind=8)    :: minimum_temperature      = 0.0001d0
        real(kind=8)    :: cooling_rate             = 0.99d0
        real(kind=8)    :: contamination            = 0.1d0
        logical(kind=4) :: skip_used_features       = f_
        logical(kind=4) :: boot_strap               = f_
        logical(kind=4) :: random_splitter          = f_
        character(len=256) :: criterion             = "mse" !> mse, mae
        character(len=256) :: strategy              = "quantile" !> uniform, quantile, k-means
        character(len=256) :: fashion               = "best" !> best, depth, level, impurity, sample
        integer(kind=8) :: n_clusters               = 10_8
    end type hparam_decisiontree

    type, extends(hparam_base) :: hparam_sgd_estimator
        character(len=256) :: loss = "squared_loss"
        integer(kind=8)    :: loss_int=1_8 !
        character(len=256) :: penalty = "l2" !
        integer(kind=8)    :: penalty_int=2_8 !
        real(kind=8)       :: alpha = 0.0001d0 !
        real(kind=8)       :: l1_ratio = 0.15d0 !
        logical(kind=4)    :: fit_intercept = t_  !
        integer(kind=8)    :: max_iter=1000_8 !
        real(kind=8)       :: tolerance = 0.001d0 !
        logical(kind=4)    :: shuffle = t_ !
        integer(kind=4)    :: verbose = 0_8 !
        real(kind=8)       :: epsilon = 0.1d0
        character(len=256) :: learning_rate = "invscaling" !
        integer(kind=8)    :: learning_rate_int = 2_8 !
        real(kind=8)       :: learning_rate_initial = 0.01d0 !
        real(kind=8)       :: power_t = 0.25d0 !
        logical(kind=4)    :: early_stopping = f_ !
        real(kind=8)       :: validation_fraction = 0.1d0 !
        integer(kind=8)    :: n_iter_no_change=10_8 !
    end type hparam_sgd_estimator

    type, extends(hparam_base) :: hparam_logistic_regression
        character(len=256) :: penalty = "l2"
        real(kind=8) :: lambda = 1d-3
        real(kind=8) :: tolerance = 1d-4
        integer(kind=8) :: max_iteration=500_8
    end type hparam_logistic_regression

contains

    !> A subroutine to validate character hyperparameter.
    !! \param hparam input hyperparameter
    !! \param min_val minimum value
    !! \param max_val maximum value
    !! \param exception exception value
    subroutine validate_char_list(this, hparam_name, hparam, preset_list, exception)
        implicit none
        class(hparam_base)           :: this
        character(len=*)             :: hparam_name
        character(len=256)           :: hparam
        character(len=256)           :: preset_list(:)
        character(len=256), optional :: exception

        character(len=512)           :: err_msg
        integer(kind=8)              :: i
        
        if ( present(exception) ) then
            if ( exception .eq. hparam ) return
        end if

        if ( any(hparam .eq. preset_list) ) return

        err_msg = 'Error: Hyperparameter "' // hparam_name // '" in "' // trim(this%algo_name) // '"'
        err_msg = trim(err_msg) // ' must be one of "' // trim(preset_list(1)) // '"'
        do i=2, size(preset_list)-1, 1
            err_msg = trim(err_msg) // ', "' // trim(preset_list(i)) // '"'
        end do
        err_msg = trim(err_msg) // ', or "'  // trim(preset_list(size(preset_list))) // '"'
        stop err_msg
    end subroutine validate_char_list


    !> A subroutine to validate real hyperparameter.
    !! \param hparam input hyperparameter
    !! \param min_val minimum value
    !! \param max_val maximum value
    !! \param exception exception value
    subroutine validate_real_range(this, hparam_name, hparam, min_val, max_val, exception)
        implicit none
        class(hparam_base)           :: this
        real(kind=8)                 :: hparam
        real(kind=8)                 :: min_val, max_val
        real(kind=8), optional       :: exception
        character(len=*)             :: hparam_name

        character(len=512)           :: err_msg
        character(:),allocatable     :: str_val
        
        if ( present(exception) ) then
            if ( exception .eq. hparam ) return
        end if

        if (min_val .le. hparam .and. hparam .le. max_val) return

        err_msg = 'Error: Hyperparameter "' // hparam_name // '" in "' // trim(this%algo_name) // '"'

        if ( hparam .lt. min_val) then
            str_val = num2char(min_val)
            err_msg = trim(err_msg) // " must be greater than " // str_val // "."
        elseif ( max_val .lt. hparam) then
            str_val = num2char(max_val)
            err_msg = trim(err_msg) // " must be less than " // str_val // "."
        end if
        stop trim(err_msg)
    end subroutine validate_real_range


    !> A subroutine to validate integer hyperparameter.
    !! \param hparam input hyperparameter
    !! \param min_val minimum value
    !! \param max_val maximum value
    !! \param exception exception value
    subroutine validate_int_range(this, hparam_name, hparam, min_val, max_val, exception)
        implicit none
        class(hparam_base)           :: this
        integer(kind=8)              :: hparam
        integer(kind=8)              :: min_val, max_val
        integer(kind=8), optional    :: exception
        character(len=*)             :: hparam_name

        character(len=512)           :: err_msg
        character(len=10)            :: val2char
        integer(kind=8)              :: num_digit
        logical(kind=8)              :: is_negative
        character(:),allocatable     :: fmt_int
        character(:),allocatable     :: str_val
        
        is_negative = min_val .lt. 0_8

        if ( present(exception) ) then
            if ( exception .eq. hparam ) return
        end if

        if (min_val .le. hparam .and. hparam .le. max_val) return

        err_msg = 'Error: Hyperparameter "' // hparam_name // '" in "' // trim(this%algo_name) // '"'

        if ( hparam .lt. min_val) then
            str_val = num2char(min_val)
            err_msg = trim(err_msg) // " must be greater than " // str_val // "."
        elseif ( max_val .lt. hparam) then
            str_val = num2char(max_val)
            err_msg = trim(err_msg) // " must be less than " // str_val // "."
        end if
        stop trim(err_msg)
    end subroutine validate_int_range


    !> A function to convert input character to integer.
    !! \param hparam input hyperparameter character
    !! \param preset_list preset character list
    function convert_char_to_int(this, hparam, preset_list)
        implicit none
        integer(kind=8)    :: convert_char_to_int
        class(hparam_base) :: this
        character(len=256) :: hparam
        character(len=256) :: preset_list(:)

        integer(kind=8) :: i

        do i=1, size(preset_list), 1
            if ( hparam .eq. preset_list(i) ) then
                convert_char_to_int = i
                return
            end if
        end do
    end function convert_char_to_int

end module mod_hyperparameter