module mod_base_estimator
    use :: mod_kinds
    use :: mod_program_limits , only : MAX_NAME_LEN
    use :: mod_config,          only: FORMAT_VER
    use :: mod_check
    use :: mod_data_summary   , only : data_summary
    use :: mod_warn_value_within_limit
    use :: mod_dump_load
   implicit none

    !–––––––––––––––––––––––––––– 1. ABSTRACT TYPE ––––––––––––––––––––––––––––
    type, abstract :: base_estimator
        character(len=MAX_NAME_LEN) :: name = 'NONE'
        type(data_summary)          :: summary
        logical                     :: is_fitted = .false.
        logical                     :: print_log = .false.
        logical                     :: fatal     = .true.

        integer(i64) :: min_n_rows=-1_i64, min_n_cols=-1_i64

    contains
        procedure :: get_class_name
        procedure :: check_X_train_common_r64
        procedure :: check_X_predict_common_r64

        procedure :: dump_base_estimator
        procedure :: load_base_estimator
    end type base_estimator

contains

    subroutine check_X_train_common_r64(this, X, file, class_name, value_name)
        implicit none
        class(base_estimator), intent(in) :: this
        real(r64),    intent(in)          :: X(:,:)
        character(*), intent(in)          :: file
        character(*), intent(in)          :: class_name
        character(*), intent(in)          :: value_name

        ! Errors
        call check_finite(X, file, class_name, value_name)
        call check_min_rows(X, this%min_n_rows, file, class_name, value_name)
        call check_min_cols(X, this%min_n_cols, file, class_name, value_name)
        call check_rows_distinct(X, file, class_name, value_name)

        ! Warnings
        call warn_value_within_limit(X, file, class_name, value_name)
    end subroutine check_X_train_common_r64

    subroutine check_X_predict_common_r64(this, X, file, class_name, value_name)
        implicit none
        class(base_estimator), intent(in) :: this
        real(r64),    intent(in)          :: X(:,:)
        character(*), intent(in)          :: file
        character(*), intent(in)          :: class_name
        character(*), intent(in)          :: value_name

        integer(i64) :: n_cols

        n_cols = size(X, dim=2)

        ! Errors
        call check_finite(X, file, class_name, value_name)
        call check_min_rows(X, 1_i64, file, class_name, value_name)
        call check_eq(                                      &
            n_cols,              "predict size(X, dim=2)",  &
            this%summary%n_cols, "train size(X, dim=2)",    &
            file, class_name)

        ! Warnings
        call warn_value_within_limit(X, file, class_name, value_name)
    end subroutine check_X_predict_common_r64

    function get_class_name(this) result(class_name)
        class(base_estimator), intent(in) :: this
        character(len=MAX_NAME_LEN)       :: class_name
        class_name = this%name
    end function get_class_name

    subroutine dump_base_estimator(this, unit)
        implicit none
        class(base_estimator), intent(in)    :: this
        integer(i64), intent(in) :: unit
        call dump_i64(unit, FORMAT_VER)
        call dump_char_fixed_scalar(unit, this%name)
        call this%summary%dump(unit)
        call dump_logical(unit, this%is_fitted)
        call dump_logical(unit, this%print_log)
    end subroutine dump_base_estimator

    subroutine load_base_estimator(this, unit)
        implicit none
        class(base_estimator), intent(inout)    :: this
        integer(i64), intent(in) :: unit
        integer(i64) :: ACTUAL_VER
        call load_i64(unit, ACTUAL_VER)
        call check_format_version_supported(ACTUAL_VER, FORMAT_VER, FORMAT_VER, &
                                    file=__FILE__,    &
                                    class_name=this%name)
        call load_char_fixed_scalar(unit, this%name)
        call this%summary%load(unit)
        call load_logical(unit, this%is_fitted)
        call load_logical(unit, this%print_log)
    end subroutine load_base_estimator

end module mod_base_estimator
