module mod_base_transformer
    use :: mod_kinds
    use :: mod_base_estimator
    use :: mod_dump_load
    use :: mod_check, only: check_fitted_model
    implicit none

    type, abstract, extends(base_estimator) :: base_transformer
        logical :: stable
    contains
        procedure :: check_X_train
        procedure :: check_X_predict

        procedure :: dump_base_transformer
        procedure :: load_base_transformer
    end type base_transformer
    
contains

    subroutine check_X_train(this, X, file, class_name, value_name)
        implicit none
        class(base_transformer), intent(inout) :: this
        real(r64),    intent(in)       :: X(:,:)
        character(*), intent(in)       :: file
        character(*), intent(in)       :: class_name
        character(*), intent(in)       :: value_name

        integer(i64) :: n_rows

        this%min_n_rows = 2_i64
        this%min_n_cols = 1_i64

        call this%check_X_train_common_r64(X, file, class_name, value_name)
    end subroutine check_X_train

    subroutine check_X_predict(this, X, file, class_name, value_name)
        implicit none
        class(base_transformer), intent(inout) :: this
        real(r64),    intent(in)       :: X(:,:)
        character(*), intent(in)       :: file
        character(*), intent(in)       :: class_name
        character(*), intent(in)       :: value_name

        integer(i64) :: n_rows

        call check_fitted_model(this%is_fitted, file, class_name, func_name="predict")
        call this%check_X_predict_common_r64(X, file, class_name, value_name)
    end subroutine check_X_predict

    subroutine dump_base_transformer(this, unit)
        implicit none
        class(base_transformer)    :: this
        integer(i64), intent(in) :: unit

        call this%dump_base_estimator(unit)
        call dump_logical(unit, this%stable)
    end subroutine dump_base_transformer

    subroutine load_base_transformer(this, unit)
        implicit none
        class(base_transformer)    :: this
        integer(i64), intent(in) :: unit

        call this%load_base_estimator(unit)
        call load_logical(unit, this%stable)
    end subroutine load_base_transformer

end module mod_base_transformer
