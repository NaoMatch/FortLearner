module mod_mean_centerer
    use :: mod_kinds
    use :: mod_base_transformer
    use :: mod_mean
    use :: mod_data_summary,     only: get_data_summary
    use :: mod_memory_helpers,   only: force_deallocate_vec_r64
    implicit none
    
    type, extends(base_transformer) :: mean_centerer
        real(r64), allocatable :: mean(:)
    contains
        procedure :: fit => fit_mean_centerer
        procedure :: transform => transform_mean_centerer
    end type mean_centerer

contains

    function new_mean_centerer(stable) result(obj)
        implicit none
        type(mean_centerer) :: obj
        logical, intent(in), optional :: stable

        obj%name = "mean_centerer"

        ! Set Hyperparameters
        if (present(stable))   obj % stable = stable
    end function new_mean_centerer


    subroutine fit_mean_centerer(this, X)
        implicit none
        class(mean_centerer), intent(inout) :: this
        real(r64), intent(in) :: X(:,:)

        integer(i64) :: n_rows, n_cols
        integer(i64) :: axis, status

        call this%check_X_train(X, file=__FILE__, class_name=this%name, value_name="X")
        call force_deallocate_vec_r64(this%mean)
        this % is_fitted = .false.
        this%summary = get_data_summary(X)
        n_rows = this%summary%n_rows
        n_cols = this%summary%n_cols

        allocate(this%mean(n_cols))

        axis = 1_i64
        call mean_mat_axis_r64(this%mean, X, n_rows, n_cols, axis, status, &
                stable=this%stable, do_check=.false.)
        this % is_fitted = .true.
    end subroutine fit_mean_centerer

    subroutine transform_mean_centerer(this, X)
        implicit none
        class(mean_centerer), intent(inout) :: this
        real(r64), intent(inout)            :: X(:,:)
        integer(i64) :: n
        call this%check_X_predict(X, file=__FILE__, class_name=this%name, value_name="X")

        n = size(X, dim=1)
        X = X - spread(this%mean, dim=1, ncopies=n)
    end subroutine transform_mean_centerer


end module mod_mean_centerer
