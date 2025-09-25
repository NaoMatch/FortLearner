module mod_warn_non_zero_mean
    use :: mod_kinds
    use :: mod_config, only: ENABLE_WARNING
    use :: iso_fortran_env,    only: error_unit
    use :: mod_character_helpers, only: bound_str, make_prefix
    use :: mod_program_limits, only: ATOL_MEAN_WARN_R64
    implicit none
    
contains

    subroutine warn_non_zero_mean(X, file, class_name, value_name)
        !---------------------------------------------------------------
        ! X          : real(r64) matrix (n_rows, n_cols)
        ! file       : caller's __FILE__
        ! class_name : e.g., "kmeans"
        ! value_name : e.g., "X"
        !---------------------------------------------------------------
        real(r64),    intent(in) :: X(:,:)
        character(*), intent(in) :: file, class_name, value_name

        integer(i64) :: j, n_rows, n_cols
        real(r64)    :: mean_j, max_mean

        if (.not. ENABLE_WARNING) return

        n_rows   = size(X, dim=1)
        n_cols   = size(X, dim=2)
        max_mean = 0.0_r64

        do j = 1, n_cols
            mean_j   = sum(X(:, j)) / real(n_rows, r64)
            max_mean = max(max_mean, abs(mean_j))
        end do

        if (max_mean > ATOL_MEAN_WARN_R64) then
        write(error_unit, '(a," may need centering: max(|mean|) = ", &
            & es13.6," > threshold = ", es13.6)') &
            trim(make_prefix(file, class_name, value_name)), &
            max_mean, ATOL_MEAN_WARN_R64
            flush(error_unit)
        end if
    end subroutine warn_non_zero_mean

end module mod_warn_non_zero_mean
