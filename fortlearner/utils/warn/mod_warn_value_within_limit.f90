module mod_warn_value_within_limit
    use :: mod_kinds,             only: r64, i64
    use :: iso_fortran_env,       only: error_unit
    use :: mod_character_helpers, only: bound_str, make_prefix
    use :: mod_program_limits,    only: SAFE_FACTOR_R64, SAFE_FACTOR_I64, HUGE_R64, HUGE_I64
    use :: mod_config,            only: ENABLE_WARNING
    implicit none

    interface warn_value_within_limit
        module procedure warn_value_within_limit_r64
        module procedure warn_value_within_limit_i64
    end interface ! warn_value_within_limit


contains

    subroutine warn_value_within_limit_r64(a, file, class_name, value_name)
        real(r64),           intent(in)           :: a(:,:)          ! 2-D 限定
        character(*),        intent(in)           :: file, class_name, value_name
        real(r64) :: threshold, max_a

        if (.not. ENABLE_WARNING) return

        threshold = HUGE_R64 * SAFE_FACTOR_R64

        if ( any(abs(a) >= threshold) ) then
            max_a = maxval(abs(a))        ! ← もっと情報が欲しければ maxloc で位置も取れる

            write(error_unit,&
                '(a," contains huge values (max |",a,"| = ",es12.5," ≥ ",es12.5,"); results may overflow.")') &
                    make_prefix(file, class_name, value_name),                                    & ! a-1
                    trim(value_name),                                                             & ! a-2
                    max_a,                                                                     & ! es12.5(最大値)
                    threshold                                                                    ! es12.5(閾値)
            flush(error_unit)
        end if
    end subroutine warn_value_within_limit_r64

    subroutine warn_value_within_limit_i64(a, file, class_name, value_name)
        implicit none
        integer(i64),        intent(in)           :: a(:,:)          ! 2-D 限定
        character(*),        intent(in)           :: file, class_name, value_name
        integer(i64) :: threshold, max_a

        if (.not. ENABLE_WARNING) return

        threshold = HUGE_I64 / SAFE_FACTOR_I64

        if ( any(abs(a) >= threshold) ) then
            max_a = maxval(abs(a))

            write(error_unit,&
                '(a," contains huge integer values (max |",a,"| = ",i0,&
                " ≥ ",i0,"); results may overflow.")') &
                    make_prefix(file, class_name, value_name), &
                    trim(value_name),                           &
                    max_a,                                      &
                    threshold
            flush(error_unit)
        end if
    end subroutine warn_value_within_limit_i64
end module mod_warn_value_within_limit
