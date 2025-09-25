module mod_check_non_negative
    use :: iso_fortran_env,       only: error_unit
    use :: mod_kinds,             only: r64, i64
    use :: mod_error_codes,       only: ERR_NEGATIVE_VALUE, ERR_NON_FINITE
    use :: mod_error_manager,     only: raise_error
    use :: mod_character_helpers, only: make_prefix
    use :: mod_predicates,        only: is_finite, is_non_negative
    use :: mod_program_limits,    only: MAX_MSG_LEN
    implicit none

    interface check_non_negative
        module procedure check_non_negative_r64
        module procedure check_non_negative_i64
    end interface ! check_non_negative

contains

    subroutine check_non_negative_r64(a, file, class_name, value_name, fatal)
        real(r64),    intent(in) :: a
        character(*), intent(in) :: file, class_name, value_name
        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        ! Reject NaN / Inf first
        if (.not. is_finite(a)) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must be finite, but got NaN/Inf.")') prefix
            msg = trim(tmp_msg)
            call raise_error(ERR_NON_FINITE, msg, do_fatal)

        ! Reject negative numbers	
        else if (.not. is_non_negative(a)) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must be >= 0, but got ",es23.16,".")') prefix, a
            msg = trim(tmp_msg)
            call raise_error(ERR_NEGATIVE_VALUE, msg, do_fatal)
        end if
    end subroutine check_non_negative_r64

    subroutine check_non_negative_i64(a, file, class_name, value_name, fatal)
        integer(i64), intent(in) :: a
        character(*), intent(in) :: file, class_name, value_name
        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        ! Reject negative numbers	
        if (.not. is_non_negative(a)) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must be >= 0, but got ",i0,".")') prefix, a
            msg = trim(tmp_msg)
            call raise_error(ERR_NEGATIVE_VALUE, msg, do_fatal)
        end if
    end subroutine check_non_negative_i64

end module mod_check_non_negative
