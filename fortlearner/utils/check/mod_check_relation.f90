module mod_check_relation
    use :: iso_fortran_env,       only: error_unit
    use :: mod_kinds,             only: r64, i64
    use :: mod_error_codes,       only: ERR_RELATION, ERR_NON_FINITE
    use :: mod_error_manager,     only: raise_error
    use :: mod_program_limits,    only: ATOL_R64, MAX_NAME_LEN, MAX_MSG_LEN
    use :: mod_character_helpers, only: make_prefix
    use, intrinsic :: ieee_arithmetic
    use :: mod_predicates,        only: is_close, is_finite 
    implicit none

    interface check_eq
        module procedure check_eq_r64
        module procedure check_eq_i64
    end interface ! check_eq

    interface check_ne
        module procedure check_ne_r64
        module procedure check_ne_i64
    end interface ! check_ne

    interface check_ge
        module procedure check_ge_r64
        module procedure check_ge_i64
    end interface ! check_ge

    interface check_gt
        module procedure check_gt_r64
        module procedure check_gt_i64
    end interface ! check_gt

    interface check_le
        module procedure check_le_r64
        module procedure check_le_i64
    end interface ! check_le

    interface check_lt
        module procedure check_lt_r64
        module procedure check_lt_i64
    end interface ! check_lt


contains

    subroutine check_eq_r64(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)
        real(r64), intent(in)    :: lhs, rhs
        character(*), intent(in) :: lhs_name, rhs_name
        character(*), intent(in) :: file, class_name

        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        if (.not. is_finite(lhs)) then
            prefix = make_prefix(file, class_name, lhs_name)
            write(tmp_msg,'(a," must be finite, but got NaN/Inf.")') prefix
            msg = trim(tmp_msg)
            call raise_error(ERR_NON_FINITE, msg, do_fatal)

        else if (.not. is_finite(rhs)) then
            prefix = make_prefix(file, class_name, rhs_name)
            write(tmp_msg,'(a," must be finite, but got NaN/Inf.")') prefix
            msg = trim(tmp_msg)
            call raise_error(ERR_NON_FINITE, msg, do_fatal)

        else if (.not. is_close(lhs, rhs)) then
            prefix = make_prefix(file, class_name, lhs_name)

            write(tmp_msg, '(a," must be == ",a," (= ",es23.16,"), but got ",es23.16,".")') &
                prefix, rhs_name, rhs, lhs
            msg = trim(tmp_msg)
            call raise_error(ERR_RELATION, msg, do_fatal)
        end if
    end subroutine check_eq_r64

    subroutine check_eq_i64(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)
        integer(i64), intent(in)    :: lhs, rhs
        character(*), intent(in) :: lhs_name, rhs_name
        character(*), intent(in) :: file, class_name

        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        if (lhs /= rhs) then
            prefix = make_prefix(file, class_name, lhs_name)

            write(tmp_msg, '(a," must be == ",a," (= ",i0,"), but got ",i0,".")') &
                prefix, rhs_name, rhs, lhs
            msg = trim(tmp_msg)
            call raise_error(ERR_RELATION, msg, do_fatal)
        end if
    end subroutine check_eq_i64

    subroutine check_ne_r64(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)
        real(r64), intent(in)    :: lhs, rhs
        character(*), intent(in) :: lhs_name, rhs_name
        character(*), intent(in) :: file, class_name

        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        if (is_close(lhs, rhs)) then
            prefix = make_prefix(file, class_name, lhs_name)

            write(tmp_msg, '(a," must be != ",a," (= ",es23.16,"), but got ",es23.16,".")') &
                prefix, rhs_name, rhs, lhs
            msg = trim(tmp_msg)
            call raise_error(ERR_RELATION, msg, do_fatal)
        end if
    end subroutine check_ne_r64

    subroutine check_ne_i64(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)
        integer(i64), intent(in)    :: lhs, rhs
        character(*), intent(in) :: lhs_name, rhs_name
        character(*), intent(in) :: file, class_name

        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        if (lhs == rhs) then
            prefix = make_prefix(file, class_name, lhs_name)

            write(tmp_msg, '(a," must be != ",a," (= ",i0,"), but got ",i0,".")') &
                prefix, rhs_name, rhs, lhs
            msg = trim(tmp_msg)
            call raise_error(ERR_RELATION, msg, do_fatal)
        end if
    end subroutine check_ne_i64

    subroutine check_ge_r64(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)
        real(r64), intent(in)    :: lhs, rhs
        character(*), intent(in) :: lhs_name, rhs_name
        character(*), intent(in) :: file, class_name

        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        if (lhs < rhs) then
            prefix = make_prefix(file, class_name, lhs_name)

            write(tmp_msg, '(a," must be >= ",a," (= ",es23.16,"), but got ",es23.16,".")') &
                prefix, rhs_name, rhs, lhs
            msg = trim(tmp_msg)
            call raise_error(ERR_RELATION, msg, do_fatal)
        end if
    end subroutine check_ge_r64

    subroutine check_ge_i64(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)
        integer(i64), intent(in)    :: lhs, rhs
        character(*), intent(in) :: lhs_name, rhs_name
        character(*), intent(in) :: file, class_name

        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        if (lhs < rhs) then
            prefix = make_prefix(file, class_name, lhs_name)

            write(tmp_msg, '(a," must be >= ",a," (= ",i0,"), but got ",i0,".")') &
                prefix, rhs_name, rhs, lhs
            msg = trim(tmp_msg)
            call raise_error(ERR_RELATION, msg, do_fatal)
        end if
    end subroutine check_ge_i64

    subroutine check_gt_r64(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)
        real(r64), intent(in)    :: lhs, rhs
        character(*), intent(in) :: lhs_name, rhs_name
        character(*), intent(in) :: file, class_name

        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        if (lhs <= rhs) then
            prefix = make_prefix(file, class_name, lhs_name)

            write(tmp_msg, '(a," must be > ",a," (= ",es23.16,"), but got ",es23.16,".")') &
                prefix, rhs_name, rhs, lhs
            msg = trim(tmp_msg)
            call raise_error(ERR_RELATION, msg, do_fatal)
        end if
    end subroutine check_gt_r64

    subroutine check_gt_i64(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)
        integer(i64), intent(in)    :: lhs, rhs
        character(*), intent(in) :: lhs_name, rhs_name
        character(*), intent(in) :: file, class_name

        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        if (lhs <= rhs) then
            prefix = make_prefix(file, class_name, lhs_name)

            write(tmp_msg, '(a," must be > ",a," (= ",i0,"), but got ",i0,".")') &
                prefix, rhs_name, rhs, lhs
            msg = trim(tmp_msg)
            call raise_error(ERR_RELATION, msg, do_fatal)
        end if
    end subroutine check_gt_i64

    subroutine check_le_r64(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)
        real(r64), intent(in)    :: lhs, rhs
        character(*), intent(in) :: lhs_name, rhs_name
        character(*), intent(in) :: file, class_name

        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        if (lhs > rhs) then
            prefix = make_prefix(file, class_name, lhs_name)

            write(tmp_msg, '(a," must be <= ",a," (= ",es23.16,"), but got ",es23.16,".")') &
                prefix, rhs_name, rhs, lhs
            msg = trim(tmp_msg)
            call raise_error(ERR_RELATION, msg, do_fatal)
        end if
    end subroutine check_le_r64

    subroutine check_le_i64(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)
        integer(i64), intent(in)    :: lhs, rhs
        character(*), intent(in) :: lhs_name, rhs_name
        character(*), intent(in) :: file, class_name

        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        if (lhs > rhs) then
            prefix = make_prefix(file, class_name, lhs_name)

            write(tmp_msg, '(a," must be <= ",a," (= ",i0,"), but got ",i0,".")') &
                prefix, rhs_name, rhs, lhs
            msg = trim(tmp_msg)
            call raise_error(ERR_RELATION, msg, do_fatal)
        end if
    end subroutine check_le_i64

    subroutine check_lt_r64(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)
        real(r64), intent(in)    :: lhs, rhs
        character(*), intent(in) :: lhs_name, rhs_name
        character(*), intent(in) :: file, class_name

        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        if (lhs >= rhs) then
            prefix = make_prefix(file, class_name, lhs_name)

            write(tmp_msg, '(a," must be < ",a," (= ",es23.16,"), but got ",es23.16,".")') &
                prefix, rhs_name, rhs, lhs
            msg = trim(tmp_msg)
            call raise_error(ERR_RELATION, msg, do_fatal)
        end if
    end subroutine check_lt_r64

    subroutine check_lt_i64(lhs, lhs_name, rhs, rhs_name, file, class_name, fatal)
        integer(i64), intent(in)    :: lhs, rhs
        character(*), intent(in) :: lhs_name, rhs_name
        character(*), intent(in) :: file, class_name

        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        if (lhs >= rhs) then
            prefix = make_prefix(file, class_name, lhs_name)

            write(tmp_msg, '(a," must be < ",a," (= ",i0,"), but got ",i0,".")') &
                prefix, rhs_name, rhs, lhs
            msg = trim(tmp_msg)
            call raise_error(ERR_RELATION, msg, do_fatal)
        end if
    end subroutine check_lt_i64

end module mod_check_relation
