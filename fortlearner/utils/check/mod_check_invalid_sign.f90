module mod_check_invalid_sign
    use :: iso_fortran_env,       only : error_unit
    use :: mod_kinds,             only : r64, i64
    use :: mod_error_codes,       only : ERR_POSITIVE_VALUE, ERR_NEGATIVE_VALUE,         &
                                         ERR_NON_POSITIVE_VALUE, ERR_NON_NEGATIVE_VALUE, &
                                         ERR_NON_FINITE
    use :: mod_error_manager,     only: raise_error
    use :: mod_character_helpers, only : make_prefix
    use :: mod_predicates                     ! provides is_positive / is_negative / is_finite
    use :: mod_program_limits,    only: MAX_MSG_LEN
    implicit none

    private
    public :: check_is_positive_r64,  check_is_positive_i64, &
              check_is_negative_r64,  check_is_negative_i64, &
            check_is_non_positive_r64,  check_is_non_positive_i64, &
            check_is_non_negative_r64,  check_is_non_negative_i64
contains

!-----------------------------------------------------------------
!  real(r64) : Positive value is NOT allowed  (must be < 0)
!-----------------------------------------------------------------
    subroutine check_is_positive_r64(a, file, class_name, value_name, fatal)
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
        ! Reject strictly positive numbers
        else if (is_positive(a)) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must be < 0, but got ",es23.16,".")') prefix, a
            msg = trim(tmp_msg)
            call raise_error(ERR_POSITIVE_VALUE, msg, do_fatal)
        end if
    end subroutine check_is_positive_r64

!-----------------------------------------------------------------
!  integer(i64) : Positive value is NOT allowed  (must be < 0)
!-----------------------------------------------------------------
    subroutine check_is_positive_i64(a, file, class_name, value_name, fatal)
        integer(i64), intent(in) :: a
        character(*), intent(in) :: file, class_name, value_name
        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal


        ! Reject strictly positive integers
        if (is_positive(a)) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must be < 0, but got ",i0,".")') prefix, a
            msg = trim(tmp_msg)
            call raise_error(ERR_POSITIVE_VALUE, msg, do_fatal)
        end if
    end subroutine check_is_positive_i64

!-----------------------------------------------------------------
!  real(r64) : Negative value is NOT allowed  (must be > 0)
!-----------------------------------------------------------------
    subroutine check_is_negative_r64(a, file, class_name, value_name, fatal)
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
        ! Reject strictly negative numbers
        else if (is_negative(a)) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must be > 0, but got ",es23.16,".")') prefix, a
            msg = trim(tmp_msg)
            call raise_error(ERR_NEGATIVE_VALUE, msg, do_fatal)
        end if
    end subroutine check_is_negative_r64

!-----------------------------------------------------------------
!  integer(i64) : Negative value is NOT allowed  (must be > 0)
!-----------------------------------------------------------------
    subroutine check_is_negative_i64(a, file, class_name, value_name, fatal)
        integer(i64), intent(in) :: a
        character(*), intent(in) :: file, class_name, value_name
        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal


        ! Reject strictly negative integers
        if (is_negative(a)) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must be > 0, but got ",i0,".")') prefix, a
            msg = trim(tmp_msg)
            call raise_error(ERR_NEGATIVE_VALUE, msg, do_fatal)
        end if
    end subroutine check_is_negative_i64

!-----------------------------------------------------------------
!  real(r64) : Positive value is NOT allowed  (must be < 0)
!-----------------------------------------------------------------
    subroutine check_is_non_positive_r64(a, file, class_name, value_name, fatal)
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
        ! Reject strictly positive numbers
        else if (is_non_positive(a)) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must be < 0, but got ",es23.16,".")') prefix, a
            msg = trim(tmp_msg)
            call raise_error(ERR_NON_POSITIVE_VALUE, msg, do_fatal)
        end if
    end subroutine check_is_non_positive_r64

!-----------------------------------------------------------------
!  integer(i64) : Positive value is NOT allowed  (must be < 0)
!-----------------------------------------------------------------
    subroutine check_is_non_positive_i64(a, file, class_name, value_name, fatal)
        integer(i64), intent(in) :: a
        character(*), intent(in) :: file, class_name, value_name
        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal


        ! Reject strictly positive integers
        if (is_non_positive(a)) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must be < 0, but got ",i0,".")') prefix, a
            msg = trim(tmp_msg)
            call raise_error(ERR_NON_POSITIVE_VALUE, msg, do_fatal)
        end if
    end subroutine check_is_non_positive_i64

!-----------------------------------------------------------------
!  real(r64) : Negative value is NOT allowed  (must be > 0)
!-----------------------------------------------------------------
    subroutine check_is_non_negative_r64(a, file, class_name, value_name, fatal)
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
        ! Reject strictly negative numbers
        else if (is_non_negative(a)) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must be > 0, but got ",es23.16,".")') prefix, a
            msg = trim(tmp_msg)
            call raise_error(ERR_NON_NEGATIVE_VALUE, msg, do_fatal)
        end if
    end subroutine check_is_non_negative_r64

!-----------------------------------------------------------------
!  integer(i64) : Negative value is NOT allowed  (must be > 0)
!-----------------------------------------------------------------
    subroutine check_is_non_negative_i64(a, file, class_name, value_name, fatal)
        integer(i64), intent(in) :: a
        character(*), intent(in) :: file, class_name, value_name
        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal


        ! Reject strictly negative integers
        if (is_non_negative(a)) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must be > 0, but got ",i0,".")') prefix, a
            msg = trim(tmp_msg)
            call raise_error(ERR_NON_NEGATIVE_VALUE, msg, do_fatal)
        end if
    end subroutine check_is_non_negative_i64

end module mod_check_invalid_sign
