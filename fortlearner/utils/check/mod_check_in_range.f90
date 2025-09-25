module mod_check_in_range
    use :: mod_kinds
    use :: mod_error_codes,       only: ERR_ABOVE_MAX, ERR_BELOW_MIN
    use :: mod_error_manager,     only: raise_error
    use :: iso_fortran_env,       only: error_unit
    use :: mod_character_helpers, only: make_prefix
    use :: mod_predicates,        only: is_in_range
    use :: mod_program_limits,    only: MAX_MSG_LEN
    implicit none

    interface check_in_range
        module procedure :: check_in_range_i64
    end interface check_in_range

    interface check_greater_equal
        module procedure :: check_greater_equal_i64
    end interface check_greater_equal

    interface check_less_equal
        module procedure :: check_less_equal_i64
    end interface check_less_equal

contains
    subroutine check_in_range_i64(a, lo, hi, file, class_name, value_name, fatal)
        implicit none
        integer(i64), intent(in)            :: a
        integer(i64), intent(in), optional  :: lo, hi
        character(*), intent(in)            :: file, class_name, value_name
        logical, optional :: fatal

        integer(i64)              :: lo_, hi_
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        ! ------------------ 範囲決定 ---------------------------------
        lo_ = -huge(0_i64) ; if (present(lo)) lo_ = lo
        hi_ =  huge(0_i64) ; if (present(hi)) hi_ = hi
        prefix = make_prefix(file, class_name, value_name, sep=" = ")

        ! ------------------ 判定ロジック ------------------------------
        if (.not. present(lo) .and. present(hi)) then        ! 上側のみ制限
            if (a > hi_) then
                write(tmp_msg,'(a," must be ≤ ",i0,", but got ",i0,".")') prefix, hi_, a
                msg = trim(tmp_msg)
                call raise_error(ERR_ABOVE_MAX, msg, do_fatal)
            end if

        else if (present(lo) .and. .not. present(hi)) then   ! 下側のみ制限
            if (a < lo_) then
                write(tmp_msg,'(a," must be ≥ ",i0,", but got ",i0,".")') prefix, lo_, a
                msg = trim(tmp_msg)
                call raise_error(ERR_BELOW_MIN, msg, do_fatal)
            end if

        else                                                 ! 両側指定
            if ( .not. is_in_range(a, lo=lo_, hi=hi_) ) then
                write(tmp_msg,'(a," must be within [",i0,", ",i0,"], but got ",i0,".")') &
                    prefix, lo_, hi_, a
                msg = trim(tmp_msg)
                if (a < lo_) then
                    call raise_error(ERR_BELOW_MIN, msg, do_fatal)
                else
                    call raise_error(ERR_ABOVE_MAX, msg, do_fatal)
                end if
            end if
        end if
    end subroutine check_in_range_i64

    subroutine check_greater_equal_i64(a, lo, file, class_name, value_name, fatal)
        implicit none
        integer(i64), intent(in) :: a, lo
        character(*), intent(in) :: file, class_name, value_name
        logical, optional :: fatal
        call check_in_range_i64(a, lo=lo, file=file, class_name=class_name, &
                             value_name=value_name, fatal=fatal)
    end subroutine check_greater_equal_i64

    subroutine check_less_equal_i64(a, hi, file, class_name, value_name, fatal)
        implicit none
        integer(i64), intent(in) :: a, hi
        character(*), intent(in) :: file, class_name, value_name
        logical, optional :: fatal
        call check_in_range_i64(a, hi=hi, file=file, class_name=class_name, &
                             value_name=value_name, fatal=fatal)
    end subroutine check_less_equal_i64

end module mod_check_in_range