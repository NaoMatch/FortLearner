module mod_check_rows_distinct
    use :: mod_kinds
    use :: iso_fortran_env, only : error_unit
    use :: mod_error_codes  , only : ERR_ROWS_IDENTICAL
    use :: mod_error_manager,     only: raise_error
    use :: mod_character_helpers, only : make_prefix
    use :: mod_program_limits, only: ATOL_R64
    use :: mod_program_limits,    only: MAX_MSG_LEN
    implicit none
    public :: check_rows_distinct

contains

    subroutine check_rows_distinct(a, file, class_name, value_name, fatal)
        real(r64), intent(in)            :: a(:,:)
        character(*), intent(in), optional :: file, class_name, value_name

        integer(i64) :: n_rows, i
        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        n_rows = size(a, dim=1)
        do i = 2, n_rows
            if (all(a(i,:) == a(1,:))) then
                prefix = make_prefix(file, class_name, value_name)
                write(tmp_msg,'(a," must have distinct rows, but all rows are identical.")') prefix
                msg = trim(tmp_msg)
                call raise_error(ERR_ROWS_IDENTICAL, msg, do_fatal)
            end if
        end do
    end subroutine

    
end module mod_check_rows_distinct