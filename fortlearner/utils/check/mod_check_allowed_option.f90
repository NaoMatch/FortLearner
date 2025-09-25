module mod_check_allowed_option
    use :: mod_kinds
    use :: mod_error_codes,       only: ERR_DISALLOWED_OPTION
    use :: mod_error_manager,     only: raise_error
    use :: iso_fortran_env,       only: error_unit
    use :: mod_character_helpers, only: make_prefix, join_char_array
    use :: mod_predicates,        only: is_allowed_option
    use :: mod_program_limits,    only: MAX_MSG_LEN
    implicit none
    


contains
    function option_to_code(option, allowed_options) result(code)
        character(len=*), intent(in) :: option
        character(len=*), intent(in) :: allowed_options(:)
        integer(i64) :: code
        integer(i64) :: i

        code = -1_i64
        do i = 1, size(allowed_options)
            if (trim(option) == trim(allowed_options(i))) then
                code = i
                exit
            end if
        end do
    end function option_to_code

    subroutine check_allowed_option(option, allowed_options, file, class_name, value_name, fatal)
        implicit none
        character(*), intent(in)            :: option
        character(*), intent(in)            :: allowed_options(:)
        character(*), intent(in), optional  :: file, class_name, value_name

        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, opts_str, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        if (.not. is_allowed_option(option, allowed_options)) then
            prefix   = make_prefix(file, class_name, value_name, sep=" = ")
            opts_str = join_char_array(allowed_options, sep=", ")
            write(tmp_msg,'(a," must be one of {",a,"}, but got """,a,""".")') &
                 trim(prefix), trim(opts_str), trim(option)
            msg = trim(tmp_msg)
            call raise_error(ERR_DISALLOWED_OPTION, msg, do_fatal)
        end if
    end subroutine check_allowed_option

end module mod_check_allowed_option
