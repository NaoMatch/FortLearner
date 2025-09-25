module mod_check_format_version
    use :: iso_fortran_env,       only: error_unit
    use :: mod_kinds
    use :: mod_error_codes
    use :: mod_error_manager,     only: raise_error
    use :: mod_character_helpers, only: make_prefix
    use :: mod_program_limits,    only: MAX_MSG_LEN
    implicit none
    
contains

    subroutine check_format_version_supported(ver, min_ver, max_ver,  &
                                              file, class_name, value_name, fatal)
        integer(i64), intent(in)            :: ver, min_ver, max_ver
        character(*), intent(in), optional  :: file, class_name, value_name

        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        if (ver < min_ver .or. ver > max_ver) then
            prefix = make_prefix(file, class_name, value_name)
            write(tmp_msg,'(a," must be in [",i0,",",i0,"], but got ",i0,".")') &
                 prefix, min_ver, max_ver, ver
            msg = trim(tmp_msg)
            call raise_error(ERR_INVALID_FORMAT_VERSION, msg, do_fatal)
        end if
    end subroutine

end module mod_check_format_version
