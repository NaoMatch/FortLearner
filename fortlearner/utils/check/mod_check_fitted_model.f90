module mod_check_fitted_model
    use :: mod_kinds
    use :: mod_error_codes
    use :: mod_error_manager,     only: raise_error
    use :: iso_fortran_env,       only: error_unit
    use :: mod_character_helpers, only: make_prefix
    use :: mod_program_limits,    only: MAX_MSG_LEN
    implicit none


contains
    
    subroutine check_fitted_model(is_fitted, file, class_name, func_name, fatal)
        implicit none
        logical,      intent(in) :: is_fitted
        character(*), intent(in) :: file, class_name
        character(*), intent(in) :: func_name

        logical, optional :: fatal
        logical :: do_fatal
        character(:), allocatable :: prefix, msg
        character(len=MAX_MSG_LEN) :: tmp_msg

        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        if (.not. is_fitted) then
            !------------------------------------------
            ! 1. prefix 生成
            prefix = make_prefix(file, class_name, func_name)

            ! 2. エラーメッセージ出力
            write(tmp_msg,'(a," must be fitted first (call fit() before using this API).")') &
                prefix
            msg = trim(tmp_msg)
            call raise_error(ERR_NOT_FITTED, msg, do_fatal)
            !------------------------------------------
        end if
    end subroutine check_fitted_model


end module mod_check_fitted_model
