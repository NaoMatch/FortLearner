module mod_check_file_exists
    use :: mod_kinds
    use :: mod_error_codes
    use :: mod_error_manager,     only: raise_error
    use :: iso_fortran_env,       only: error_unit
    use :: mod_character_helpers, only: make_prefix
    use :: mod_program_limits,    only: MAX_MSG_LEN
    implicit none
    private
    public :: check_file_exists

contains

    !> 指定パスが空でなく、既存の「ファイル」であることを確認する。
    !! 存在しない場合や空文字の場合は raise_error を呼び出す。
    subroutine check_file_exists(path, file, class_name, func_name, fatal)
        implicit none
        character(*), intent(in) :: path
        character(*), intent(in) :: file, class_name
        character(*), intent(in) :: func_name
        logical,      intent(in), optional :: fatal

        logical :: do_fatal
        logical :: ex
        character(len=:), allocatable :: prefix
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: path_trim
        character(len=MAX_MSG_LEN)    :: tmp_msg

        ! 1) fatal 既定値
        do_fatal = .true.
        if (present(fatal)) do_fatal = fatal

        ! 2) 入力検証（空文字チェック）
        path_trim = trim(path)
        if (len_trim(path_trim) == 0) then
            prefix = make_prefix(file, class_name, func_name)
            write(tmp_msg, '(a,": path must not be empty.")') prefix
            msg = trim(tmp_msg)
            ! 想定: ERR_INVALID_ARGUMENT が定義済み
            call raise_error(ERR_INVALID_ARGUMENT, msg, do_fatal)
            return
        end if

        ! 3) 存在確認（移植性のため INQUIRE のみ使用）
        inquire(file=path_trim, exist=ex)
        if (.not. ex) then
            prefix = make_prefix(file, class_name, func_name)
            write(tmp_msg, '(a,": file not found: ''",a,"''")') prefix, path_trim
            msg = trim(tmp_msg)
            ! 想定: ERR_FILE_NOT_FOUND が定義済み
            call raise_error(ERR_FILE_NOT_FOUND, msg, do_fatal)
            return
        end if
        ! 4) 正常ケースは何もせず復帰
    end subroutine check_file_exists

end module mod_check_file_exists
